#' Generates a microwell plate shaped csv or excel file from tibble or dataframe
#'
#' @description
#' Dataframes or tibbles are not good for visual inspection of microwell plate
#' shaped data. `generate_plate()` helps the user by transforming dataframe or
#' tibble into a microwell plate data. It does the opposite of what
#' `tidy_plate()` does to a plate data.
#'
#' @param x A dataframe or tibble.
#' @param well_id A character string or an integer which points to the column
#' containing the well ids.
#' @param plate_type A specific integer (6, 12, 24, 48, 96, 384, 1536)
#' indicating the type of microwell plate.
#' @param file A character string with the filename of the output file with
#' the path and type of exported file. Only csv or xlsx files are supported.
#'
#' @seealso [tidy_plate()],[build_plate()]
#'
#' @return A csv or xlsx file.
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "tidy_12_well.csv", package = "tidyplate")
#'
#' tbl <- read.csv(file_path)
#' temp_file <- tempfile(fileext = ".csv")
#'
#' plate_12 <- generate_plate(tbl,
#'                            well_id = "well",
#'                            plate_type = 12,
#'                            file = temp_file)
generate_plate <- function(x,
                           well_id,
                           plate_type,
                           file) {

  # Convert x to tibble if it's a data frame but not a tibble
  if (is.data.frame(x) && !tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }

  # Check that x is a tibble/data.frame
  if (!is.data.frame(x)) {
    rlang::abort("`x` must be a tibble or data frame.", call = NULL)
  }

  # Validate well_id as either character or integer
  if (!(is.character(well_id) || is.integer(well_id))) {
    rlang::abort("`well_id` must be a character string or an integer indicating the column containing the well ids (A01, A02, ...).", call = NULL)
  }

  # Check if plate_type is valid
  if (!as.character(plate_type) %in% names(plate_dims)) {
    rlang::abort("`plate_type` must be one of 6, 12, 24, 48, 96, 384, or 1536.", call = NULL)
  }

  # Extract column names
  x_colnames <- names(x)

  # Check if well_id column exists in data
  if (is.character(well_id) && !well_id %in% x_colnames) {
    rlang::abort("`well_id` must refer to an existing column in the input file.", call = NULL)
  } else if (is.integer(well_id) && (well_id < 1 || well_id > ncol(x))) {
    rlang::abort("`well_id` integer must refer to a valid column index in the input file.", call = NULL)
  }

  # Extract well column
  well_col <- x[[well_id]]

  # Validate that well_id column has valid well format based on plate_type
  if (plate_type == 384) {
    valid_pattern <- "^[A-P](0[1-9]|[1-5][0-9]|6[0-4])$"  # A01 to P64 for 384-well plates
  } else if (plate_type == 1536) {
    valid_pattern <- "^[A-Z]{1,2}(0[1-9]|[1-9][0-9]|1[0-5][0-9]|1600)$"  # A01 to AF48 for 1536-well plates
  } else {
    valid_pattern <- "^[A-Z](0[1-9]|[1-9][0-9])$"  # For smaller plates like 6, 12, 24, etc.
  }

  # Check for specific well identifiers AA to AF only for 1536 plates
  if (plate_type != 1536 && any(grepl("^(AA|AB|AC|AD|AE|AF)", well_col))) {
    rlang::abort(c("`well_id` column contains wells labeled 'AA' to 'AF'.",
                   "i" = "Is this a 1536 plate data?"),
                 call = NULL)
  }

  # Perform validation
  if (!all(grepl(valid_pattern, well_col))) {
    rlang::abort(c("`well_id` column does not contain valid well identifiers.",
                   "i" = "Valid well identifiers for this plate type are defined by the pattern: A01, B01, etc"),
                 call = NULL)
  }

  # Get the number of rows and columns for the specified plate type
  plate_size <- plate_dims[[as.character(plate_type)]]
  num_rows <- plate_size[1]
  num_cols <- plate_size[2]

  # Generate row labels for 1536-well plates
  row_labels <- if (plate_type == 1536) {
    c(LETTERS, paste0("A", LETTERS[1:6]))  # A to AF
  } else {
    LETTERS[1:num_rows]
  }

  # Determine file extension from `file` argument
  file_ext <- tools::file_ext(file)
  if (!file_ext %in% c("csv", "xlsx")) {
    rlang::abort("`file` extension must be 'csv' or 'xlsx'.", call = NULL)
  }

  # Extract other data columns
  plate_cols <- x[, !(x_colnames %in% well_id)]

  # Initialize a list to store plate data for each plate
  all_plate_data <- list()

  # Iterate over each plate column and construct the plate format
  for (i in seq_along(plate_cols)) {
    plate_id <- names(plate_cols)[i]
    values <- plate_cols[[i]]

    # Create an empty data frame for the current plate with extra row/column for headers
    plate_data <- data.frame(matrix(NA, nrow = num_rows + 1, ncol = num_cols + 1))

    # Fill in headers
    plate_data[1, 1] <- plate_id
    plate_data[1, 2:(num_cols + 1)] <- as.character(1:num_cols)
    plate_data[2:(num_rows + 1), 1] <- row_labels

    # Split wells into row and column indices
    well_row <- substr(well_col, 1, nchar(well_col) - 2)  # Row is first part of well id (e.g., "A" or "AF")
    well_col_num <- as.numeric(substr(well_col, nchar(well_col) - 1, nchar(well_col)))  # Column is last two digits

    # Place values in the correct positions in the plate data
    for (j in seq_along(well_row)) {
      row_idx <- match(well_row[j], row_labels) + 1
      col_idx <- well_col_num[j] + 1
      plate_data[row_idx, col_idx] <- values[j]
    }

    # Add the plate data to the list, including a separator row
    all_plate_data[[i]] <- plate_data
  }

  # Combine all plates into a single data frame with a separator row
  combined_data <- do.call(rbind, lapply(all_plate_data, function(x) rbind(x, NA)))

  # Check if the last row is entirely NA, and remove it if so
  if (all(is.na(combined_data[nrow(combined_data), ]))) {
    combined_data <- combined_data[-nrow(combined_data), ]
  }

  # Export based on file extension
  if (file_ext == "csv") {
    write.table(combined_data, file = file, sep = ",", col.names = FALSE, row.names = FALSE, na = "")
  } else if (file_ext == "xlsx") {
    openxlsx::write.xlsx(combined_data, file = file, colNames = FALSE, rowNames = FALSE)
  }
}
