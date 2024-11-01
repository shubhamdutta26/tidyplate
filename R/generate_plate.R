#' Generates a microwell plate shaped csv or excel files from tibble or dataframe.
#'
#' @param x A dataframe or tibble.
#' @param well_id A character string or an integer which points to the column
#' containing the well ids.
#' @param plate_type A specific integer (6, 12, 24, 48, 96, 384, 1536)
#' indicating the type of microwell plate.
#' @param file_out A character string with the name (with path) of the output
#' file name.
#' @param file_type A character string with the type of exported file ("csv",
#' "xlsx"). The default is "csv".
#'
#' @return A csv or xlsx file.
#'
#' @examples
generate_plate <- function(x,
                           well_id,
                           plate_type,
                           file_out,
                           file_type = c("csv", "xlsx")) {

  # Check that x is a tibble/data.frame
  if (!is.data.frame(x)) {
    rlang::abort("`x` must be a tibble or data frame.", call = NULL)
  }

  if (!is.character(well_id) || !is.integer(well_id)) {
    rlang::abort("`well_id` must be a character string or an integer which points to the
    column containing the well ids (A01, A02, ...).", call = NULL)
  }

  x_colnames <- names(x)

  if (is.character(well_id) & !any(well_id %in% x_colnames)) {
    rlang::abort("`well_id` must be a character string or an integer which points to the
    column containing the well ids (A01, A02, ...).", call = NULL)
  }

  if (!is.integer(plate_type) ||
      !any(plate_type %in% c(6, 12, 24, 48, 96, 384, 1536))) {
    rlang::abort("`plate_type` must be an integer indicating one of the type of
    the following microwell plates: 6, 12, 24, 48, 96, 384, 1536.",
         call = NULL)
  }

  file_type <- rlang::arg_match(file_type)

  # Extract well and other data columns
  well_col <- x[[well_id]]
  plate_cols <- x[ , !(x_colnames %in% well_id)]

  # Get the number of columns for the plate based on plate type
  plate_dims <- switch(as.character(plate_type),
                       "6" = c(2, 3),
                       "12" = c(3, 4),
                       "24" = c(4, 6),
                       "48" = c(6, 8),
                       "96" = c(8, 12),
                       "384" = c(16, 24),
                       "1536" = c(32, 48),
                       rlang::abort("Unsupported plate type"), call = NULL)

  num_rows <- plate_dims[1]
  num_cols <- plate_dims[2]

  # Initialize a list to store plate data for each plate
  all_plate_data <- list()

  # Iterate over each plate column and construct the plate format
  for (i in seq_along(plate_cols)) {
    plate_id <- names(plate_cols)[i]
    values <- plate_cols[[i]]

    # Create an empty data frame for the current plate
    # Adding +1 to rows and columns to make space for row/column headers
    plate_data <- data.frame(matrix(NA, nrow = num_rows + 1, ncol = num_cols + 1))

    # Fill the top-left corner with "Plate ID"
    plate_data[1, 1] <- plate_id

    # Fill the first row with column identifiers (1 to num_cols)
    plate_data[1, 2:(num_cols + 1)] <- as.character(1:num_cols)

    # Fill the first column with row identifiers (A to num_rows)
    plate_data[2:(num_rows + 1), 1] <- LETTERS[1:num_rows]

    # Split wells into row and column indices
    well_row <- substr(well_col, 1, 1)
    well_col_num <- as.numeric(substr(well_col, 2, nchar(well_col)))

    # Place values in the correct positions in the plate data
    for (j in seq_along(well_row)) {
      row_idx <- match(well_row[j], LETTERS[1:num_rows]) + 1
      col_idx <- well_col_num[j] + 1
      plate_data[row_idx, col_idx] <- values[j]
    }

    # Add the plate data to the list, including a separator row
    all_plate_data[[i]] <- plate_data
  }

  # Combine all plates into a single data frame with a separator row
  combined_data <- do.call(rbind,
                           lapply(all_plate_data, function(x) rbind(x, NA)))

  # Export the combined data to the specified file type
  if (file_type == "csv") {
    # Use `write.table` to remove column names (X1, X2, etc.) in the output
    write.table(combined_data,
                file = paste0(file_out, ".csv"),
                sep = ",",
                col.names = FALSE,
                row.names = FALSE,
                na = "")
  } else if (file_type == "xlsx") {
    # Write Excel without row/column names using openxlsx
    openxlsx::write.xlsx(combined_data,
                         file = paste0(file_out, ".xlsx"),
                         colNames = FALSE,
                         rowNames = FALSE)
  } else {
    rlang::abort("Unsupported file type. Use 'csv' or 'xlsx'.", call = NULL)
  }
}
