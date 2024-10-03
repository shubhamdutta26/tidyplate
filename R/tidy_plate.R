#' Reads and transforms microwell plate to a tibble
#'
#' @param file A character string containing the path to a csv or excel file.
#' The format is described below.
#' @param well_id A character string that will be the name for the well id
#' column.
#' @param sheet A character or integer indicating the excel sheet to be
#' read.
#'
#' @return A tibble.
#' @export
#' @examples
#' file_path <- system.file("extdata", "example_12_well.xlsx",
#'   package = "tidyplate"
#' )
#'
#' data_12 <- tidy_plate(file = file_path)
#'
#' head(data_12)
tidy_plate <- function(file,
                       well_id = "well",
                       sheet = 1) {

  # Check whether function arguments are valid----
  ## One file should be provided----
  if (length(file) != 1) {
    stop(
      paste0(
        "Invalid input: ",
        ifelse(length(file) > 1,
               "More than one file provided."
        )
      ),
      call. = FALSE
    )
  }

  ## `well_id` should be a character vector of length 1----
  if (!is.character(well_id) || length(well_id) != 1L) {
    stop("`well_id` should be a character vector of length 1", call. = FALSE)
  }

  ## Read file ext and basename----
  # file_ext <- tolower(tools::file_ext(file))
  file_full_name <- basename(file)

  ## Check if file exists----
  if (!(file.exists(file))) {
    stop(paste0(file_full_name, " does not exist!"), call. = FALSE)
  }

  # Read data----
  raw_data <- read_data(file = file, sheet = sheet)

  # Check if input file has the correct format----
  ## Count number of columns and rows in raw_data----
  count_columns <- ncol(raw_data)
  count_rows_actual <- nrow(raw_data)

  # Check if plate format is valid----
  plate_parameters <- valid_plate(raw_data,
                                  count_columns ,
                                  count_rows_actual,
                                  well_id,
                                  file_full_name)[[1]]

  # Final transformation----

  ## Converting to data.frame----
  raw_data <- as.data.frame(raw_data)
  ## Remove any completely empty rows----
  plate_data <-
    raw_data[rowSums(!is.na(raw_data) & raw_data != "", na.rm = TRUE) > 0, ]
  ## Find the rows that contain the plate identifiers----
  plate_rows <- which(
    plate_data[, 1] != "" & !grepl("^[A-Z]{1,2}$", plate_data[, 1])
  )
  ## Initialize an empty list to store the reformatted data----
  reformatted_data <- vector("list", plate_parameters[[1]])  # Pre-allocate based on the number of plates

  # Process each plate----
  for (i in seq_along(plate_rows)) {
    start_row <- plate_rows[i]
    end_row <- if (i < length(plate_rows)) plate_rows[i + 1] - 1 else nrow(plate_data)

    ## Extract the current plate----
    plate <- plate_data[start_row:end_row, ]

    ## Get the plate identifier----
    plate_id <- as.character(plate[1, 1])

    ## Remove the first row and set column names----
    plate <- plate[-1, ]
    colnames(plate) <- c("row", as.character(1:(ncol(plate) - 1)))

    ## Check if we have valid data----
    if (nrow(plate) > 0) {
      # Create well identifiers and values in one go
      num_cols <- ncol(plate) - 1
      zwellsx <- paste0(rep(plate$row, each = num_cols), sprintf("%02d", rep(1:num_cols, times = nrow(plate))))

      # Flatten the plate values and replace empty strings with NA
      values <- as.vector(t(plate[, -1]))
      values[values == ""] <- NA

      # Create long format data frame directly
      long_plate <- data.frame(zwellx = zwellsx, value = values)

      # Add to the pre-allocated list at the correct index
      reformatted_data[[i]] <- long_plate
    }
  }

  # Set names for the list based on plate identifiers
  names(reformatted_data) <- sapply(plate_rows, function(i) as.character(plate_data[i, 1]))


  # Merge all
  final_data <- reformatted_data[[1]]
  colnames(final_data)[2] <- names(reformatted_data)[1]

  if (length(reformatted_data) > 1) {
    for (i in seq_along(reformatted_data)[-1]) {
      new_data <- reformatted_data[[i]]
      colnames(new_data)[2] <- names(reformatted_data)[i]
      final_data <- merge(final_data, new_data, by = "zwellx", all = TRUE)
    }
  }

  # Sort the dataframe by well
  final_data <- final_data[order(final_data$zwellx), ]
  # Rename the first column
  colnames(final_data)[1] <- well_id

  final_data_no_na <- utils::type.convert(
    final_data[!apply(final_data[-1], 1, \(x) all(is.na(x))), ],
    as.is = TRUE
  )


  message(paste("Data: ", file_full_name,
                "; Plate type: ", plate_parameters[[3]],
                "-well plate", sep = ""))
  return(tibble::tibble(final_data_no_na))
}
