#' Checks whether the input file can be used to transform to a tidy plate using
#' the `tidy_plate()` function
#'
#' @param file This is the path to a xlsx or csv file containing data for the
#' following types of plates: 6, 12, 24, 48, 96, 384, and 1536. The plate format
#' is described below.
#' @param well_id This is takes a character of length 1 and cannot be the
#' same as individual plate names.
#' @param sheet If file type is xlsx this is the sheet name (character) or
#' number (integer).
#'
#' @return An error or a message saying that input file can be used with the
#' `tidy_plate()` function
#' @export
#'
#' @examples
#' file_path <- system.file(
#'   "extdata",
#'   "example_12_well.xlsx",
#'   package = "tidyplate"
#' )
#'
#' check_plate(file = file_path)
check_plate <- function(file,
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
  file_ext <- tolower(tools::file_ext(file))
  file_full_name <- basename(file)

  ## Check if file exists----
  if (!(file.exists(file))) {
    stop(paste0(file_full_name, " does not exist!"), call. = FALSE)
  }

  # Read data----
  raw_data <- read_data(file = file, file_ext = file_ext, sheet = sheet)

  # Check if input file has the correct format----
  ## Count number of columns and rows in raw_data----
  count_columns <- ncol(raw_data)
  count_rows_actual <- nrow(raw_data)

  plate_parameters <- valid_plate(raw_data,
                                  count_columns ,
                                  count_rows_actual,
                                  well_id,
                                  file_full_name)[[1]]

  message(paste0(file_full_name, ": OK; Plate type: ", plate_parameters[[3]], "-well"))
}
