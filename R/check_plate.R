#' Checks whether the input file is valid for use with the `tidy_plate()`
#' function
#'
#' @param file A character string containing the path to a csv or excel file.
#' @param well_id A character string that will be the name for the well id
#' column.
#' @param sheet If file type is xlsx this is the sheet name (character) or
#' number (integer).
#'
#' @return An message indicating whether the input file is compatible with the
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

  plate_parameters <- valid_plate(raw_data,
                                  count_columns ,
                                  count_rows_actual,
                                  well_id,
                                  file_full_name)[[1]]

  message(paste0(file_full_name, ": OK; Plate type: ", plate_parameters[[3]], "-well"))
}
