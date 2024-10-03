#' Returns the name of each plate in the file
#'
#' @param file This is the path to a xlsx or csv file containing data for the
#' following types of plates: 6, 12, 24, 48, 96, 384, and 1536.
#' @param sheet If file type is xlsx this is the sheet name (character) or
#' number (integer).
#'
#' @return A character vector
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "example_12_well.xlsx", package = "tidyplate")
#'
#' data_12 <- view_plate_names(file = file_path)
#'
#' data_12
view_plate_names <- function(file, sheet = 1){

  # Set default well_id value
  well_id = "well"

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

  ## Read file ext and basename----
  # file_ext <- tolower(tools::file_ext(file))
  file_full_name <- basename(file)

  ## Check if file exists----
  if (!(file.exists(file))) {
    stop(paste0(file_full_name, " does not exist!"), call. = FALSE)
  }

  # Read data----
  raw_data <- read_data(file = file, sheet = sheet)

  # Count number of columns and rows in raw_data
  count_columns <- ncol(raw_data)
  count_rows_actual <- nrow(raw_data)

  # Check if plate format is valid----
  plate_parameters <- valid_plate(raw_data,
                                  count_columns ,
                                  count_rows_actual,
                                  well_id,
                                  file_full_name)[[2]]

  plate_names <- unlist(lapply(plate_parameters, function(x) x[1,1]),
                        use.names = FALSE)

  list_of_plates <- lapply(plate_parameters, convert_first_row_to_header)


  selected_plates <- extract_plates(list = list_of_plates)

  return(plate_names)
}
