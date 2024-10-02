#' Subset individual plates from the input file
#'
#' @param file This is the path to a xlsx or csv file containing data for the
#' following types of plates: 6, 12, 24, 48, 96, 384, and 1536.
#' @param sheet If file type is xlsx this is the sheet name (character) or
#' number (integer).
#' @param plate_id Character or numeric vector that will be used to subset the file.
#'
#' @return a list of tibbles
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "example_12_well.xlsx", package = "tidyplate")
#'
#' n_id = c(1, 3)
#' c_id = c("drug", "percent_survived")
#'
#' data_n <- pull_plate(file = file_path, plate_id = n_id)
#' data_c <- pull_plate(file = file_path, plate_id = c_id)
#'
#' print(data_n)
#' print(data_c)
pull_plate <- function(file, sheet = 1, plate_id) {

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

  # Check if `plate_id` is either a character vector or an integer vector
  if (!(is.character(plate_id) || (is.numeric(plate_id) && all(plate_id == as.integer(plate_id))))) {
    stop("`plate_id` must be either a character or an integer vector.", call. = FALSE)
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
                                  file_full_name)[[2]]

  plate_names <- unlist(lapply(plate_parameters, function(x) x[1,1]),
                        use.names = FALSE)

  # Check if `plate_id` is a character vector or integer vector
  if (is.character(plate_id)) {
    # Check if `plate_id` matches plate names
    if (!all(plate_id %in% plate_names)) {
      warning(paste0("`plate_id` does not match any plate names in ", file_full_name, "."),
              call. = FALSE)
    }
  }

  list_of_plates <- lapply(plate_parameters, convert_first_row_to_header)

  # Assign names to plates
  names(list_of_plates) <- plate_names

  selected_plate <- list_of_plates[plate_id]

  return(selected_plate)
}
