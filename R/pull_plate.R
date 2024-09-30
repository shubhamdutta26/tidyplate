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
                                  file_full_name)[[2]]

  plate_names <- unlist(lapply(plate_parameters, function(x) x[1,1]),
                        use.names = FALSE)

  list_of_plates <- lapply(plate_parameters, convert_first_row_to_header)


  selected_plates <- extract_plates(list = list_of_plates, indices = plate_id)

  return(plate_names)

  # if (typeof(plate_id) %in% c("numeric", "double") & max(plate_id) > length(list_of_plates)) {
  #   stop("`plate_id` value(s) are greater than the total number of plates in the input file.", call. = FALSE)
  # } else if (typeof(plate_id) == "character") {
  #   index <- sort(match(plate_id, each_plate_name))
  #   return(list_of_plates[index] |>
  #            purrr::map(function(x) janitor::row_to_names(x,row_number = 1)) |>
  #            purrr::map(function(x) utils::type.convert(x, as.is = TRUE)))
  # } else if (typeof(plate_id) %in% c("numeric", "double")) {
  #   return(list_of_plates[plate_id] |>
  #            purrr::map(function(x) janitor::row_to_names(x,row_number = 1)) |>
  #            purrr::map(function(x) utils::type.convert(x, as.is = TRUE)))
  # }

}
