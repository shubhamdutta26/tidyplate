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
check_plate <- function(file, well_id = "well", sheet = 1) {
  # check if one file is provided by the user
  if (typeof(file) %in% c("double", "integer", "logical")) {
    stop(
      paste0("file argument cannot be of type: ", typeof(file)),
      call. = FALSE
    )
  } else if (length(file) > 1) {
    stop(
      paste0("More than one file provided. Only one file should be provided"),
      call. = FALSE
    )
  }

  # Check whether `well_id` is a character and has length == 1
  if (typeof(well_id) != "character") {
    stop("well_id should be a character vector of length 1", call. = FALSE)
  } else if (length(well_id) > 1L) {
    stop("well_id should be a character vector of length 1", call. = FALSE)
  }

  # Extract file name
  file_name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file)
  file_ext <- tools::file_ext(file)
  file_full_name <- paste(file_name, file_ext, sep = ".")

  # Check if file exists
  if (!(file.exists(file))) {
    stop(paste0(file_full_name, ": Not OK; File does not exist."), call. = FALSE)
  }

  # Determine file ext and call appropriate function to import as raw_data
  if (!(file_ext %in% c("xlsx", "csv"))) {
    stop(paste0(file_full_name, ": Not OK; Must be either xlsx or csv file."), call. = FALSE)
  } else if (tools::file_ext(file) == "xlsx") {
    suppressMessages(raw_data <- readxl::read_excel(file, col_names = FALSE, sheet = sheet))
  } else {
    suppressMessages(raw_data <- readr::read_csv(file, col_names = FALSE))
  }

  # Check if file is empty
  # xlsx files cannot be empty
  if (nrow(raw_data) == 0) {
    stop(paste0(file_full_name, ": Not OK; File is empty."), call. = FALSE)
  }

  # Count number of columns and rows in raw_data
  count_columns <- ncol(raw_data)
  count_rows_actual <- nrow(raw_data)

  # Check if there are a exact number of columns
  if (!(count_columns %in% c(4L, 5L, 7L, 9L, 13L, 25L, 49L))) {
    stop(paste0(file_full_name, ": Not OK; Verify input data format."), call. = FALSE)
  }

  # This counts the number of plates in the dataset
  # This counts the theoretical number of rows the raw_data should have for each plate type
  # This determines the plate type
  # Initialize indexing for each plate size
  # Sets increment value for counter for each well type
  row_start <- 1L
  plate_parameters <- plate_params(raw_data, count_columns)

  # This checks whether the input formating is correct or not.
  # There must be one empty row between each data.
  if (plate_parameters[[2]] != count_rows_actual) {
    stop(paste0(file_full_name, ": Not OK; Verify input data format."),
      call. = FALSE
    )
  }

  # Initialize empty list to store the raw_data.
  # Each element in the list will store one plate from the raw_data
  # no_of_plates <- as.integer(plate_parameters[[1]])
  list_of_plates <- vector(mode = "list", length = plate_parameters[[1]])

  # This is a vector which will be used to name each plate
  # name_of_plates <- paste0("plate", 1:plate_parameters[[1]])

  # Initialize counter for indexing each plate from raw_data
  counter <- 0

  # Store each plate as an obect in a list
  for (i in 1:plate_parameters[[1]]) {
    list_of_plates[[i]] <- raw_data[(row_start + counter):(plate_parameters[[5]] + counter), ]
    counter <- counter + plate_parameters[[6]]
  }

  # Make the list a named list
  # names(list_of_plates) <- name_of_plates

  # Extract names for each plate
  each_plate_name <- purrr::map(list_of_plates, function(x) {
    x |>
      dplyr::slice(1) |>
      dplyr::select(1)
  }) |>
    unlist(use.names = FALSE)

  # Check if the plate name matches the user input `well_id`
  if (well_id %in% each_plate_name) {
    stop(paste0(file_full_name, ": Not OK; Plate names cannot be the same as variable 'well_id'"),
      call. = FALSE
    )
  }

  # Check if plate name exists and unique
  if (sum(is.na(each_plate_name)) != 0L || length(each_plate_name) != length(unique(each_plate_name))) {
    stop(paste0(file_full_name, ": Not OK; Verify each plate has a unique name."), call. = FALSE)
  }

  # Check if plates have 1:x as column names after plate name
  first_row <- purrr::map(list_of_plates, function(x) {
    x |>
      dplyr::slice(1) |>
      dplyr::select(-1)
  }) |>
    purrr::map(function(x) sum(x == plate_parameters[[8]])) |>
    purrr::map(function(x) x != (count_columns - 1)) |>
    unlist(use.names = FALSE)
  first_row_sum <- replace(first_row, is.na(first_row), TRUE) |>
    sum() # has to be 0; otherwise there is at least one plate that has bad column names

  # Check if plates have A:x as row names after plate name
  first_col <- purrr::map(list_of_plates, function(x) x |> dplyr::select(1)) |>
    purrr::map(function(x) {
      x |>
        dplyr::filter(dplyr::row_number() != 1) |>
        t() |>
        toupper()
    }) |>
    purrr::map(function(x) sum(x != plate_parameters[[7]])) |>
    unlist(use.names = FALSE)
  first_col_sum <- replace(first_col, is.na(first_col), TRUE) |>
    sum() # has to be 0; otherwise there is at least one plate that has bad row names

  if (first_row_sum != 0L & first_col_sum != 0L) {
    stop(paste0(file_full_name, ": Not OK; Verify row and column names."), call. = FALSE)
  } else if (first_row_sum != 0L) {
    stop(paste0(file_full_name, ": Not OK; Verify column names."), call. = FALSE)
  } else if (first_col_sum != 0L) {
    stop(paste0(file_full_name, ": Not OK; Verify row names."), call. = FALSE)
  }

  message(paste0(file_full_name, ": OK; Plate type: ", plate_parameters[[3]], " well"))
}
