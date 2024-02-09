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

  # Extract file name
  # file_name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file)
  file_ext <- tools::file_ext(file)
  file_full_name <- basename(file)

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
    suppressMessages(raw_data <- readr::read_csv(file, col_names = FALSE, show_col_types = FALSE))
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

  if (typeof(plate_id) %in% c("numeric", "double") & max(plate_id) > length(list_of_plates)) {
    stop("`plate_id` value(s) are greater than the total number of plates in the input file.", call. = FALSE)
  } else if (typeof(plate_id) == "character") {
    index <- sort(match(plate_id, each_plate_name))
    return(list_of_plates[index] |>
             purrr::map(function(x) janitor::row_to_names(x,row_number = 1)) |>
             purrr::map(function(x) utils::type.convert(x, as.is = TRUE)))
  } else if (typeof(plate_id) %in% c("numeric", "double")) {
    return(list_of_plates[plate_id] |>
             purrr::map(function(x) janitor::row_to_names(x,row_number = 1)) |>
             purrr::map(function(x) utils::type.convert(x, as.is = TRUE)))
  }

}
