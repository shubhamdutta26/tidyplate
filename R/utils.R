# Check if file exists
file_exists_check <- function(file) {
  if (!(file.exists(file))) {
    stop("File does not exist!", call. = FALSE)
  }
}

# check if one file is provided by the user
check_if_one_file <- function(file) {
  if (length(file) > 1) {
    stop(
      paste0("More than one file provided. Only one file should be provided"),
      call. = FALSE)
  }
}

# Determine if file is empty
check_that_data_is_empty <- function(df) {
  if(nrow(df) == 0){
    stop(paste0("This data is empty after import. Please verify input file."), call. = FALSE)
  }
}

# Determine file ext and call appropriate function to import
import_data <- function(file, sheet) {
  if (!(tools::file_ext(file) %in% c("xlsx", "csv"))) {
    stop("Input file must be either xlsx or csv format!")
  } else if (tools::file_ext(file) == "xlsx") {
    suppressMessages(raw_data <- readxl::read_excel(file, col_names = FALSE, sheet = sheet))
  } else {
    suppressMessages(raw_data <- readr::read_csv(file, col_names = FALSE))
  }
}

# Check if there are a exact number of columns
check_if_valid_n_cols <- function(n_cols) {
  if (!(n_cols %in% c(4L, 5L, 7L, 9L, 13L, 25L, 49L))) {
    stop("This is not a valid plate. Plate size must be one of the following: 6, 12, 24, 48, 96, 384, and 1536. Please review an example dataset.")
  }
}

# This function names each column the right way in the final dataset
naming_cols <- function(df) {
  nm <- colnames(df)[1]
  dplyr::mutate(df, well = paste0(df[[1]], df[[2]])) |>
    dplyr::rename(old_first = 1, !!nm := 3) |>
    dplyr::select(4, 3)
}
