#' Transforms a plate to a tidy dataframe
#'
#' @param file This is the path to a xlsx or csv file containing data for the
#' following types of plates: 6, 12, 24, 48, 96, 384, and 1536. The plate format
#' is described below.
#' @param sheet If file type is xlsx this is the sheet name (character) or
#' number (integer).
#'
#' @return A tidy dataframe
#' @export
#' @examples
#' file_path <- system.file("extdata", "example_12_well.xlsx", package = "tidyplate")
#'
#' data_12 <- tidy_plate(
#'    file = file_path
#'    )
#'
#' head(data_12)
tidy_plate <- function(file, sheet=1) {

  # Extract file name
  file_name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file)
  file_ext <- tools::file_ext(file)
  file_full_name <- paste(file_name, file_ext, sep=".")

  # Check if file exists
  if (!(file.exists(file))) {
    stop(paste0(file_full_name, " does not exist!"), call. = FALSE)
  }

  # check if one file is provided by the user
  if (length(file) > 1) {
    stop(
      paste0("More than one file provided. Only one file should be provided"),
      call. = FALSE)
  }

  # Determine file ext and call appropriate function to import as raw_data
  if (!(file_ext %in% c("xlsx", "csv"))) {
    stop(paste0(file_full_name, " must be either xlsx or csv file!"), call. = F)
  } else if (tools::file_ext(file) == "xlsx") {
    suppressMessages(raw_data <- readxl::read_excel(file, col_names = F, sheet = sheet))
  } else {
    suppressMessages(raw_data <- readr::read_csv(file, col_names = F))
  }

  # Check if file is empty
  if(nrow(raw_data) == 0){
    stop(paste0(file_full_name, " is empty. Please verify input file."), call. = FALSE)
  }

  # Count number of columns and rows in raw_data
  count_columns = ncol(raw_data)
  count_rows_actual = nrow(raw_data)

  # Check if there are a exact number of columns
  if (!(count_columns %in% c(4L, 5L, 7L, 9L, 13L, 25L, 49L))) {
    stop(paste0(file_full_name, " is not a valid plate. Plate size must be one of the
following: 6, 12, 24, 48, 96, 384, and 1536. Please review an example dataset."), call. = FALSE)
  }

  # This counts the number of plates in the dataset
  # This counts the theoretical number of rows the raw_data should have for each plate type
  # This determines the plate type
  # Initialize indexing for each plate size
  # Sets increment value for counter for each well type
  row_start <- 1L
  if (count_columns == 4L) {
    no_of_plates <- sum(rowSums(is.na(raw_data)) == 4L) + 1L
    count_rows_theoretical <- (no_of_plates * 4L) - 1L
    plate_type <- 6L
    cols <- 1L:4L
    row_end <- 3L
    increment <- 4L
    first_col_vec <- LETTERS[1:2]
    first_row_vec <- 1L:3L
  } else if (count_columns == 5L) {
    no_of_plates <- sum(rowSums(is.na(raw_data)) == 5L) + 1L
    count_rows_theoretical <- (no_of_plates * 5L) - 1L
    plate_type <- 12L
    cols <- 1L:5L
    row_end <- 4L
    increment <- 5L
    first_col_vec <- LETTERS[1:3]
    first_row_vec <- 1L:4L
  } else if (count_columns == 7L) {
    no_of_plates <- sum(rowSums(is.na(raw_data)) == 7L) + 1L
    count_rows_theoretical <- (no_of_plates * 6L) - 1L
    plate_type <- 24L
    cols <- 1L:7L
    row_end <- 5L
    increment <- 6L
    first_col_vec <- LETTERS[1:4]
    first_row_vec <- 1L:6L
  } else if (count_columns == 9L) {
    no_of_plates <- sum(rowSums(is.na(raw_data)) == 9L) + 1L
    count_rows_theoretical <- (no_of_plates * 8L) - 1L
    plate_type <- 48L
    cols <- 1L:9L
    row_end <- 7L
    increment <- 8L
    first_col_vec <- LETTERS[1:6]
    first_row_vec <- 1L:8L
  } else if (count_columns == 13L) {
    no_of_plates <- sum(rowSums(is.na(raw_data)) == 13L) + 1L
    count_rows_theoretical <- (no_of_plates * 10L) - 1L
    plate_type <- 96L
    cols <- 1L:13L
    row_end <- 9L
    increment <- 10L
    first_col_vec <- LETTERS[1:8]
    first_row_vec <- 1:12
  } else if (count_columns == 25L) {
    no_of_plates <- sum(rowSums(is.na(raw_data)) == 25L) + 1L
    count_rows_theoretical <- (no_of_plates * 18L) - 1L
    plate_type <- 384L
    cols <- 1L:25L
    row_end <- 17L
    increment <- 18L
    first_col_vec <- LETTERS[1:16]
    first_row_vec <- 1L:24L
  } else if (count_columns == 49L) {
    no_of_plates <- sum(rowSums(is.na(raw_data)) == 49L) + 1L
    count_rows_theoretical <- (no_of_plates * 34L) - 1L
    plate_type <- 1536L
    cols <- 1L:49L
    row_end <- 33L
    increment <- 34L
    first_col_vec <- c(LETTERS[1:26], "AA", "AB", "AC", "AD", "AE", "AF")
    first_row_vec <- 1L:48L
  }

  # This checks whether the input formating is correct or not.
  # There must be one empty row between each data.
  if (count_rows_theoretical != count_rows_actual) {
    stop(paste0(file_full_name, " is not a valid input file. Please review an example dataset."),
         call. = F)
  }

  # Initialize empty list to store the raw_data.
  # Each element in the list will store one plate from the raw_data
  no_of_plates <- as.integer(no_of_plates)
  list_of_plates <- vector(mode = "list", length = no_of_plates)

  # This is a vector which will be used to name each plate
  name_of_plates <- paste0("plate", 1:no_of_plates)

  # Initialize counter for indexing each plate from raw_data
  counter = 0

  # Store each plate as an obect in a list
  for (i in 1:no_of_plates) {
    list_of_plates[[i]] <- raw_data[(row_start+counter):(row_end+counter), ]
    counter = counter + increment
  }

  # Make the list a named list
  names(list_of_plates) <- name_of_plates

  # Extract names for each plate
  each_plate_name <- purrr::map(list_of_plates, function(x) x |> dplyr::slice(1) |> dplyr::select(1)) |>
    unlist(use.names = F)

  # Check if plate name exists and unique
  if (sum(is.na(each_plate_name)) != 0L) {
    stop(paste0("Verify that each plate in ",file_full_name, " a name."), call. = F)
  } else if (length(each_plate_name) != length(unique(each_plate_name))) {
    stop(paste0("Verify that each plate name in ",file_full_name, " is unique."), call. = F)
  }

  # Check if plates have 1:x as column names after plate name
  first_row <- purrr::map(list_of_plates, function(x) x |> dplyr::slice(1) |> dplyr::select(-1)) |>
    purrr::map(function(x) sum(x == first_row_vec)) |>
    purrr::map(function(x) x != (count_columns-1)) |>
    unlist(use.names = F)
  first_row_sum <- replace(first_row, is.na(first_row), TRUE) |>
    sum() # has to be 0; otherwise there is at least one plate that has bad column names

  # Check if plates have A:x as row names after plate name
  first_col <- purrr::map(list_of_plates, function(x) x |> dplyr::select(1)) |>
    purrr::map(function(x) x |> dplyr::filter(dplyr::row_number() != 1) |> t() |> toupper()) |>
    purrr::map(function(x) sum(x != first_col_vec)) |>
    unlist(use.names = F)
  first_col_sum <- replace(first_col, is.na(first_col), TRUE) |>
    sum()  # has to be 0; otherwise there is at least one plate that has bad row names

  if (first_row_sum != 0L & first_col_sum != 0L) {
    stop(paste0("Verify row names and column names in ", file_full_name, "."),
         call. = FALSE)
  }

  if (first_row_sum != 0L) {
    stop(paste0("Verify column names in ", file_full_name, "."),
         call. = FALSE)
  }

  if (first_col_sum != 0L) {
    stop(paste0("Verify row names in ", file_full_name, "."),
         call. = FALSE)
  }

  # Run a loop to extract each plate and store it in list_of_plates
  final_data_list <-list_of_plates |>
    purrr::map(function(x) janitor::row_to_names(x, row_number = 1) |>
                 tidyr::pivot_longer(-1, names_to = "name", values_to = "value",
                                     values_transform = list(value = as.character))
    )

  # Prep final_data as a dataframe/tibble
  # Remove NAs in all rows (except well/ first column)
  final_data <- final_data_list |>
    purrr::map(naming_cols) |>
    purrr::reduce(dplyr::full_join, by = "well") |>
    dplyr::filter(!dplyr::if_all(-"well", is.na)) |>
    utils::type.convert(as.is = TRUE)

  # Return data with plate type
  message(paste("Data: ", file_full_name, "\nPlate type: ", plate_type, " well plate", sep = ""))
  return(final_data)
}
