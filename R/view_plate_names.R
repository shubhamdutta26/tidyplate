#' Returns the name of each plate in the file
#'
#' @description
#' `view_plate_names()` returns the names of all plates in the input file as a
#' character vector. In case of empty or duplicates it diplays a warning and
#' then returns the plate names.
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
view_plate_names <- function(file, sheet = 1) {
  # Set default well_id value
  well_id = "well"

  # Check whether function arguments are valid----
  ## One file should be provided----
  if (length(file) != 1) {
    rlang::abort(
      paste0(
        "Invalid input: ",
        ifelse(length(file) > 1,
               "More than one file provided."
        )
      ),
      call = NULL
    )
  }

  ## Check if file exists----
  if (!(file.exists(file))) {
    rlang::abort("File does not exist!", call = NULL)
  }

  ## Read file ext and basename----
  file_full_name <- basename(file)

  # Read data----
  raw_data <- read_data(file = file, sheet = sheet)

  # Count number of columns and rows in raw_data
  count_columns <- ncol(raw_data)
  count_rows_actual <- nrow(raw_data)

  ## Check if there are an exact number of columns----
  if (!(count_columns %in% c(4L, 5L, 7L, 9L, 13L, 25L, 49L))) {
    rlang::abort(
      c(
        paste0(file_full_name, " is not a valid input file."),
        "i" = "Only 6, 12, 24, 48, 96, 384, and 1536-well plate formats are accepted.",
        "i" = "Use the `build_plate()` function to build an empty template."
      ), call = NULL)
  }

  ## Count the number of plates in the dataset----
  row_start <- 1L
  plate_parameters <- plate_params(raw_data, count_columns)

  ## Check whether the input formatting is correct or not----
  if (plate_parameters[[2]] != count_rows_actual) {
    rlang::abort(
      c(
        paste0(file_full_name, " is not a valid input file."),
        "i" = "Only 6, 12, 24, 48, 96, 384, and 1536-well plate formats are accepted.",
        "i" = "Use the `build_plate()` function to build an empty template."
      ), call = NULL)
  }

  ## Store each plate in a list
  list_of_plates <- lapply(1:plate_parameters[[1]], function(i) {
    start_idx <- row_start + (i - 1) * plate_parameters[[6]]
    end_idx <- start_idx + plate_parameters[[5]]
    raw_data[start_idx:(end_idx - 1), ]
  })

  ## Efficiently extract names for each plate
  each_plate_name <- unlist(
    lapply(list_of_plates, function(x) x[1, 1]),
    use.names = FALSE
  )

  # Treat empty strings ("") or spaces (" ") as missing values
  is_empty <- function(x) {
    is.na(x) || x == "" || x == " "
  }

  # Check for empty plate names (NA, "", or " ")
  empty_plates <- which(sapply(each_plate_name, is_empty))

  # Check for duplicate plate names, excluding empty ones
  non_empty_names <- each_plate_name[!sapply(each_plate_name, is_empty)]
  dup_plates_names <- which(duplicated(non_empty_names) | duplicated(non_empty_names, fromLast = TRUE))

  # Combined check: both empty and duplicate names
  if (length(empty_plates) > 0 && length(dup_plates_names) > 0) {
    dup_names <- unique(non_empty_names[dup_plates_names])
    rlang::abort(
      c(
        paste0("Empty (blank or NA) & duplicate plate names found in ", file_full_name, ":"),
        "i" = paste0("Empty position(s): ", paste(empty_plates, collapse = ", ")),
        "i" = paste0("Duplicated name(s): ", paste(dup_names, collapse = ", ")),
        "i" = paste0("Duplicated position(s): ", paste(dup_plates_names, collapse = ", ")),
        "x" = "No empty or duplicate plate names are allowed."
      ),
      call = NULL
    )
  }

  # Separate checks and messages for empty and duplicate names
  if (length(empty_plates) > 0) {
    rlang::inform(c(
      paste0("Empty (blank or NA) plate name(s) found in ", file_full_name, ":"),
      "i" = paste0("Position(s): ", paste(empty_plates, collapse = ", ")),
      "x" = "Empty plate name(s) are not allowed."
    ), call = NULL)
  }

  if (length(dup_plates_names) > 0) {
    dup_names <- unique(non_empty_names[dup_plates_names])

    rlang::inform(c(
      paste0("Duplicated plate name(s) found in ", file_full_name, ":"),
      "i" = paste0("Duplicated name(s): ", paste(dup_names, collapse = ", ")),
      "i" = paste0("Position(s): ", paste(dup_plates_names, collapse = ", ")),
      "x" = "No duplicate name(s) are allowed."
    ), call = NULL)
  }

  return(each_plate_name)
}
