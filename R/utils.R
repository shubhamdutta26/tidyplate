# Function to read data based on file extension
read_data <- function(file, sheet = NULL) {

  # Get file ext
  file_ext <- tolower(tools::file_ext(file))
  full_name <- basename(file)

  # Check for supported file extensions
  if (file_ext == "csv") {
    raw_data <-
      suppressWarnings(
        tibble::as_tibble(data.table::fread(file, sep = ",", header = FALSE, na.strings = ""))
      )
  } else if (file_ext %in% c("xls", "xlsx")) {
    raw_data <-
      suppressMessages(
        readxl::read_excel(file, sheet = sheet, col_names = FALSE)
      )
  } else {
    rlang::abort(c(
      "Unsupported file format. Please use CSV or Excel files.",
      "i" = "Expected: csv, xls, xlsx",
      "x" = paste0("Found: ", ifelse(file_ext=="", NA, file_ext))), call = NULL)
  }

  # Validate that the data is not empty
  if (nrow(raw_data) == 0 || ncol(raw_data) == 0) {
    rlang::abort(c(
      paste0("Cannot read ", full_name, ":"),
      "i" = paste0("The input ", ifelse(file_ext == "csv", "file", "sheet"), " is empty.")
      ),
      call = NULL)
  }

  return(raw_data)
}

plate_params <- function(data, n_cols) {
  # Define a list of parameters for each n_cols value
  plate_info <- list(
    `4` = list(plate_type = 6L, row_end = 3L, increment = 4L, first_col_vec = LETTERS[1:2], first_row_vec = 1:3),
    `5` = list(plate_type = 12L, row_end = 4L, increment = 5L, first_col_vec = LETTERS[1:3], first_row_vec = 1:4),
    `7` = list(plate_type = 24L, row_end = 5L, increment = 6L, first_col_vec = LETTERS[1:4], first_row_vec = 1:6),
    `9` = list(plate_type = 48L, row_end = 7L, increment = 8L, first_col_vec = LETTERS[1:6], first_row_vec = 1:8),
    `13` = list(plate_type = 96L, row_end = 9L, increment = 10L, first_col_vec = LETTERS[1:8], first_row_vec = 1:12),
    `25` = list(plate_type = 384L, row_end = 17L, increment = 18L, first_col_vec = LETTERS[1:16], first_row_vec = 1:24),
    `49` = list(plate_type = 1536L, row_end = 33L, increment = 34L,
                first_col_vec = c(LETTERS[1:26], paste0("A", LETTERS[1:6])),
                first_row_vec = 1:48)
  )

  # Get the relevant parameters
  params <- plate_info[[as.character(n_cols)]]

  # Calculate no_of_plates and count_rows_theoretical
  no_of_plates <- sum(rowSums(is.na(data)) == n_cols) + 1L
  count_rows_theoretical <- (no_of_plates * params$increment) - 1L

  # Create a vector of columns
  cols <- seq_len(n_cols)

  return(list(
    no_of_plates,
    count_rows_theoretical,
    params$plate_type,
    cols,
    params$row_end,
    params$increment,
    params$first_col_vec,
    params$first_row_vec
  ))
}

# Check if plate(s) are empty
is_list_of_dfs_empty <- function(df_list) {
  sapply(df_list, function(df) {
    if (nrow(df) > 1 && ncol(df) > 1) {
      # Subset the dataframe from second row and second column to the last
      df_subset <- df[2:nrow(df), 2:ncol(df)]
      all(is.na(df_subset) | df_subset == "")
    } else {
      # If the dataframe doesn't have at least 2 rows and 2 columns, return FALSE
      FALSE
    }
  })
}

valid_plate <- function(raw_data,
                        count_columns,
                        count_rows_actual,
                        well_id,
                        file_full_name) {

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

  ## Check if the plate name matches the user input `well_id`
  if (well_id %in% each_plate_name) {
    rlang::abort(
      c("`well_id` value is invalid.",
        "x" = "`well_id` value cannot be the same as the plate names."), call = NULL)
  }

  # Treat empty strings ("") or spaces (" ") as missing values
  is_empty <- function(x) {
    is.na(x) || x == "" || x == " "
  }

  # Check for empty plate names (NA, "", or " ")
  empty_plates <- which(sapply(each_plate_name, is_empty))
  if (length(empty_plates) > 0) {
    rlang::abort(c(
      paste0("Empty (blank or NA) plate name(s) found in ", file_full_name, ":"),
      "i" = paste0("Position(s): ", paste(empty_plates, collapse = ", ")),
      "x" = "Plate name(s) cannot be empty or duplicated."
    ), call = NULL)
  }

  # Check for duplicate plate names, excluding empty ones
  non_empty_names <- each_plate_name[!sapply(each_plate_name, is_empty)]
  dup_plates_names <- which(duplicated(non_empty_names) | duplicated(non_empty_names, fromLast = TRUE))

  if (length(dup_plates_names) > 0) {
    dup_names <- unique(non_empty_names[dup_plates_names])

    rlang::abort(c(
      paste0("Duplicated plate name(s) found in ", file_full_name, ":"),
      "i" = paste0("Duplicated name(s): ", paste(dup_names, collapse = ", ")),
      "i" = paste0("Position(s): ", paste(dup_plates_names, collapse = ", ")),
      "x" = "Plate name(s) cannot be empty or duplicated."
    ), call = NULL)
  }


  if (sum(is_list_of_dfs_empty(list_of_plates)) == plate_parameters[[1]]) {
    rlang::abort("Plate(s) are empty.")
  }

  ## Check if plates have 1:x as column names after plate name
  first_row <- unlist(
    lapply(list_of_plates, function(x) x[1, 2:ncol(x)]),
    use.names = FALSE
  )

  ## Check if plates have A:x as row names after plate name
  first_col <- unlist(
    lapply(list_of_plates, function(x) x[2:nrow(x), 1]),
    use.names = FALSE
  )

  ## Check row ids and column ids are ok
  expected_row <- as.character(rep(plate_parameters[[8]], plate_parameters[[1]]))
  expected_col <- rep(plate_parameters[[7]], plate_parameters[[1]])

  ## Initialize flags for errors
  row_check <- identical(expected_row, first_row)
  col_check <- identical(expected_col, first_col)

  ## Check for both errors
  if (!row_check && !col_check) {
    rlang::abort(
      c(
        paste0("Verify row and column ids in ", file_full_name, "."),
        "i" = "Expected column ids: 1, 2, 3, and so on.",
        "i" = "Expected row ids: A, B, C, and so on.",
        "i" = "Use the `build_plate()` function to build an empty template."
      ), call = NULL)
  }

  if (!row_check) {
    rlang::abort(
      c(
        paste0("Verify column ids in ", file_full_name, "."),
        "i" = "Expected column ids: 1, 2, 3, and so on.",
        "i" = "Use the `build_plate()` function to build an empty template."
      ), call = NULL)
  }

  if (!col_check) {
    rlang::abort(
      c(
        paste0("Verify row ids in ", file_full_name, "."),
        "i" = "Expected row ids: A, B, C, and so on.",
        "i" = "Use the `build_plate()` function to build an empty template."
      ), call = NULL)
  }

  return(plate_parameters)  # Return the processed plates
}

# Define the number of rows and columns for each plate type
plate_dims <- list(
  `6` = c(2, 3),
  `12` = c(3, 4),
  `24` = c(4, 6),
  `48` = c(6, 8),
  `96` = c(8, 12),
  `384` = c(16, 24),
  `1536` = c(32, 48)
)
