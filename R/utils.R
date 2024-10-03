# Function to read data based on file extension
read_data <- function(file, sheet = NULL) {

  # Get file ext
  file_ext <- tolower(tools::file_ext(file))

  # Check for supported file extensions
  if (file_ext == "csv") {
    raw_data <- tryCatch({
      suppressWarnings(
        tibble::as_tibble(data.table::fread(file, sep = ",", header = FALSE, na.strings = ""))
      )
    }, error = function(e) {
      stop("Error reading CSV file: ", e$message, call. = FALSE)
    })
  } else if (file_ext %in% c("xls", "xlsx")) {
    raw_data <- tryCatch({
      suppressMessages(
        readxl::read_excel(file, sheet = sheet, col_names = FALSE)
      )
    }, error = function(e) {
      stop("Error reading Excel file: ", e$message, call. = FALSE)
    })
  } else {
    stop("Unsupported file format. Please use CSV or Excel files.", call. = FALSE)
  }

  # Validate that the data is not empty
  if (nrow(raw_data) == 0 || ncol(raw_data) == 0) {
    stop("The input file or sheet is empty.", call. = FALSE)
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
    stop(paste0(file_full_name, " is not a valid input file. Please review an example dataset."), call. = FALSE)
  }

  ## Count the number of plates in the dataset----
  row_start <- 1L
  plate_parameters <- plate_params(raw_data, count_columns)

  ## Check whether the input formatting is correct or not----
  if (plate_parameters[[2]] != count_rows_actual) {
    stop(paste0(file_full_name, " is not a valid input file. Please review an example dataset."), call. = FALSE)
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
    stop("Plate names cannot be the same as argument `well_id`.", call. = FALSE)
  }

  ## Check if all plate names are unique and non-missing
  if (anyNA(each_plate_name) || length(each_plate_name) != length(unique(each_plate_name))) {
    stop(paste0("Verify that each plate in ", file_full_name, " has a unique name."), call. = FALSE)
  }

  if (sum(is_list_of_dfs_empty(list_of_plates)) == plate_parameters[[1]]) {
    stop("Plate(s) are empty.", call. = FALSE)
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
    stop(paste0("Verify row and column ids in ", file_full_name, "."), call. = FALSE)
  }

  if (!row_check) {
    stop(paste0("Verify column id(s) in ", file_full_name, "."), call. = FALSE)
  }

  if (!col_check) {
    stop(paste0("Verify row id(s) in ", file_full_name, "."), call. = FALSE)
  }

  return(list(plate_parameters, list_of_plates))  # Return the processed plates
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

# Function to convert the first row into header
convert_first_row_to_header <- function(df) {
  new_header <- as.character(df[1,])  # Extract first row
  df <- df[-1,]                        # Remove first row
  colnames(df) <- new_header           # Assign new header
  return(df)                           # Return modified dataframe
}

# Function to extract specific dataframes by indices
extract_plates <- function(list, indices) {
  return(list[indices])  # Subset the list using the provided indices
}
