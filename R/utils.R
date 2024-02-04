# This function generates different plate parameters if the data is formatted
# correctly
plate_params <- function(data, n_cols) {
  if (n_cols == 4L) {
    no_of_plates <- sum(rowSums(is.na(data)) == 4L) + 1L
    count_rows_theoretical <- (no_of_plates * 4L) - 1L
    plate_type <- 6L
    cols <- 1L:4L
    row_end <- 3L
    increment <- 4L
    first_col_vec <- LETTERS[1:2]
    first_row_vec <- 1L:3L
  } else if (n_cols == 5L) {
    no_of_plates <- sum(rowSums(is.na(data)) == 5L) + 1L
    count_rows_theoretical <- (no_of_plates * 5L) - 1L
    plate_type <- 12L
    cols <- 1L:5L
    row_end <- 4L
    increment <- 5L
    first_col_vec <- LETTERS[1:3]
    first_row_vec <- 1L:4L
  } else if (n_cols == 7L) {
    no_of_plates <- sum(rowSums(is.na(data)) == 7L) + 1L
    count_rows_theoretical <- (no_of_plates * 6L) - 1L
    plate_type <- 24L
    cols <- 1L:7L
    row_end <- 5L
    increment <- 6L
    first_col_vec <- LETTERS[1:4]
    first_row_vec <- 1L:6L
  } else if (n_cols == 9L) {
    no_of_plates <- sum(rowSums(is.na(data)) == 9L) + 1L
    count_rows_theoretical <- (no_of_plates * 8L) - 1L
    plate_type <- 48L
    cols <- 1L:9L
    row_end <- 7L
    increment <- 8L
    first_col_vec <- LETTERS[1:6]
    first_row_vec <- 1L:8L
  } else if (n_cols == 13L) {
    no_of_plates <- sum(rowSums(is.na(data)) == 13L) + 1L
    count_rows_theoretical <- (no_of_plates * 10L) - 1L
    plate_type <- 96L
    cols <- 1L:13L
    row_end <- 9L
    increment <- 10L
    first_col_vec <- LETTERS[1:8]
    first_row_vec <- 1:12
  } else if (n_cols == 25L) {
    no_of_plates <- sum(rowSums(is.na(data)) == 25L) + 1L
    count_rows_theoretical <- (no_of_plates * 18L) - 1L
    plate_type <- 384L
    cols <- 1L:25L
    row_end <- 17L
    increment <- 18L
    first_col_vec <- LETTERS[1:16]
    first_row_vec <- 1L:24L
  } else if (n_cols == 49L) {
    no_of_plates <- sum(rowSums(is.na(data)) == 49L) + 1L
    count_rows_theoretical <- (no_of_plates * 34L) - 1L
    plate_type <- 1536L
    cols <- 1L:49L
    row_end <- 33L
    increment <- 34L
    first_col_vec <- c(LETTERS[1:26], "AA", "AB", "AC", "AD", "AE", "AF")
    first_row_vec <- 1L:48L
  }
  return(list(no_of_plates, count_rows_theoretical, plate_type, cols, row_end, increment, first_col_vec, first_row_vec))
}


# This function names each column the right way in the final dataset
naming_cols <- function(df, well_id) {
  nm <- colnames(df)[1]
  nm2 <- paste0(sample(LETTERS[1:26], 5), sample(letters[1:26], 5), sample(1:26, 5), collapse = "")
  nm3 <- paste0(sample(LETTERS[1:26], 5), sample(letters[1:26], 5), sample(1:26, 5), collapse = "")

  df_new <- dplyr::mutate(df, !!nm3 := paste0(df[[1]], df[[2]])) |>
    dplyr::rename(!!nm2 := 1, !!nm := 3) |>
    dplyr::select(4, 3)
  df_new[[1]] <- sub("(\\D)(\\d)$", "\\10\\2", df_new[[1]])
  names(df_new)[1] <- well_id
  return(df_new)
}
