# This function names each column the right way in the final dataset
naming_cols <- function(df) {
  nm <- colnames(df)[1]
  dplyr::mutate(df, well = paste0(df[[1]], df[[2]])) |>
    dplyr::rename(old_first = 1, !!nm := 3) |>
    dplyr::select(4, 3)
}
