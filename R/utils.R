# This function names each column the right way in the final dataset
# May not be needed (Needs more check)
naming_cols <- function(df) {

  nm <- colnames(df)[1]
  nm2 <- paste0(sample(LETTERS[1:26], 5), sample(letters[1:26], 5), sample(1:26, 5), collapse = "")
  nm3 <- paste0(sample(LETTERS[1:26], 5), sample(letters[1:26], 5), sample(1:26, 5), collapse = "")

  dplyr::mutate(df, !!nm3 := paste0(df[[1]], df[[2]])) |>
    dplyr::rename(!!nm2 := 1, !!nm := 3) |>
    dplyr::select(4, 3) |>
    dplyr::rename(well_id = nm3)
}
