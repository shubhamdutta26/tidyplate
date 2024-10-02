#' Returns a skeleton csv or xlsx file
#'
#' @param plate_type Type of microwell plate (double)
#' @param n_plates The total number of plates (double)
#' @param file_type Type of file; either a "csv" (DEFAULT) or "xlsx"
#' @param plate_names A character vector that will be assigned to each plate
#'
#' @return A csv or xlsx file
#' @importFrom utils write.table
#'
build_plate <- function(plate_type = 6,
                        n_plates = 1,
                        file_type = c("csv", "xlsx"),
                        plate_names = NULL) {

  # Validate inputs
  file_type <- match.arg(file_type)

  if (!(plate_type %in% c(6, 12, 24, 48, 96, 384, 1536))) {
    stop("Invalid `plate_type` provided. `plate_type` must be one of 6, 12, 24, 48, 96, 384, or 1536.", call. = FALSE)
  }

  if (is.null(plate_names)) {
    plate_names <- paste0("plate_", 1:n_plates)
  } else if (length(plate_names) != n_plates) {
    stop("The length of `plate_names` must match `n_plates`.", call. = FALSE)
  } else {
    if (anyDuplicated(plate_names)) {
      duplicates <- plate_names[duplicated(plate_names)]
      stop(paste0("`plate_names` cannot have duplicates.\nDuplicated names: ", paste(unique(duplicates), collapse = ", ")), call. = FALSE)
    }
  }

  dims <- plate_dims[[as.character(plate_type)]]
  n_rows <- dims[1]
  n_cols <- dims[2]

  # Generate row labels (extend beyond A-Z for larger plates)
  row_labels <- c(LETTERS, paste0("A", LETTERS))[1:n_rows]

  # Initialize plate list
  plate_list <- vector("list", n_plates * 2 - 1)  # For plates and spacing

  for (i in seq_len(n_plates)) {
    # Create the plate with the correct number of rows and columns
    plate <- data.frame(
      X1 = c(plate_names[i], row_labels),  # Plate name + row labels
      X2 = c(NA, rep(NA, n_rows))          # Initialize other columns as NA
    )

    for (col in seq_len(n_cols)) {
      plate[, paste0("X", col + 1)] <- c(col, rep(NA, n_rows))  # Fill columns with well numbers
    }

    plate_list[[2 * i - 1]] <- plate  # Add plate

    if (i < n_plates) {
      # Add a spacer row if more plates remain
      plate_list[[2 * i]] <- data.frame(matrix(NA, nrow = 1, ncol = n_cols + 1))
    }
  }

  # Combine plates
  final_plate <- do.call(rbind, plate_list)

  final_filename <- paste0("tidyplate_", plate_type, "_well")

  # Export as file
  if (file_type == "xlsx") {
    openxlsx::write.xlsx(final_plate, paste0(final_filename, ".xlsx"), colNames = FALSE)
  } else if (file_type == "csv") {
    write.table(final_plate, paste0(final_filename, ".csv"), sep = ",",
                row.names = FALSE, col.names = FALSE, na = "")
  }
}
