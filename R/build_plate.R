#' Generates a csv or xlsx template for each plate type
#'
#' @param plate_type A specific integer (6, 12, 24, 48, 96, 384, 1536)
#' indicating the type of microwell plate.
#' @param n_plates A positive integer indicating the number of plates.
#' @param file_type A character string ("csv" (the default) or "xlsx")
#' indicating the filetype.
#' @param plate_names A character vector of unique values that will be assigned
#' to each plate. Its length should be equal to the value of `n_plates`.
#' @param file A character string naming the file.
#'
#' @return A csv or xlsx template file.
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#' build_plate(plate_type = 6, n_plates = 2)
#' build_plate(plate_type = 6, n_plates = 2, file_type = "xlsx")
#' }
build_plate <- function(plate_type = 6,
                        n_plates = 1,
                        file_type = c("csv", "xlsx"),
                        plate_names = NULL,
                        file = NULL) {

  # Validate inputs
  file_type <- match.arg(file_type)

  if (!(plate_type %in% c(6, 12, 24, 48, 96, 384, 1536))) {
    stop("Invalid `plate_type` provided. `plate_type` must be an integer and one of 6, 12, 24, 48, 96, 384, or 1536.", call. = FALSE)
  }

  # Validate n_plates (check if it's a positive integer)
  if (!is.numeric(n_plates) || n_plates != as.integer(n_plates) || n_plates < 1) {
    stop("Invalid `n_plates` value provided. `n_plates` must be a positive integer.", call. = FALSE)
  }

  # Validate plate_names
  if (is.null(plate_names)) {
    plate_names <- paste0("plate_", 1:n_plates)
  } else {
    # Check if plate_names is a character vector
    if (!is.character(plate_names)) {
      stop("`plate_names` must be a character vector.", call. = FALSE)
    }
    if (length(plate_names) != n_plates) {
      stop("The length of `plate_names` must match `n_plates`.", call. = FALSE)
    }
    if (anyDuplicated(plate_names)) {
      duplicates <- plate_names[duplicated(plate_names)]
      stop(paste0("`plate_names` cannot have duplicates.\nDuplicated names: ", paste(unique(duplicates), collapse = ", ")), call. = FALSE)
    }
  }

  # Validate file (if provided)
  if (!is.null(file) && !is.character(file)) {
    stop("`file` must be a character string.", call. = FALSE)
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

  # Define default file path and name if not provided
  if (is.null(file)) {
    file <- paste0("template_", plate_type, "-well.", file_type)
  }

  # Export as file
  if (file_type == "xlsx") {
    openxlsx::write.xlsx(final_plate, file = file, colNames = FALSE)
  } else if (file_type == "csv") {
    write.table(final_plate, file = file, sep = ",",
                row.names = FALSE, col.names = FALSE, na = "")
  }
}
