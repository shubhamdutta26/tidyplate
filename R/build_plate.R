#' Build a csv or xlsx template for each plate type
#'
#' @description
#' `build_plate()` helps the user build an empty csv or xlsx file that can be
#' used as a template for storing plate data. Ensure that plate names are unique.
#' Once populated it can be used as an input for `tidy_plate()` function.
#'
#' @param plate_type A specific integer (6, 12, 24, 48, 96, 384, or 1536)
#' indicating the type of microwell plate.
#' @param n_plates A positive integer indicating the number of plates.
#' @param plate_names A character vector of unique values that will be assigned
#' to each plate. Its length should be equal to the value of `n_plates`.
#' @param file A character string naming the file.
#' @param file_type `r lifecycle::badge("deprecated")` A character string of the
#' output file type. It can either be a csv or xlsx file. The default is csv.
#'
#' @seealso [tidy_plate()], [generate_plate()]
#'
#' @return A csv or xlsx template file.
#' @export
#' @importFrom utils write.table
#'
#' @examples
#' temp_file <- tempfile(fileext = ".csv")
#'
#' build_plate(plate_type = 6, n_plates = 2, file = temp_file)
build_plate <- function(plate_type = 6,
                        n_plates = 1,
                        plate_names = NULL,
                        file = NULL,
                        file_type = NULL) {

  # Warn about deprecated `file_type` argument
  if (!is.null(file_type)) {
    lifecycle::deprecate_soft("2.1.0",
                              "build_plate(file_type)",
                              "build_plate(file)")
  }

  # Validate `plate_type`
  if (!(plate_type %in% c(6, 12, 24, 48, 96, 384, 1536))) {
    rlang::abort(c("Invalid `plate_type` provided.",
                   "i" = "`plate_type` must be an integer and one of 6, 12, 24, 48, 96, 384, or 1536."),
                 call = NULL)
  }

  # Validate `n_plates`
  if (!is.numeric(n_plates) || n_plates != as.integer(n_plates) || n_plates < 1) {
    rlang::abort(c("Invalid `n_plates` value provided.",
                   "i" = "`n_plates` must be a positive integer."),
                 call = NULL)
  }

  # Validate `plate_names`
  if (is.null(plate_names)) {
    plate_names <- paste0("plate_", 1:n_plates)
  } else {
    if (!is.character(plate_names)) {
      rlang::abort("`plate_names` must be a character vector.", call = NULL)
    }
    if (length(plate_names) != n_plates) {
      rlang::abort("The length of `plate_names` must match `n_plates`.", call = NULL)
    }
    if (anyDuplicated(plate_names)) {
      duplicates <- plate_names[duplicated(plate_names)]
      rlang::abort(c("`plate_names` cannot have duplicates.",
                     "i" = "\nDuplicated names: ", paste(unique(duplicates), collapse = ", ")),
                   call = NULL)
    }
  }

  # Validate `file`
  if (!is.null(file) && !is.character(file)) {
    rlang::abort("`file` must be a character string.", call = NULL)
  }

  # Determine file extension from `file` or fallback to `file_type`
  if (is.null(file)) {
    file_extension <- if (!is.null(file_type)) file_type else "csv"
    file <- paste0("template_", plate_type, "-well.", file_extension)
  } else {
    file_extension <- tools::file_ext(file)
    if (file_extension == "") {
      file_extension <- if (!is.null(file_type)) file_type else "csv"
      file <- paste0(file, ".", file_extension)
    }
  }

  dims <- plate_dims[[as.character(plate_type)]]
  n_rows <- dims[1]
  n_cols <- dims[2]
  row_labels <- c(LETTERS, paste0("A", LETTERS))[1:n_rows]

  # Initialize plate list
  plate_list <- vector("list", n_plates * 2 - 1)

  for (i in seq_len(n_plates)) {
    plate <- data.frame(
      X1 = c(plate_names[i], row_labels),
      X2 = c(NA, rep(NA, n_rows))
    )
    for (col in seq_len(n_cols)) {
      plate[, paste0("X", col + 1)] <- c(col, rep(NA, n_rows))
    }
    plate_list[[2 * i - 1]] <- plate
    if (i < n_plates) {
      plate_list[[2 * i]] <- data.frame(matrix(NA, nrow = 1, ncol = n_cols + 1))
    }
  }

  final_plate <- do.call(rbind, plate_list)

  # Export based on file extension
  if (file_extension == "xlsx") {
    openxlsx::write.xlsx(final_plate, file = file, colNames = FALSE)
  } else if (file_extension == "csv") {
    write.table(final_plate, file = file, sep = ",",
                row.names = FALSE, col.names = FALSE, na = "")
  } else {
    rlang::abort("Unsupported file type. Only 'csv' and 'xlsx' are supported.", call = NULL)
  }
}
