# read_data tests
test_that("read_data throws error when not csv or excel file or file/ sheet is empty", {
  file_txt <- "test_data/badFileType.txt"
  file_csv <- "test_data/allWellIds.csv"
  file_xlsx <- "test_data/allWellIds.xlsx"
  file_csv_empty <- "test_data/emptyFile.csv"
  file_xlsx_empty <- "test_data/emptySheet2.xlsx"

  expect_no_error(read_data(file_csv))
  expect_no_error(read_data(file_xlsx))
  expect_error(read_data(file_txt),
               "Unsupported file format. Please use CSV or Excel files.")
  expect_error(read_data(file_csv_empty),
               "The input file or sheet is empty.")
  expect_error(read_data(file_xlsx_empty, sheet = 2),
               "The input file or sheet is empty.")
})
# plate_params tests
for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
  path <- paste0("test_data/", i, "/")

  test_that("plate_params can return a list of those parameters", {
    csv_file <- paste0(path, "allWellIds.csv")
    xlsx_file <- paste0(path, "allWellIds.xlsx")

    imported_csv <- suppressMessages((tibble::as_tibble(
      data.table::fread(
        csv_file,
        sep = ",",
        header = FALSE,
        na.strings = ""
      )
    )))
    imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
    n_cols_csv <- ncol(imported_csv)
    n_cols_xlsx <- ncol(imported_csv)

    result_csv <- plate_params(imported_csv, n_cols_csv)
    result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

    expect_type(result_csv, "list")
    expect_type(result_xlsx, "list")
  })
}

test_that("6 well params are identical to 6 well plate", {
  path <- "test_data/6/"
  csv_file <- paste0(path, "allWellIds.csv")
  xlsx_file <- paste0(path, "allWellIds.xlsx")

  imported_csv <- suppressMessages(tibble::as_tibble(
    data.table::fread(
      csv_file,
      sep = ",",
      header = FALSE,
      na.strings = ""
    )
  ))
  imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
  n_cols_csv <- ncol(imported_csv)
  n_cols_xlsx <- ncol(imported_csv)

  result_csv <- plate_params(imported_csv, n_cols_csv)
  result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

  expected <- list(1L, 3L, 6L, c(1:4), 3L, 4L, LETTERS[1:2], c(1:3))

  expect_identical(result_csv, expected)
  expect_identical(result_xlsx, expected)
})

test_that("12 well params are identical to 12 well plate", {
  path <- "test_data/12/"
  csv_file <- paste0(path, "allWellIds.csv")
  xlsx_file <- paste0(path, "allWellIds.xlsx")

  imported_csv <- suppressMessages(tibble::as_tibble(
    data.table::fread(
      csv_file,
      sep = ",",
      header = FALSE,
      na.strings = ""
    )
  ))
  imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
  n_cols_csv <- ncol(imported_csv)
  n_cols_xlsx <- ncol(imported_csv)

  result_csv <- plate_params(imported_csv, n_cols_csv)
  result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

  expected <- list(1L, 4L, 12L, c(1:5), 4L, 5L, LETTERS[1:3], c(1:4))

  expect_identical(result_csv, expected)
  expect_identical(result_xlsx, expected)
})

test_that("24 well params are identical to 24 well plate", {
  path <- "test_data/24/"
  csv_file <- paste0(path, "allWellIds.csv")
  xlsx_file <- paste0(path, "allWellIds.xlsx")

  imported_csv <- suppressMessages(tibble::as_tibble(
    data.table::fread(
      csv_file,
      sep = ",",
      header = FALSE,
      na.strings = ""
    )
  ))
  imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
  n_cols_csv <- ncol(imported_csv)
  n_cols_xlsx <- ncol(imported_csv)

  result_csv <- plate_params(imported_csv, n_cols_csv)
  result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

  expected <- list(1L, 5L, 24L, c(1:7), 5L, 6L, LETTERS[1:4], c(1:6))

  expect_identical(result_csv, expected)
  expect_identical(result_xlsx, expected)
})

test_that("48 well params are identical to 48 well plate", {
  path <- "test_data/48/"
  csv_file <- paste0(path, "allWellIds.csv")
  xlsx_file <- paste0(path, "allWellIds.xlsx")

  imported_csv <- suppressMessages(tibble::as_tibble(
    data.table::fread(
      csv_file,
      sep = ",",
      header = FALSE,
      na.strings = ""
    )
  ))
  imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
  n_cols_csv <- ncol(imported_csv)
  n_cols_xlsx <- ncol(imported_csv)

  result_csv <- plate_params(imported_csv, n_cols_csv)
  result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

  expected <- list(1L, 7L, 48L, c(1:9), 7L, 8L, LETTERS[1:6], c(1:8))

  expect_identical(result_csv, expected)
  expect_identical(result_xlsx, expected)
})

test_that("96 well params are identical to 96 well plate", {
  path <- "test_data/96/"
  csv_file <- paste0(path, "allWellIds.csv")
  xlsx_file <- paste0(path, "allWellIds.xlsx")

  imported_csv <- suppressMessages(tibble::as_tibble(
    data.table::fread(
      csv_file,
      sep = ",",
      header = FALSE,
      na.strings = ""
    )
  ))
  imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
  n_cols_csv <- ncol(imported_csv)
  n_cols_xlsx <- ncol(imported_csv)

  result_csv <- plate_params(imported_csv, n_cols_csv)
  result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

  expected <- list(1L, 9L, 96L, c(1:13), 9L, 10L, LETTERS[1:8], c(1:12))

  expect_identical(result_csv, expected)
  expect_identical(result_xlsx, expected)
})

test_that("384 well params are identical to 384 well plate", {
  path <- "test_data/384/"
  csv_file <- paste0(path, "allWellIds.csv")
  xlsx_file <- paste0(path, "allWellIds.xlsx")

  imported_csv <- suppressMessages(tibble::as_tibble(
    data.table::fread(
      csv_file,
      sep = ",",
      header = FALSE,
      na.strings = ""
    )
  ))
  imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
  n_cols_csv <- ncol(imported_csv)
  n_cols_xlsx <- ncol(imported_csv)

  result_csv <- plate_params(imported_csv, n_cols_csv)
  result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

  expected <- list(1L, 17L, 384L, c(1:25), 17L, 18L, LETTERS[1:16], c(1:24))

  expect_identical(result_csv, expected)
  expect_identical(result_xlsx, expected)
})

test_that("1536 well params are identical to 1536 well plate", {
  path <- "test_data/1536/"
  csv_file <- paste0(path, "allWellIds.csv")
  xlsx_file <- paste0(path, "allWellIds.xlsx")

  imported_csv <- suppressMessages(tibble::as_tibble(
    data.table::fread(
      csv_file,
      sep = ",",
      header = FALSE,
      na.strings = ""
    )
  ))
  imported_xlsx <- suppressMessages(readxl::read_xlsx(xlsx_file, col_names = FALSE))
  n_cols_csv <- ncol(imported_csv)
  n_cols_xlsx <- ncol(imported_csv)

  result_csv <- plate_params(imported_csv, n_cols_csv)
  result_xlsx <- plate_params(imported_xlsx, n_cols_xlsx)

  expected <- list(1L, 33L, 1536L, c(1:49), 33L, 34L, c(LETTERS[1:26], paste0("A", LETTERS[1:6])), c(1:48))

  expect_identical(result_csv, expected)
  expect_identical(result_xlsx, expected)
})

