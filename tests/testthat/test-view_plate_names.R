for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
  path <- paste0("test_data/", i, "/")

  test_that("view_plate_names extracts the correct names", {
    csv_file <- paste0(path, "oneFullOnePartEmpty.csv")
    xlsx_file <- paste0(path, "oneFullOnePartEmpty.xlsx")

    result_csv <- view_plate_names(csv_file)
    result_xlsx <- view_plate_names(xlsx_file)

    expected_output <- c("full", "partial")

    expect_identical(result_csv, expected_output)
    expect_identical(result_xlsx, expected_output)
  })
}

test_that("view_plate_names throws an error when user inputs more than one file", {
  file_1 <- "test_data/allWellIds.csv"
  file_2 <- "test_data/oneFullOnePartEmpty.csv"

  expect_error(
    view_plate_names(file = c(file_1, file_2)),
    "Invalid input: More than one file provided."
  )
})

# Test for filenames extraction
test_that("view_plate_names throws an error when input file is non existant", {
  csv_file <- "test_data/doesNotExist.csv"
  xlsx_file <- "test_data/doesNotExist.xlsx"

  expect_error(view_plate_names(csv_file), "doesNotExist.csv does not exist!")
  expect_error(view_plate_names(xlsx_file), "doesNotExist.xlsx does not exist!")
})

test_that("view_plate_names throws an error when input file is not csv or xlsx", {
  file <- "test_data/badFileType.txt"

  expect_error(view_plate_names(file), "Unsupported file format. Please use CSV or Excel files.")
})

test_that("view_plate_names throws an error when input file is empty", {
  csv_file <- "test_data/emptyFile.csv"

  expect_error(view_plate_names(csv_file), "The input file or sheet is empty.")
})
