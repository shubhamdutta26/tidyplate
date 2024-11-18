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

  test_that("view_plate_names throws error when plate format is not correct", {
    csv_file_1 <- paste0(path, "extraCol.csv")
    csv_file_2 <- paste0(path, "twoPlatesExtraCol.csv")
    xlsx_file_1 <- paste0(path, "extraCol.xlsx")
    xlsx_file_2 <- paste0(path, "twoPlatesExtraCol.xlsx")

    expect_error(
      view_plate_names(csv_file_1),
      regexp = "is not a valid input file"
    )
    expect_error(
      view_plate_names(csv_file_2),
      regexp = "is not a valid input file"
    )
    expect_error(
      view_plate_names(xlsx_file_1),
      regexp = "is not a valid input file"
    )
    expect_error(
      view_plate_names(xlsx_file_2),
      regexp = "is not a valid input file"
    )
  })

  test_that("view_plate_names message if no plate names", {
    csv_file <- paste0(path, "noName.csv")
    xlsx_file <- paste0(path, "noName.xlsx")

    expect_message(view_plate_names(csv_file), regexp = "Empty ")
    expect_message(view_plate_names(xlsx_file), regexp = "Empty ")
  })

  test_that("view_plate_names message if duplicated plate names", {
    csv_file <- paste0(path, "sameNames.csv")
    xlsx_file <- paste0(path, "sameNames.xlsx")

    expect_message(view_plate_names(csv_file), regexp = "Duplicated plate ")
    expect_message(view_plate_names(xlsx_file), regexp = "Duplicated plate ")
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

  expect_error(view_plate_names(csv_file),
               regexp = " does not exist!")
  expect_error(view_plate_names(xlsx_file),
               regexp = " does not exist!")
})

test_that("view_plate_names throws an error when input file is not csv or xlsx", {
  file <- "test_data/badFileType.txt"

  expect_error(view_plate_names(file),
               regexp = "Unsupported file format. Please use CSV or Excel ")
})

test_that("view_plate_names throws an error when input file is empty", {
  csv_file <- "test_data/emptyFile.csv"

  expect_error(view_plate_names(csv_file),
               regexp = "file is empty")
})


test_that("view_plate_names throws error when plate format is not correct", {
  csv_file <- "test_data/extraRow.csv"
  xlsx_file <- "test_data/extraRow.xlsx"

  expect_error(
    view_plate_names(csv_file),
    regexp = "is not a valid input file"
  )
  expect_error(
    view_plate_names(xlsx_file),
    regexp = "is not a valid input file"
  )
})

test_that("view_plate_names error when no name and duplicates", {
  csv_file <- "test_data/noNameDup.csv"
  xlsx_file <- "test_data/noNameDup.xlsx"

  expect_error(view_plate_names(csv_file),
               regexp = "& duplicate plate names found in")
  expect_error(view_plate_names(xlsx_file),
               regexp = "& duplicate plate names found in")
})
