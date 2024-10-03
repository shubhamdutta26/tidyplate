for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
  path <- paste0("test_data/", i, "/")

  test_that("tidy_plate works for complete valid data", {
    csv_file <- paste0(path, "allWellIds.csv")
    xlsx_file <- paste0(path, "allWellIds.xlsx")

    result_csv <- suppressMessages(tidy_plate(csv_file, well_id = "wells"))
    result_xlsx <- suppressMessages(tidy_plate(xlsx_file, well_id = "wells"))

    expect_identical(result_csv$values, result_csv$wells)
    expect_identical(result_xlsx$values, result_xlsx$wells)
  })

  test_that("tidy_plate works for missing data from plate", {
    csv_file <- paste0(path, "wellIdsAndEmptyWells.csv")
    xlsx_file <- paste0(path, "wellIdsAndEmptyWells.xlsx")

    result_csv <- suppressMessages(tidy_plate(csv_file, well_id = "wells"))
    result_xlsx <- suppressMessages(tidy_plate(xlsx_file, well_id = "wells"))

    expect_identical(result_csv$values, result_csv$wells)
    expect_identical(result_xlsx$values, result_xlsx$wells)
  })

  test_that("tidy_plate works with one full plate and one partially empty", {
    csv_file <- paste0(path, "oneFullOnePartEmpty.csv")
    xlsx_file <- paste0(path, "oneFullOnePartEmpty.xlsx")

    result_csv <- suppressMessages(tidy_plate(csv_file, well_id = "wells"))
    result_xlsx <- suppressMessages(tidy_plate(xlsx_file, well_id = "wells"))

    expect_identical(result_csv$full, result_csv$wells)
    expect_identical(result_xlsx$full, result_xlsx$wells)
    r_csv <- is.na(result_csv$partial) | result_csv$partial == as.character(result_csv$wells)
    r_xlsx <- is.na(result_xlsx$partial) | result_xlsx$partial == as.character(result_xlsx$wells)
    expect_true(all(r_csv))
    expect_true(all(r_xlsx))
  })

  test_that("tidy_plate returns a tbl_df", {
    expect_s3_class(suppressMessages(tidy_plate(paste0(path, "allWellIds.csv"))), "tbl_df")
    expect_s3_class(suppressMessages(tidy_plate(paste0(path, "allWellIds.xlsx"))), "tbl_df")
  })

  test_that("tidy_plate allows weird characters", {
    csv_file <- paste0(path, "weirdCharacters.csv")
    xlsx_file <- paste0(path, "weirdCharacters.xlsx")

    result_csv <- suppressMessages(tidy_plate(csv_file, well_id = "wells"))
    result_xlsx <- suppressMessages(tidy_plate(xlsx_file, well_id = "wells"))

    expect_identical(result_csv$values[1:3], c("#a", "`abc", "~"))
    expect_identical(result_xlsx$values[1:3], c("#a", "`abc", "~"))
  })

  test_that("tidy_plate works with single well", {
    csv_file <- paste0(path, "oneWell.csv")
    xlsx_file <- paste0(path, "oneWell.xlsx")

    result_csv <- suppressMessages(tidy_plate(csv_file, well_id = "wells"))
    result_xlsx <- suppressMessages(tidy_plate(xlsx_file, well_id = "wells"))

    expect_identical(result_csv$values, "singleton")
    expect_identical(result_xlsx$values, "singleton")
  })

  test_that("tidy_plate throws error when plate format is not correct", {
    csv_file_1 <- paste0(path, "extraCol.csv")
    csv_file_2 <- paste0(path, "twoPlatesExtraCol.csv")
    xlsx_file_1 <- paste0(path, "extraCol.xlsx")
    xlsx_file_2 <- paste0(path, "twoPlatesExtraCol.xlsx")

    expect_error(
      tidy_plate(csv_file_1, well_id = "wells"),
      "extraCol.csv is not a valid input file. Please review an example dataset."
    )
    expect_error(
      tidy_plate(csv_file_2, well_id = "wells"),
      "twoPlatesExtraCol.csv is not a valid input file. Please review an example dataset."
    )
    expect_error(
      tidy_plate(xlsx_file_1, well_id = "wells"),
      "extraCol.xlsx is not a valid input file. Please review an example dataset."
    )
    expect_error(
      tidy_plate(xlsx_file_2, well_id = "wells"),
      "twoPlatesExtraCol.xlsx is not a valid input file. Please review an example dataset."
    )
  })

  test_that("tidy_plate throws error when well_id argument matches plate names", {
    csv_file <- paste0(path, "oneFullOnePartEmpty.csv")
    xlsx_file <- paste0(path, "oneFullOnePartEmpty.xlsx")

    expect_error(
      tidy_plate(csv_file, well_id = "full"),
      "Plate names cannot be the same as argument `well_id`."
    )
    expect_error(
      tidy_plate(xlsx_file, well_id = "full"),
      "Plate names cannot be the same as argument `well_id`."
    )
  })

  test_that("tidy_plate throws error when plate names are either empty or not unique", {
    csv_file_1 <- paste0(path, "sameNames.csv")
    xlsx_file_1 <- paste0(path, "sameNames.xlsx")
    csv_file_2 <- paste0(path, "noName.csv")
    xlsx_file_2 <- paste0(path, "noName.xlsx")

    expect_error(
      tidy_plate(csv_file_1, well_id = "wells"),
      "Verify that each plate in sameNames.csv has a unique name."
    )
    expect_error(
      tidy_plate(xlsx_file_1, well_id = "wells"),
      "Verify that each plate in sameNames.xlsx has a unique name."
    )
    expect_error(
      tidy_plate(csv_file_2, well_id = "wells"),
      "Verify that each plate in noName.csv has a unique name."
    )
    expect_error(
      tidy_plate(xlsx_file_2, well_id = "wells"),
      "Verify that each plate in noName.xlsx has a unique name."
    )
  })

  test_that("tidy_plate throws error when row and column ids are empty or bad", {
    csv_file_1 <- paste0(path, "badColRowName.csv")
    xlsx_file_1 <- paste0(path, "badColRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyColRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyColRowName.xlsx")

    expect_error(
      tidy_plate(csv_file_1, well_id = "wells"),
      "Verify row and column ids in badColRowName.csv.")
    expect_error(
      tidy_plate(xlsx_file_1, well_id = "wells"),
      "Verify row and column ids in badColRowName.xlsx.")
    expect_error(
      tidy_plate(csv_file_2, well_id = "wells"),
      "Verify row and column ids in emptyColRowName.csv.")
    expect_error(
      tidy_plate(xlsx_file_2, well_id = "wells"),
      "Verify row and column ids in emptyColRowName.xlsx.")
  })

  test_that("tidy_plate throws error when row id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badRowName.csv")
    xlsx_file_1 <- paste0(path, "badRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyRowName.xlsx")

    expect_error(tidy_plate(csv_file_1, well_id = "wells"), "Verify row id(s) in badRowName.csv.", fixed = TRUE)
    expect_error(tidy_plate(xlsx_file_1, well_id = "wells"), "Verify row id(s) in badRowName.xlsx.", fixed = TRUE)
    expect_error(tidy_plate(csv_file_2, well_id = "wells"), "Verify row id(s) in emptyRowName.csv.", fixed = TRUE)
    expect_error(tidy_plate(xlsx_file_2, well_id = "wells"), "Verify row id(s) in emptyRowName.xlsx.", fixed = TRUE)
  })

  test_that("tidy_plate throws error when column id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badColName.csv")
    xlsx_file_1 <- paste0(path, "badColName.xlsx")
    csv_file_2 <- paste0(path, "emptyColName.csv")
    xlsx_file_2 <- paste0(path, "emptyColName.xlsx")

    expect_error(tidy_plate(csv_file_1, well_id = "wells"), "Verify column id(s) in badColName.csv.", fixed = TRUE)
    expect_error(tidy_plate(xlsx_file_1, well_id = "wells"), "Verify column id(s) in badColName.xlsx.", fixed = TRUE)
    expect_error(tidy_plate(csv_file_2, well_id = "wells"), "Verify column id(s) in emptyColName.csv.", fixed = TRUE)
    expect_error(tidy_plate(xlsx_file_2, well_id = "wells"), "Verify column id(s) in emptyColName.xlsx.", fixed = TRUE)
  })
}

test_that("tidy_plate throws an error when user inputs more than one file", {
  file_1 <- "test_data/allWellIds.csv"
  file_2 <- "test_data/oneFullOnePartEmpty.csv"

  expect_error(
    tidy_plate(file = c(file_1, file_2)),
    "Invalid input: More than one file provided."
  )
})

test_that("tidy_plate throws an error when `well_id` argument not a character vector of length 1", {
  file <- "test_data/allWellIds.csv"

  expect_error(tidy_plate(file, well_id = 1), "`well_id` should be a character vector of length 1")
  expect_error(tidy_plate(file, well_id = 1L), "`well_id` should be a character vector of length 1")
  expect_error(tidy_plate(file, well_id = c("xxx", "yyy")), "`well_id` should be a character vector of length 1")
  expect_error(tidy_plate(file, well_id = TRUE), "`well_id` should be a character vector of length 1")
})

# Test for filenames extraction
test_that("tidy_plate throws an error when input file is non existant", {
  csv_file <- "test_data/doesNotExist.csv"
  xlsx_file <- "test_data/doesNotExist.xlsx"

  expect_error(tidy_plate(csv_file), "doesNotExist.csv does not exist!")
  expect_error(tidy_plate(xlsx_file), "doesNotExist.xlsx does not exist!")
})

test_that("tidy_plate throws an error when input file is not csv or xlsx", {
  file <- "test_data/badFileType.txt"

  expect_error(tidy_plate(file), "Unsupported file format. Please use CSV or Excel files.")
})

test_that("tidy_plate throws an error when input file is empty", {
  csv_file <- "test_data/emptyFile.csv"

  expect_error(tidy_plate(csv_file), "The input file or sheet is empty.")
})
