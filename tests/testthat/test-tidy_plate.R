for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
  path <- paste0("test_data/", i, "/")

  test_that("tidy_plate works for same data type for all plates in a single file", {
    csv_num <- paste0(path, "allPlatesNumeric.csv")
    xlsx_num <- paste0(path, "allPlatesNumeric.xlsx")
    csv_char <- paste0(path, "allPlatesChar.csv")
    xlsx_char <- paste0(path, "allPlatesChar.xlsx")

    expect_no_error(suppressMessages(tidy_plate(csv_num, well_id = "wells")))
    expect_no_error(suppressMessages(tidy_plate(xlsx_num, well_id = "wells")))
    expect_no_error(suppressMessages(tidy_plate(csv_char, well_id = "wells")))
    expect_no_error(suppressMessages(tidy_plate(xlsx_char, well_id = "wells")))
  })

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
      regexp = "is not a valid input file"
    )
    expect_error(
      tidy_plate(csv_file_2, well_id = "wells"),
      regexp = "is not a valid input file"
    )
    expect_error(
      tidy_plate(xlsx_file_1, well_id = "wells"),
      regexp = "is not a valid input file"
    )
    expect_error(
      tidy_plate(xlsx_file_2, well_id = "wells"),
      regexp = "is not a valid input file"
    )
  })

  test_that("tidy_plate throws error when well_id argument matches plate names", {
    csv_file <- paste0(path, "oneFullOnePartEmpty.csv")
    xlsx_file <- paste0(path, "oneFullOnePartEmpty.xlsx")

    expect_error(
      tidy_plate(csv_file, well_id = "full"),
      regexp = "`well_id` value is invalid"
    )
    expect_error(
      tidy_plate(xlsx_file, well_id = "full"),
      regexp = "`well_id` value is invalid"
    )
  })

  test_that("tidy_plate throws error when plate names are empty", {

    csv_file_1 <- paste0(path, "noName.csv")
    xlsx_file_1 <- paste0(path, "noName.xlsx")

    expect_error(
      tidy_plate(csv_file_1, well_id = "wells"),
      regexp = "Empty (blank or NA) plate name(s) found in", fixed = TRUE)
    expect_error(
      tidy_plate(xlsx_file_1, well_id = "wells"),
      regexp = "Empty (blank or NA) plate name(s) found in", fixed = TRUE)
  })

  test_that("tidy_plate throws error when plate names are not unique", {
    csv_file_1 <- paste0(path, "sameNames.csv")
    xlsx_file_1 <- paste0(path, "sameNames.xlsx")

    expect_error(
      tidy_plate(csv_file_1, well_id = "wells"),
      regexp = "Duplicated plate name(s) found in", fixed = TRUE)

    expect_error(
      tidy_plate(xlsx_file_1, well_id = "wells"),
      regexp = "Duplicated plate name(s) found in", fixed = TRUE)
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

    expect_error(tidy_plate(csv_file_1, well_id = "wells"),
                 regexp = "Verify row ids in ")
    expect_error(tidy_plate(xlsx_file_1, well_id = "wells"),
                 regexp = "Verify row ids in ")
    expect_error(tidy_plate(csv_file_2, well_id = "wells"),
                 regexp = "Verify row ids in ")
    expect_error(tidy_plate(xlsx_file_2, well_id = "wells"),
                 regexp = "Verify row ids in ")
  })

  test_that("tidy_plate throws error when column id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badColName.csv")
    xlsx_file_1 <- paste0(path, "badColName.xlsx")
    csv_file_2 <- paste0(path, "emptyColName.csv")
    xlsx_file_2 <- paste0(path, "emptyColName.xlsx")

    expect_error(tidy_plate(csv_file_1, well_id = "wells"),
                 regexp = "Verify column id(s) in ")
    expect_error(tidy_plate(xlsx_file_1, well_id = "wells"),
                 regexp = "Verify column id(s) in ")
    expect_error(tidy_plate(csv_file_2, well_id = "wells"),
                 regexp = "Verify column id(s) in ")
    expect_error(tidy_plate(xlsx_file_2, well_id = "wells"),
                 regexp = "Verify column id(s) in ")
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

  expect_error(tidy_plate(file, well_id = 1), "`well_id` should be a single character string.")
  expect_error(tidy_plate(file, well_id = 1L), "`well_id` should be a single character string.")
  expect_error(tidy_plate(file, well_id = c("xxx", "yyy")), "`well_id` should be a single character string.")
  expect_error(tidy_plate(file, well_id = TRUE), "`well_id` should be a single character string.")
})

# Test for filenames extraction
test_that("tidy_plate throws an error when input file is non existant", {
  csv_file <- "test_data/doesNotExist.csv"
  xlsx_file <- "test_data/doesNotExist.xlsx"

  expect_error(tidy_plate(csv_file), "File does not exist!")
  expect_error(tidy_plate(xlsx_file), "File does not exist!")
})

test_that("tidy_plate throws an error when input file is not csv or xlsx", {
  file <- "test_data/badFileType.txt"

  expect_error(tidy_plate(file),
               regexp = "Unsupported file format. Please use CSV or Excel ")
})

test_that("tidy_plate throws an error when input file is empty", {
  csv_file <- "test_data/emptyFile.csv"

  expect_error(tidy_plate(csv_file),
               regexp = " is empty")
})
