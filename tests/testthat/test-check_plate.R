for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
  path <- paste0("test_data/", i, "/")

  test_that("check_plate throws error when plate format is not correct", {
    csv_file_1 <- paste0(path, "extraCol.csv")
    csv_file_2 <- paste0(path, "twoPlatesExtraCol.csv")
    xlsx_file_1 <- paste0(path, "extraCol.xlsx")
    xlsx_file_2 <- paste0(path, "twoPlatesExtraCol.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "extraCol.csv: Not OK; Verify input data format."
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "twoPlatesExtraCol.csv: Not OK; Verify input data format."
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "extraCol.xlsx: Not OK; Verify input data format."
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "twoPlatesExtraCol.xlsx: Not OK; Verify input data format."
    )
  })

  test_that("check_plate throws error when well_id argument matches plate names", {
    csv_file <- paste0(path, "oneFullOnePartEmpty.csv")
    xlsx_file <- paste0(path, "oneFullOnePartEmpty.xlsx")

    expect_error(
      check_plate(csv_file, well_id = "full"),
      "oneFullOnePartEmpty.csv: Not OK; Plate names cannot be the same as variable 'well_id'"
    )
    expect_error(
      check_plate(xlsx_file, well_id = "full"),
      "oneFullOnePartEmpty.xlsx: Not OK; Plate names cannot be the same as variable 'well_id'"
    )
  })

  test_that("check_plate throws error when plate names are either empty or not unique", {
    csv_file_1 <- paste0(path, "sameNames.csv")
    xlsx_file_1 <- paste0(path, "sameNames.xlsx")
    csv_file_2 <- paste0(path, "noName.csv")
    xlsx_file_2 <- paste0(path, "noName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "sameNames.csv: Not OK; Verify each plate has a unique name."
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "sameNames.xlsx: Not OK; Verify each plate has a unique name."
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "noName.csv: Not OK; Verify each plate has a unique name."
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "noName.xlsx: Not OK; Verify each plate has a unique name."
    )
  })

  test_that("check_plate throws error when row and column ids are empty or bad", {
    csv_file_1 <- paste0(path, "badColRowName.csv")
    xlsx_file_1 <- paste0(path, "badColRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyColRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyColRowName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "badColRowName.csv: Not OK; Verify row and column names."
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "badColRowName.xlsx: Not OK; Verify row and column names."
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "emptyColRowName.csv: Not OK; Verify row and column names."
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "emptyColRowName.xlsx: Not OK; Verify row and column names."
    )
  })

  test_that("check_plate throws error when row id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badRowName.csv")
    xlsx_file_1 <- paste0(path, "badRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyRowName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "badRowName.csv: Not OK; Verify row names."
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "badRowName.xlsx: Not OK; Verify row names."
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "emptyRowName.csv: Not OK; Verify row names."
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "emptyRowName.xlsx: Not OK; Verify row names."
    )
  })

  test_that("check_plate throws error when column id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badColName.csv")
    xlsx_file_1 <- paste0(path, "badColName.xlsx")
    csv_file_2 <- paste0(path, "emptyColName.csv")
    xlsx_file_2 <- paste0(path, "emptyColName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "badColName.csv: Not OK; Verify column names."
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "badColName.xlsx: Not OK; Verify column names."
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "emptyColName.csv: Not OK; Verify column names."
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "emptyColName.xlsx: Not OK; Verify column names."
    )
  })
}

test_that("check_plate throws an error when user inputs bad file type", {
  expect_error(check_plate(file = 1), "file argument cannot be of type: double")
  expect_error(check_plate(file = 1L), "file argument cannot be of type: integer")
  expect_error(check_plate(file = TRUE), "file argument cannot be of type: logical")
})

test_that("check_plate throws an error when user inputs more than one file", {
  file_1 <- "test_data/allWellIds.csv"
  file_2 <- "test_data/oneFullOnePartEmpty.csv"

  expect_error(
    check_plate(file = c(file_1, file_2)),
    "More than one file provided. Only one file should be provided"
  )
})

test_that("check_plate throws an error when `well_id` argument not a character vector of length 1", {
  file <- "test_data/allWellIds.csv"

  expect_error(check_plate(file, well_id = 1), "well_id should be a character vector of length 1")
  expect_error(check_plate(file, well_id = 1L), "well_id should be a character vector of length 1")
  expect_error(check_plate(file, well_id = c("xxx", "yyy")), "well_id should be a character vector of length 1")
  expect_error(check_plate(file, well_id = TRUE), "well_id should be a character vector of length 1")
})

# # Test for filenames extraction

test_that("check_plate throws an error when input file is non existant", {
  csv_file <- "test_data/doesNotExist.csv"
  xlsx_file <- "test_data/doesNotExist.xlsx"

  expect_error(check_plate(csv_file), "doesNotExist.csv: Not OK; File does not exist.")
  expect_error(check_plate(xlsx_file), "doesNotExist.xlsx: Not OK; File does not exist.")
})

test_that("check_plate throws an error when input file is not csv or xlsx", {
  file <- "test_data/badFileType.txt"

  expect_error(check_plate(file), "badFileType.txt: Not OK; Must be either xlsx or csv file.")
})

test_that("check_plate throws an error when input file is empty", {
  csv_file <- "test_data/emptyFile.csv"

  expect_error(check_plate(csv_file), "emptyFile.csv: Not OK; File is empty.")
})
