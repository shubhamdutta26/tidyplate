for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
  path <- paste0("test_data/", i, "/")

  test_that("view_plate_names works for complete valid data", {
    csv_file <- paste0(path, "allWellIds.csv")
    xlsx_file <- paste0(path, "allWellIds.xlsx")

    result_csv <- view_plate_names(csv_file)
    result_xlsx <- view_plate_names(xlsx_file)

    expect_identical(result_csv, "values")
    expect_identical(result_xlsx, "values")
  })

  test_that("view_plate_names works for missing data from plate", {
    csv_file <- paste0(path, "wellIdsAndEmptyWells.csv")
    xlsx_file <- paste0(path, "wellIdsAndEmptyWells.xlsx")

    result_csv <- view_plate_names(csv_file)
    result_xlsx <- view_plate_names(xlsx_file)

    expect_identical(result_csv, "values")
    expect_identical(result_xlsx, "values")
  })

  test_that("view_plate_names works with one full plate and one partially empty", {
    csv_file <- paste0(path, "oneFullOnePartEmpty.csv")
    xlsx_file <- paste0(path, "oneFullOnePartEmpty.xlsx")

    result_csv <- view_plate_names(csv_file)
    result_xlsx <- view_plate_names(xlsx_file)

    expect_identical(result_csv, c("full", "partial"))
    expect_identical(result_xlsx, c("full", "partial"))
  })

  test_that("view_plate_names returns a character vector", {
    expect_type(view_plate_names(paste0(path, "allWellIds.csv")), "character")
    expect_type(view_plate_names(paste0(path, "allWellIds.xlsx")), "character")
  })

  test_that("view_plate_names allows weird characters", {
    csv_file <- paste0(path, "weirdCharacters.csv")
    xlsx_file <- paste0(path, "weirdCharacters.xlsx")

    result_csv <- view_plate_names(csv_file)
    result_xlsx <- view_plate_names(xlsx_file)

    expect_identical(result_csv, "values")
    expect_identical(result_xlsx, "values")
  })

  test_that("view_plate_names works with single well", {
    csv_file <- paste0(path, "oneWell.csv")
    xlsx_file <- paste0(path, "oneWell.xlsx")

    result_csv <- view_plate_names(csv_file)
    result_xlsx <- view_plate_names(xlsx_file)

    expect_identical(result_csv, "values")
    expect_identical(result_xlsx, "values")
  })

  test_that("view_plate_names throws error when plate format is not correct", {
    csv_file_1 <- paste0(path, "extraCol.csv")
    csv_file_2 <- paste0(path, "twoPlatesExtraCol.csv")
    xlsx_file_1 <- paste0(path, "extraCol.xlsx")
    xlsx_file_2 <- paste0(path, "twoPlatesExtraCol.xlsx")

    expect_error(
      view_plate_names(csv_file_1),
      "extraCol.csv: Not OK; Verify input data format."
    )
    expect_error(
      view_plate_names(csv_file_2),
      "twoPlatesExtraCol.csv: Not OK; Verify input data format."
    )
    expect_error(
      view_plate_names(xlsx_file_1),
      "extraCol.xlsx: Not OK; Verify input data format."
    )
    expect_error(
      view_plate_names(xlsx_file_2),
      "twoPlatesExtraCol.xlsx: Not OK; Verify input data format."
    )
  })


  test_that("view_plate_names throws error when plate names are either empty or not unique", {
    csv_file_1 <- paste0(path, "sameNames.csv")
    xlsx_file_1 <- paste0(path, "sameNames.xlsx")
    csv_file_2 <- paste0(path, "noName.csv")
    xlsx_file_2 <- paste0(path, "noName.xlsx")

    expect_error(
      view_plate_names(csv_file_1),
      "sameNames.csv: Not OK; Verify each plate has a unique name."
    )
    expect_error(
      view_plate_names(xlsx_file_1),
      "sameNames.xlsx: Not OK; Verify each plate has a unique name."
    )
    expect_error(
      view_plate_names(csv_file_2),
      "noName.csv: Not OK; Verify each plate has a unique name."
    )
    expect_error(
      view_plate_names(xlsx_file_2),
      "noName.xlsx: Not OK; Verify each plate has a unique name."
    )
  })

  test_that("view_plate_names throws error when row and column ids are empty or bad", {
    csv_file_1 <- paste0(path, "badColRowName.csv")
    xlsx_file_1 <- paste0(path, "badColRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyColRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyColRowName.xlsx")

    expect_error(
      view_plate_names(csv_file_1),
      "badColRowName.csv: Not OK; Verify row and column names."
    )
    expect_error(
      view_plate_names(xlsx_file_1),
      "badColRowName.xlsx: Not OK; Verify row and column names."
    )
    expect_error(
      view_plate_names(csv_file_2),
      "emptyColRowName.csv: Not OK; Verify row and column names."
    )
    expect_error(
      view_plate_names(xlsx_file_2),
      "emptyColRowName.xlsx: Not OK; Verify row and column names."
    )
  })

  test_that("view_plate_names throws error when row id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badRowName.csv")
    xlsx_file_1 <- paste0(path, "badRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyRowName.xlsx")

    expect_error(
      view_plate_names(csv_file_1),
      "badRowName.csv: Not OK; Verify row names."
    )
    expect_error(
      view_plate_names(xlsx_file_1),
      "badRowName.xlsx: Not OK; Verify row names."
    )
    expect_error(
      view_plate_names(csv_file_2),
      "emptyRowName.csv: Not OK; Verify row names."
    )
    expect_error(
      view_plate_names(xlsx_file_2),
      "emptyRowName.xlsx: Not OK; Verify row names."
    )
  })

  test_that("view_plate_names throws error when column id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badColName.csv")
    xlsx_file_1 <- paste0(path, "badColName.xlsx")
    csv_file_2 <- paste0(path, "emptyColName.csv")
    xlsx_file_2 <- paste0(path, "emptyColName.xlsx")

    expect_error(
      view_plate_names(csv_file_1),
      "badColName.csv: Not OK; Verify column names."
    )
    expect_error(
      view_plate_names(xlsx_file_1),
      "badColName.xlsx: Not OK; Verify column names."
    )
    expect_error(
      view_plate_names(csv_file_2),
      "emptyColName.csv: Not OK; Verify column names."
    )
    expect_error(
      view_plate_names(xlsx_file_2),
      "emptyColName.xlsx: Not OK; Verify column names."
    )
  })
}

test_that("view_plate_names throws an error when user inputs bad file type", {
  expect_error(view_plate_names(file = 1), "file argument cannot be of type: double")
  expect_error(view_plate_names(file = 1L), "file argument cannot be of type: integer")
  expect_error(view_plate_names(file = TRUE), "file argument cannot be of type: logical")
})

test_that("view_plate_names throws an error when user inputs more than one file", {
  file_1 <- "test_data/allWellIds.csv"
  file_2 <- "test_data/oneFullOnePartEmpty.csv"

  expect_error(
    view_plate_names(file = c(file_1, file_2)),
    "More than one file provided. Only one file should be provided"
  )
})

# # Test for filenames extraction

test_that("view_plate_names throws an error when input file is non existant", {
  csv_file <- "test_data/doesNotExist.csv"
  xlsx_file <- "test_data/doesNotExist.xlsx"

  expect_error(view_plate_names(csv_file), "doesNotExist.csv: Not OK; File does not exist.")
  expect_error(view_plate_names(xlsx_file), "doesNotExist.xlsx: Not OK; File does not exist.")
})

test_that("view_plate_names throws an error when input file is not csv or xlsx", {
  file <- "test_data/badFileType.txt"

  expect_error(view_plate_names(file), "badFileType.txt: Not OK; Must be either xlsx or csv file.")
})

test_that("view_plate_names throws an error when input file is empty", {
  csv_file <- "test_data/emptyFile.csv"

  expect_error(view_plate_names(csv_file), "emptyFile.csv: Not OK; File is empty.")
})
