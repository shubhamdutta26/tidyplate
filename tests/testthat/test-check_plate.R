for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
  path <- paste0("test_data/", i, "/")

  test_that("check_plate guesses the correct plate type", {
    csv_file <- paste0(path, "allWellIds.csv")

    expect_message(check_plate(csv_file), paste0("allWellIds.csv: OK; Plate type: ", i, "-well"))
  })

  test_that("check_plate throws error when plate format is not correct", {
    csv_file_1 <- paste0(path, "extraCol.csv")
    csv_file_2 <- paste0(path, "twoPlatesExtraCol.csv")
    xlsx_file_1 <- paste0(path, "extraCol.xlsx")
    xlsx_file_2 <- paste0(path, "twoPlatesExtraCol.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "extraCol.csv is not a valid input file. Please review an example dataset."
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "twoPlatesExtraCol.csv is not a valid input file. Please review an example dataset."
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "extraCol.xlsx is not a valid input file. Please review an example dataset."
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "twoPlatesExtraCol.xlsx is not a valid input file. Please review an example dataset."
    )
  })

  test_that("check_plate throws error when well_id argument matches plate names", {
    csv_file <- paste0(path, "oneFullOnePartEmpty.csv")
    xlsx_file <- paste0(path, "oneFullOnePartEmpty.xlsx")

    expect_error(
      check_plate(csv_file, well_id = "full"),
      "Plate names cannot be the same as argument `well_id`."
    )
    expect_error(
      check_plate(xlsx_file, well_id = "full"),
      "Plate names cannot be the same as argument `well_id`."
    )
  })

  test_that("check_plate throws error when plate names are either empty or not unique", {
    csv_file_1 <- paste0(path, "sameNames.csv")
    xlsx_file_1 <- paste0(path, "sameNames.xlsx")
    csv_file_2 <- paste0(path, "noName.csv")
    xlsx_file_2 <- paste0(path, "noName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "Verify that each plate in sameNames.csv has a unique name."
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "Verify that each plate in sameNames.xlsx has a unique name."
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "Verify that each plate in noName.csv has a unique name."
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "Verify that each plate in noName.xlsx has a unique name."
    )
  })

  test_that("check_plate throws error when row and column ids are empty or bad", {
    csv_file_1 <- paste0(path, "badColRowName.csv")
    xlsx_file_1 <- paste0(path, "badColRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyColRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyColRowName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "Verify row and column ids in badColRowName.csv.", fixed = TRUE
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "Verify row and column ids in badColRowName.xlsx.", fixed = TRUE
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "Verify row and column ids in emptyColRowName.csv.", fixed = TRUE
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "Verify row and column ids in emptyColRowName.xlsx.", fixed = TRUE
    )
  })

  test_that("check_plate throws error when row id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badRowName.csv")
    xlsx_file_1 <- paste0(path, "badRowName.xlsx")
    csv_file_2 <- paste0(path, "emptyRowName.csv")
    xlsx_file_2 <- paste0(path, "emptyRowName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "Verify row id(s) in badRowName.csv.", fixed = TRUE
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "Verify row id(s) in badRowName.xlsx.", fixed = TRUE
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "Verify row id(s) in emptyRowName.csv.", fixed = TRUE
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "Verify row id(s) in emptyRowName.xlsx.", fixed = TRUE
    )
  })

  test_that("check_plate throws error when column id(s) are empty or bad", {
    csv_file_1 <- paste0(path, "badColName.csv")
    xlsx_file_1 <- paste0(path, "badColName.xlsx")
    csv_file_2 <- paste0(path, "emptyColName.csv")
    xlsx_file_2 <- paste0(path, "emptyColName.xlsx")

    expect_error(
      check_plate(csv_file_1, well_id = "wells"),
      "Verify column id(s) in badColName.csv.", fixed = TRUE
    )
    expect_error(
      check_plate(xlsx_file_1, well_id = "wells"),
      "Verify column id(s) in badColName.xlsx.", fixed = TRUE
    )
    expect_error(
      check_plate(csv_file_2, well_id = "wells"),
      "Verify column id(s) in emptyColName.csv.", fixed = TRUE
    )
    expect_error(
      check_plate(xlsx_file_2, well_id = "wells"),
      "Verify column id(s) in emptyColName.xlsx.", fixed = TRUE
    )
  })
}


test_that("check_plate throws an error when user inputs more than one file", {
  file_1 <- "test_data/allWellIds.csv"
  file_2 <- "test_data/oneFullOnePartEmpty.csv"

  expect_error(
    check_plate(file = c(file_1, file_2)),
    "Invalid input: More than one file provided."
  )
})

test_that("check_plate throws an error when `well_id` argument not a character vector of length 1", {
  file <- "test_data/allWellIds.csv"

  expect_error(check_plate(file, well_id = 1), "`well_id` should be a character vector of length 1")
  expect_error(check_plate(file, well_id = 1L), "`well_id` should be a character vector of length 1")
  expect_error(check_plate(file, well_id = c("xxx", "yyy")), "`well_id` should be a character vector of length 1")
  expect_error(check_plate(file, well_id = TRUE), "`well_id` should be a character vector of length 1")
})

# Test for filenames extraction

test_that("check_plate throws an error when input file is non existant", {
  csv_file <- "test_data/doesNotExist.csv"
  xlsx_file <- "test_data/doesNotExist.xlsx"

  expect_error(check_plate(csv_file), "doesNotExist.csv does not exist!")
  expect_error(check_plate(xlsx_file), "doesNotExist.xlsx does not exist!")
})

test_that("check_plate throws an error when input file is not csv or xlsx", {
  file <- "test_data/badFileType.txt"

  expect_error(check_plate(file), "Unsupported file format. Please use CSV or Excel files.")
})

test_that("check_plate throws an error when input file is empty", {
  csv_file <- "test_data/emptyFile.csv"

  expect_error(check_plate(csv_file), "The input file or sheet is empty.")
})
