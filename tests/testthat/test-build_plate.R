for (i in c(6, 12, 24, 48, 96, 384, 1536)) {

  path_csv <- paste0("test_data/", i, "/build_plate/empty_", i, "-well.csv")
  path_xlsx <- paste0("test_data/", i, "/build_plate/empty_", i, "-well.xlsx")

  test_that("build_plate creates a CSV file", {
    result_file_csv <- build_plate(plate_type = i,
                                   n_plates = 3,
                                   file_type = "csv",
                                   plate_names = NULL,
                                   file = path_csv)
    file_ext_csv <- tolower(tools::file_ext(path_csv))

    result_file_xlsx <- build_plate(plate_type = i,
                                    n_plates = 3,
                                    file_type = "xlsx",
                                    plate_names = NULL,
                                    file = path_xlsx)
    file_ext_xlsx <- tolower(tools::file_ext(path_xlsx))
    expect_true(file.exists(path_csv))
    expect_equal(file_ext_csv, "csv")
    expect_true(file.exists(path_xlsx))
    expect_equal(file_ext_xlsx, "xlsx")
    })

  test_that("Created plates are empty", {
    expect_error(
      tidy_plate(path_csv), "Plate(s) are empty.", fixed = TRUE)
    expect_error(
      tidy_plate(path_xlsx), "Plate(s) are empty.", fixed = TRUE)
  })

  path_bad_csv <- paste0("test_data/", i, "/build_plate/empty_", i, "-well_bad.csv")
  path_bad_xlsx <- paste0("test_data/", i, "/build_plate/empty_", i, "-well_bad.xlsx")

  filename_csv <- paste0("empty_", i, "-well_bad.csv")
  filename_xlsx <- paste0("empty_", i, "-well_bad.xlsx")

  testthat::test_that("Created plates are formatted correctly", {
    testthat::expect_error(
      check_plate(path_bad_csv),
      paste0(filename_csv, " is not a valid input file. Please review an example dataset."))
    testthat::expect_error(
      check_plate(path_bad_xlsx),
      paste0(filename_xlsx, " is not a valid input file. Please review an example dataset."))
  })
}

testthat::test_that("Throws error when plate_type argument is bad", {
  testthat::expect_error(
    build_plate(plate_type = "x"),
    "Invalid `plate_type` provided. `plate_type` must be an integer and one of 6, 12, 24, 48, 96, 384, or 1536.")
  testthat::expect_error(
    build_plate(plate_type = TRUE),
    "Invalid `plate_type` provided. `plate_type` must be an integer and one of 6, 12, 24, 48, 96, 384, or 1536.")
  testthat::expect_error(
    build_plate(plate_type = 2.4),
    "Invalid `plate_type` provided. `plate_type` must be an integer and one of 6, 12, 24, 48, 96, 384, or 1536.")
})

testthat::test_that("Throws error when n_plates value is invalid", {
  testthat::expect_error(
    build_plate(plate_type = 6, n_plates = 0),
    "Invalid `n_plates` value provided.")
  testthat::expect_error(
    build_plate(plate_type = 6, n_plates = 1.5),
    "Invalid `n_plates` value provided.")
  testthat::expect_error(
    build_plate(plate_type = 6, n_plates = TRUE),
    "Invalid `n_plates` value provided.")
})

testthat::test_that("Throws error when n_plate & plate_names argument don't match", {
  testthat::expect_error(
    build_plate(plate_type = 6L, n_plate = 2, plate_names = c("alpha")),
    "The length of `plate_names` must match `n_plates`.")
})

testthat::test_that("plate_names and file values are invalid", {
  testthat::expect_error(
    build_plate(plate_type = 6, n_plates = 2, plate_names = c(1, 2)),
    "`plate_names` must be a character vector.")
  testthat::expect_error(
    build_plate(plate_type = 6, n_plates = 1, plate_names = "1", file = 1),
    "`file` must be a character string.")
})
