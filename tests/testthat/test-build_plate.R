# Define temporary paths for file outputs
temp_csv <- tempfile(fileext = ".csv")
temp_xlsx <- tempfile(fileext = ".xlsx")
temp_null <- tempfile(fileext = "")
temp_wrong <- tempfile(fileext = ".txt")

for (i in c(6, 12, 24, 48, 96, 384, 1536)) {

  # Test for CSV output
  build_plate(plate_type = i,
              n_plates = 3,
              #file_type = "csv",
              plate_names = NULL,
              file = temp_csv)
  expect_true(file.exists(temp_csv))

  build_plate(plate_type = i,
              n_plates = 3,
              #file_type = "xlsx",
              plate_names = NULL,
              file = temp_xlsx)
  expect_true(file.exists(temp_xlsx))

  # Test for XLSX output
  build_plate(plate_type = i,
              n_plates = 3,
              #file_type = "xlsx",
              plate_names = NULL,
              file = temp_xlsx)
  expect_true(file.exists(temp_xlsx))

  test_that("Created plates are empty", {
    expect_error(
      tidy_plate(temp_csv), "Plate(s) are empty.", fixed = TRUE)
    expect_error(
      tidy_plate(temp_xlsx), "Plate(s) are empty.", fixed = TRUE)
  })
}

test_that("Throws error when plate_type argument is bad", {
  expect_error(
    build_plate(plate_type = "x"),
    regexp = "Invalid `plate_type` provided")
  expect_error(
    build_plate(plate_type = TRUE),
    regexp = "Invalid `plate_type` provided")
  expect_error(
    build_plate(plate_type = 2.4),
    regexp = "Invalid `plate_type` provided")
})

test_that("Deprecation message", {
  expect_warning(
    build_plate(plate_type = 6L, n_plates = 2, plate_names = c("alpha", "beta"), file = temp_csv, file_type = "csv")
    )
})

test_that("Throws error when n_plates value is invalid", {
  expect_error(
    build_plate(plate_type = 6, n_plates = 0),
    "Invalid `n_plates` value provided.")
  expect_error(
    build_plate(plate_type = 6, n_plates = 1.5),
    "Invalid `n_plates` value provided.")
  expect_error(
    build_plate(plate_type = 6, n_plates = TRUE),
    "Invalid `n_plates` value provided.")
})

test_that("Throws error when n_plate & plate_names argument don't match", {
  expect_error(
    build_plate(plate_type = 6L, n_plates = 2, plate_names = c("alpha")),
    "The length of `plate_names` must match `n_plates`.")
})

test_that("Throws error if duplicate `plate_names`", {
  expect_error(
    build_plate(plate_type = 6L, n_plates = 2, plate_names = c("alpha", "alpha")),
    "`plate_names` cannot have duplicates")
})

test_that("plate_names and file values are invalid", {
  expect_error(
    build_plate(plate_type = 6, n_plates = 2, plate_names = c(1, 2)),
    "`plate_names` must be a character vector.")
  expect_error(
    build_plate(plate_type = 6, n_plates = 1, plate_names = "1", file = 1),
    "`file` must be a character string.")
})

test_that("when no `file` provided", {
  expect_silent(
    build_plate(plate_type = 6, n_plates = 2, plate_names = c("alpha", "beta"))
    )
  expect_silent(
    build_plate(plate_type = 6, n_plates = 2, plate_names = c("alpha", "beta"), file = temp_null)
  )
})

test_that("Wrong file extension provided", {
  expect_error(
    build_plate(plate_type = 6, n_plates = 2, plate_names = c("alpha", "beta"), file = temp_wrong)
  )
})

# Set up cleanup to remove files after test
on.exit({
  if (file.exists(temp_csv)) unlink(temp_csv)
  if (file.exists(temp_xlsx)) unlink(temp_xlsx)
  if (file.exists(temp_null)) unlink(temp_null)
  if (file.exists(temp_wrong)) unlink(temp_wrong)
})
