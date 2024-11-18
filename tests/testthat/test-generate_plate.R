# Define temporary paths for file outputs
temp_csv <- tempfile(fileext = ".csv")
temp_xlsx <- tempfile(fileext = ".xlsx")

df <- tibble::tibble(
  well = c("A01", "A02", "A03", "B01", "B02"),
  values = c(10, 15, 20, 25, 30)
)

# Setup test fixtures to create temporary files
test_that("generate_plate creates temporary files", {
  # Define a sample tibble
  df <- tibble::tibble(well_id = c("A01", "B02"), value = c(10, 20))

  # Mock plate_dims list for testing
  plate_dims <- list("96" = c(8, 12))

  # Test for CSV output
  generate_plate(df, well_id = "well_id", plate_type = 96, file = temp_csv)
  expect_true(file.exists(temp_csv))

  # Test for XLSX output
  generate_plate(df, well_id = "well_id", plate_type = 96, file = temp_xlsx)
  expect_true(file.exists(temp_xlsx))
})

test_that("generate_plate converts data frame to tibble if necessary", {

    # Define a sample data frame
    df <- data.frame(
      well_id = c("A01", "A02", "A03", "B01", "B02"),
      values = c(10, 15, 20, 25, 30)
    )

    # Define a mock plate_dims list for the test (since plate_dims needs to exist)
    plate_dims <- list("96" = c(8, 12), "1536" = c(32, 48))

    # Run the function with the data frame as input
    expect_silent(generate_plate(df, well_id = "well_id", plate_type = 96, file = temp_csv))
  })

test_that("generate_plate transforms plate data for multiple plate types", {
  # Define plate types and corresponding file names in the package
  plate_types <- c(6, 12, 24, 48, 96, 384, 1536)

  # Iterate over each plate type
  for (plate_type in plate_types) {
    path_csv <- system.file("extdata", paste0("example_", plate_type, "_well.csv"), package = "tidyplate")
    plate_csv <- suppressMessages(tidy_plate(path_csv))
    generate_plate(plate_csv, "well", plate_type, temp_csv)
    re_tbl_csv <- suppressMessages(tidy_plate(temp_csv))

    path_xl <- system.file("extdata", paste0("example_", plate_type, "_well.xlsx"), package = "tidyplate")
    plate_xl <- suppressMessages(tidy_plate(path_xl))
    generate_plate(plate_xl, "well", plate_type, temp_xlsx)
    re_tbl_xl <- suppressMessages(tidy_plate(temp_xlsx))

    expect_identical(plate_csv, re_tbl_csv)
    expect_identical(plate_xl, re_tbl_xl, tolerance = 2)
  }
})

test_that("generate_plate requires plate_type to be 1536 for AA-AF wells", {
    df_1536 <- data.frame(
      well_id = c("AA01", "AB02", "AC03", "AD04", "AE05", "AF06"),
      values = c(5, 10, 15, 20, 25, 30)
    )
    # Check that an error is raised if plate_type is not 1536
    expect_error(
      generate_plate(df_1536, well_id = "well_id", plate_type = 96, file = temp_csv),
      regexp = "`well_id` column contains wells labeled 'AA' to"
    )

    expect_silent(
      generate_plate(df_1536, well_id = "well_id", plate_type = 1536, file = temp_csv)
    )
  })

  test_that("generate_plate throws an error if x is not a tibble/data frame", {
    expect_error(generate_plate(x = 1, well_id = "well", plate_type = 96, file = temp_csv),
                 "`x` must be a tibble or data frame.")
  })

  test_that("generate_plate throws an error if well_id is not character or integer", {
    expect_error(generate_plate(df, well_id = list("well"), plate_type = 96, file = temp_csv),
                 "`well_id` must be a character string or an integer")
  })

  test_that("generate_plate throws an error if well_id column does not exist", {
    expect_error(generate_plate(df, well_id = "nonexistent", plate_type = 96, file = temp_csv),
                 "`well_id` must refer to an existing column in")
    expect_error(generate_plate(df, well_id = 20L, plate_type = 96, file = temp_csv),
                 "`well_id` integer must refer to a valid column")
  })

  test_that("generate_plate throws an error if well_id integer index is out of bounds", {
    expect_error(generate_plate(df, well_id = 3, plate_type = 96, file = temp_csv),
                 regexp = "`well_id` must be a character string or an integer indicating the column")
  })

  test_that("generate_plate throws an error if well_id column has invalid well format", {
    df <- tibble::tibble(well = c("Z99", "A00"), value = c(10, 20))
    expect_error(generate_plate(df, well_id = "well", plate_type = 96, file = temp_csv),
                 regexp = "`well_id` column does not contain valid well identifiers")
  })

  test_that("generate_plate throws an error if plate_type is not valid", {
    expect_error(generate_plate(df, well_id = "well", plate_type = 100, file = temp_csv),
                 "`plate_type` must be one of 6, 12, 24, 48, 96, 384, or 1536.")
  })

  test_that("generate_plate throws an error for unsupported file types", {
    expect_error(generate_plate(df, well_id = "well", plate_type = 96, file = "file.txt"),
                 "`file` extension must be 'csv' or 'xlsx'.")
  })

  test_that("generate_plate correctly exports to CSV with valid inputs", {
    df <- tibble::tibble(well = c("A01", "A02"), value = c(10, 20))

    # Run the function
    generate_plate(df, well_id = "well", plate_type = 96, file = temp_csv)

    # Check if file exists and has content
    expect_true(file.exists(temp_csv))
    generated_data <- read.csv(temp_csv, header = FALSE)
    expect_equal(ncol(generated_data), 13)  # 12 columns for plate + 1 header
    expect_equal(nrow(generated_data), 9)   # 8 rows for plate + 1 header
  })

  test_that("generate_plate correctly exports to XLSX with valid inputs", {
    df <- tibble::tibble(well = c("A01", "A02"), value = c(10, 20))

    # Run the function
    generate_plate(df, well_id = "well", plate_type = 96, file = temp_xlsx)

    # Check if file exists and has content
    expect_true(file.exists(temp_xlsx))
    generated_data <- openxlsx::read.xlsx(temp_xlsx, colNames = FALSE)
    expect_equal(ncol(generated_data), 13)  # 12 columns for plate + 1 header
    expect_equal(nrow(generated_data), 9)   # 8 rows for plate + 1 header
  })

# Set up cleanup to remove files after test
on.exit({
  if (file.exists(temp_csv)) unlink(temp_csv)
  if (file.exists(temp_xlsx)) unlink(temp_xlsx)
})
