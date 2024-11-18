# tidyplate 2.2.0

# tidyplate 2.1.0

### New features
-   `generate_plate()`: Exports a tibble into plate-shaped csv or xlsx file.

### Major changes
-   Soft deprecation of `file_type` argument in `build_plate()` and replaced
with `file`.
-   `view_plate_names()` returns plate names even when the plate names are not
valid with a warning.

### Minor changes
-   Improved error and warning messages.
-   Polished README and vignettes.
-   Added description field for each function.
-   Added more tests.

# tidyplate 2.0.1

### Minor changes
-   Exported `build_plate()` to NAMESPACE.

# tidyplate 2.0.0

### New features
-   `build_plate()`: Generate plate templates.

### Major changes
-   All functions are rewritten to improve speed and efficiency.

### Minor changes
-   Added more examples in vignette and README.
-   Polished Roxygen documentation.

# tidyplate 1.1.0

### Minor changes
-   Removed tidyplate warnings.
-   `well_id` column will now have leading zeros.

### Bug fixes
-   Fixed double file extension printing in output messages.

# tidyplate 1.0.0

### Minor changes
-   Added quotes around package name in DESCRIPTION.
-   Removed unnecessary files.

# tidyplate 0.9.0

### Features
-   `tidy_plate()`: Convert microwell plate data into tibbles.
-   `check_plate()`: Verify errors in input file.
-   `view_plate_names()`: Get individual plate names in data.
