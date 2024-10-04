
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyplate

<!-- badges: start -->

[![R-CMD-check](https://github.com/shubhamdutta26/tidyplate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shubhamdutta26/tidyplate/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidyplate)](https://CRAN.R-project.org/package=tidyplate)
[![](http://cranlogs.r-pkg.org/badges/last-month/tidyplate)](https://cran.r-project.org/package=tidyplate)
[![](http://cranlogs.r-pkg.org/badges/grand-total/tidyplate)](https://cran.r-project.org/package=tidyplate)
[![Codecov test
coverage](https://codecov.io/gh/shubhamdutta26/tidyplate/graph/badge.svg)](https://app.codecov.io/gh/shubhamdutta26/tidyplate)

<!-- badges: end -->

Microtiter plates or microplates have become a standard tool in
analytical research and clinical diagnostic testing laboratories. They
are convenient, high-throughput tools for organizing tissue culture, PCR
tests (such as HIV/ COVID screening), or immunological assays such as
ELISA, RIA and FIA. They offer many advantages over traditional assay
formats including reduced sample and reagent volumes, increased
throughput, and ease of automation. The goal of `tidyplate` is to help
researchers convert different types of microplates into tidy dataframes
which can be used in data analysis. `tidyplate` accepts xlsx and csv
files formatted in a specific way as input. `tidyplate` supports all
types of standard microplate formats namely: 6-well, 12-well, 24-well,
48-well, 96-well, 384-well, and 1536-well plates.

`tidyplate` has four functions:

- `tidy_plate`: Transforms the input file (xlsx or csv) into a tidy
  dataframe.
- `check_plate`: Checks whether the input file is valid for use with
  `tidy_plate()`.
- `view_plate_names`: Returns names/ id(s) of each plate in the input
  file.
- `build_plate`: Generates a csv or xlsx template for each plate type.

## Installation

To install tidyplate from CRAN:

``` r
install.packages("tidyplate")
```

You can install the development version of tidyplate from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shubhamdutta26/tidyplate")
```

## Formating the input data file

<div class="figure">

<img src="man/figures/README_tidy_plate.png" alt="This figure demonstrates how to format the 12-well plate input file. Colors are for visualization purposes only." width="100%" />
<p class="caption">
This figure demonstrates how to format the 12-well plate input file.
Colors are for visualization purposes only.
</p>

</div>

The input xlsx or csv should be formatted in a specific way:

- Top left corner must hold the name for that plate.
- Column names should be: 1, 2, 3, and so on and so forth.
- Row names should be: A, B, C, and so on and so forth.
- There must be an empty row between each plate.

## Example

This is an example which shows you how to use the `tidyplate`. If the
input file is an xlsx file it reads the first sheet by default. Users
can specify sheet using the `sheet` argument for an xlsx file. Users can
also specify the variable name of column where well ids will be stored
(defaults to “well”). Please make sure that `well_id` argument does not
match individual plate names in the input file.

First check if the input file is valid or not:

``` r
library(tidyplate)
file <- system.file("extdata", 
                    "example_12_well.xlsx", 
                    package = "tidyplate")
check_plate(file) # No error for valid file
#> example_12_well.xlsx: OK; Plate type: 12-well

incorrect_file <- system.file("extdata",
                              "incorrect_format.csv",
                              package = "tidyplate")
check_plate(incorrect_file) # Error type displayed
#> Error: Verify row and column ids in incorrect_format.csv.
```

As mentioned above, the formatting of the input file is very important.
A csv or excel template for each plate type can be created using the
`build_plate` function:

``` r
build_plate(plate_type = 96, 
            n_plates = 2, 
            file_type = "xlsx") # default is csv
```

If you want to retrieve the names of individual plates:

``` r
view_plate_names(file)
#> [1] "drug"             "cell_line"        "percent_survived"
```

Import the file as a tibble:

``` r
data <- tidy_plate(file)
#> Data: example_12_well.xlsx; Plate type: 12-well plate
head(data)
#> # A tibble: 6 × 4
#>   well  drug      cell_line percent_survived
#>   <chr> <chr>     <chr>                <int>
#> 1 A01   Neomycin  HEK293                  60
#> 2 A02   Puromycin HEK293                  NA
#> 3 A03   Neomycin  Hela                    52
#> 4 A04   Puromycin Hela                    18
#> 5 B01   Neomycin  HEK293                  62
#> 6 B02   Puromycin HEK293                  23
```

Import multiple csv files into separate tibbles:

``` r
csv_files <- list.files(path = file, 
                        pattern = "*.csv",
                        full.names = TRUE)

names <- tools::file_path_sans_ext(basename(csv_files))

# Loop through the filenames and assign data
for(i in seq_along(csv_files)) {
  assign(names[i], tidy_plate(csv_files[i]))
}
```

Import multiple csv files as a **list** of tibbles:

``` r
# Initialize an empty list to store tibbles for each file
tb_csv_list <- list()

# Loop through the filenames and assign data
for(i in seq_along(csv_files)) {
  tb_csv_list[[i]] <- tidy_plate(csv_files[i])
}
```

For multiple excel sheets in the same excel file:

``` r
# as individual tibbles
xl_file <- system.file("extdata", 
                       "multisheet_example.xlsx", 
                       package = "tidyplate")

sheets <- readxl::excel_sheets(xl_file)

for (sheet in sheets) {
  tb <- tidy_plate(xl_file, sheet = sheet)
  name <- paste0("tb_", sheet)
  assign(name, tb)
}
#> Data: multisheet_example.xlsx; Plate type: 6-well plate
#> Data: multisheet_example.xlsx; Plate type: 12-well plate
#> Data: multisheet_example.xlsx; Plate type: 12-well plate

# as elements of a list
# Initialize an empty list to store tibbles for each sheet
tb_xl_list <- list()

for (sheet in sheets) {
  tb_xl_list[[sheet]] <- tidy_plate(xl_file, sheet = sheet)
}
#> Data: multisheet_example.xlsx; Plate type: 6-well plate
#> Data: multisheet_example.xlsx; Plate type: 12-well plate
#> Data: multisheet_example.xlsx; Plate type: 12-well plate
```
