
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyplate

<!-- badges: start -->
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

`tidyplate` package has two functions:

- `tidy_plate`: This function takes the input file (xlsx or csv) and
  transforms into a tidy dataframe.
- `check_plate`: This function checks whether the input file is valid
  for use with `tidy_plate()` function.

## Installation

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
- Column names should be: 1, 2, 3,…
- Row names should be: A, B, C, …
- There must be an empty row between each plate.

## Example

This is an example which shows you how to use the `tidyplate` package.
If the input file is an xlsx file it reads the first sheet by default.
Users can specify sheet using the `sheet` argument for an xlsx file.

First check if the input file is valid or not:

``` r
library(tidyplate)
check_plate("inst/extdata/example_12_well.xlsx")
#> example_12_well.xlsx: OK; Plate type: 12 well
```

Import the file as a tidy dataframe:

``` r
data <- tidy_plate("inst/extdata/example_12_well.xlsx")
#> Data: example_12_well.xlsx
#> Plate type: 12 well plate
head(data)
#> # A tibble: 6 × 4
#>   well  drug      cell_line percent_survived
#>   <chr> <chr>     <chr>                <int>
#> 1 A1    Neomycin  HEK293                  60
#> 2 A2    Puromycin HEK293                  22
#> 3 A3    Neomycin  Hela                    52
#> 4 A4    Puromycin Hela                    18
#> 5 B1    Neomycin  HEK293                  62
#> 6 B2    Puromycin HEK293                  23
```

If you want to import multiple files you can first check whether the
formats for each file is good or not:

``` r
file_list <- list.files(".", pattern='*.xlsx', recursive = T)
purrr::walk(file_list, purrr::possibly(tidyplate::check_plate, quiet = F))
#> Error: bad_empty.xlsx: Not OK; File is empty.
#> Error: bad_example_12_well.xlsx: Not OK; Verify row & column names.
#> example_12_well.xlsx: OK; Plate type: 12 well
#> example_1536_well.xlsx: OK; Plate type: 1536 well
#> example_24_well.xlsx: OK; Plate type: 24 well
#> example_384_well.xlsx: OK; Plate type: 384 well
#> example_48_well.xlsx: OK; Plate type: 48 well
#> example_6_well.xlsx: OK; Plate type: 6 well
#> example_96_NA.xlsx: OK; Plate type: 96 well
#> example_96_well.xlsx: OK; Plate type: 96 well
```

As you can see that the first two files threw error messages. They can
be removed before importing. Now you can import them using the
`tidy_plate()` function.

``` r
imported_list <- purrr::map(file_list, purrr::possibly(tidyplate::tidy_plate, quiet = F))
#> Error: bad_empty.xlsx is empty. Please verify input file.
#> Error: Verify row names and column names in bad_example_12_well.xlsx.
#> Data: example_12_well.xlsx
#> Plate type: 12 well plate
#> Data: example_1536_well.xlsx
#> Plate type: 1536 well plate
#> Data: example_24_well.xlsx
#> Plate type: 24 well plate
#> Data: example_384_well.xlsx
#> Plate type: 384 well plate
#> Data: example_48_well.xlsx
#> Plate type: 48 well plate
#> Data: example_6_well.xlsx
#> Plate type: 6 well plate
#> Data: example_96_NA.xlsx
#> Plate type: 96 well plate
#> Data: example_96_well.xlsx
#> Plate type: 96 well plate
```

If a file had an error during import their corresponding list object
will be empty.

``` r
imported_list[[1]]     # Empty since there was an error during import
#> NULL
imported_list[[3]] |>  # No errors;imported as a tibble
  head()
#> # A tibble: 6 × 4
#>   well  drug      cell_line percent_survived
#>   <chr> <chr>     <chr>                <int>
#> 1 A1    Neomycin  HEK293                  60
#> 2 A2    Puromycin HEK293                  22
#> 3 A3    Neomycin  Hela                    52
#> 4 A4    Puromycin Hela                    18
#> 5 B1    Neomycin  HEK293                  62
#> 6 B2    Puromycin HEK293                  23
```
