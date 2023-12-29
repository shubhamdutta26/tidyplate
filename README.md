
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyplate

<!-- badges: start -->
<!-- badges: end -->

The goal of tidyplate is to convert different sizes of plates used in
scientific studies to a tidy dataframe or tibble. tidyplate accepts xlsx
and csv files formatted in a specific way as input. tidyplate supports
6-well, 12-well, 24-well, 48-well, 96-well, 384-well, and 1536-well
plates.

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

This is an example which shows you how to use the `tidy_plate` function.
If the input file is an xlsx file it reads the first sheet by default.
Users can specify sheet using the `sheet` argument.

``` r
library(tidyplate)
data <- tidy_plate("inst/extdata/example_12_well.xlsx")
#> [1] "Plate type: 12 well plate"
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
