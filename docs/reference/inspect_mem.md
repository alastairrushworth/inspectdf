# Summary and comparison of memory usage of data frame columns

For a single data frame, summarise the memory usage in each column. If
two data frames are supplied, compare memory usage for columns appearing
in both data frames. For grouped data frames, summarise the memory usage
separately for each group.

## Usage

``` r
inspect_mem(df1, df2 = NULL)
```

## Arguments

- df1:

  A data frame.

- df2:

  An optional second data frame with which to compare memory usage.
  Defaults to `NULL`.

## Value

A tibble summarising and comparing the columnwise memory usage for one
or a pair of data frames.

## Details

For a **single data frame**, the tibble returned contains the columns:\

- `col_name`, a character vector containing column names of `df1`.

- `bytes`, integer vector containing the number of bytes in each column
  of `df1`.

- `size`, a character vector containing display-friendly memory usage of
  each column.

- `pcnt`, the percentage of the data frame's total memory footprint used
  by each column.

For a **pair of data frames**, the tibble returned contains the
columns:\

- `col_name`, a character vector containing column names of `df1` and
  `df2`.

- `size_1`, `size_2`, a character vector containing memory usage of each
  column in each of `df1` and `df2`.

- `pcnt_1`, `pcnt_2`, the percentage of total memory usage of each
  column within each of `df1` and `df2`.

For a **grouped data frame**, the tibble returned is as for a single
data frame, but where the first `k` columns are the grouping columns.
There will be as many rows in the result as there are unique
combinations of the grouping variables.

## See also

[`show_plot`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)

## Author

Alastair Rushworth

## Examples

``` r
# Load dplyr for starwars data & pipe
library(dplyr)

# Single data frame summary
inspect_mem(starwars)
#> # A tibble: 14 × 4
#>    col_name   bytes size        pcnt
#>    <chr>      <int> <chr>      <dbl>
#>  1 films      20008 19.54 Kb  36.0  
#>  2 starships   7424 7.25 Kb   13.4  
#>  3 name        6280 6.13 Kb   11.3  
#>  4 vehicles    5944 5.8 Kb    10.7  
#>  5 homeworld   3608 3.52 Kb    6.49 
#>  6 species     2952 2.88 Kb    5.31 
#>  7 skin_color  2656 2.59 Kb    4.78 
#>  8 eye_color   1608 1.57 Kb    2.89 
#>  9 hair_color  1384 1.35 Kb    2.49 
#> 10 sex          976 976 bytes  1.76 
#> 11 gender       872 872 bytes  1.57 
#> 12 mass         744 744 bytes  1.34 
#> 13 birth_year   744 744 bytes  1.34 
#> 14 height       400 400 bytes  0.719

# Paired data frame comparison
inspect_mem(starwars, starwars[1:20, ])
#> # A tibble: 14 × 5
#>    col_name   size_1    size_2    pcnt_1 pcnt_2
#>    <chr>      <chr>     <chr>      <dbl>  <dbl>
#>  1 films      19.54 Kb  7.23 Kb   36.0   40.5  
#>  2 starships  7.25 Kb   2.58 Kb   13.4   14.4  
#>  3 name       6.13 Kb   1.49 Kb   11.3    8.35 
#>  4 vehicles   5.8 Kb    1.74 Kb   10.7    9.75 
#>  5 homeworld  3.52 Kb   816 bytes  6.49   4.46 
#>  6 species    2.88 Kb   552 bytes  5.31   3.02 
#>  7 skin_color 2.59 Kb   808 bytes  4.78   4.41 
#>  8 eye_color  1.57 Kb   664 bytes  2.89   3.63 
#>  9 hair_color 1.35 Kb   736 bytes  2.49   4.02 
#> 10 sex        976 bytes 440 bytes  1.76   2.40 
#> 11 gender     872 bytes 336 bytes  1.57   1.84 
#> 12 mass       744 bytes 208 bytes  1.34   1.14 
#> 13 birth_year 744 bytes 208 bytes  1.34   1.14 
#> 14 height     400 bytes 176 bytes  0.719  0.962

# Grouped data frame summary
starwars %>% group_by(gender) %>% inspect_mem()
#> # A tibble: 39 × 5
#> # Groups:   gender [3]
#>    gender    col_name   bytes size       pcnt
#>    <chr>     <chr>      <int> <chr>     <dbl>
#>  1 masculine films      15592 15.23 Kb  36.0 
#>  2 masculine starships   5856 5.72 Kb   13.5 
#>  3 masculine name        4776 4.66 Kb   11.0 
#>  4 masculine vehicles    4592 4.48 Kb   10.6 
#>  5 masculine homeworld   3088 3.02 Kb    7.13
#>  6 masculine species     2536 2.48 Kb    5.86
#>  7 masculine skin_color  2208 2.16 Kb    5.10
#>  8 masculine eye_color   1320 1.29 Kb    3.05
#>  9 masculine hair_color  1104 1.08 Kb    2.55
#> 10 masculine sex          752 752 bytes  1.74
#> # ℹ 29 more rows
```
