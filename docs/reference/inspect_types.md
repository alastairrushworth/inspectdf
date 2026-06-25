# Summary and comparison of column types

For a single data frame, summarise the column types. If two data frames
are supplied, compare column type composition of both data frames.

## Usage

``` r
inspect_types(df1, df2 = NULL, compare_index = FALSE)
```

## Arguments

- df1:

  A data frame.

- df2:

  An optional second data frame for comparison.

- compare_index:

  Whether to check column positions as well as types when comparing data
  frames. Defaults to `FALSE`.

## Value

A tibble summarising the count and percentage of different column types
for one or a pair of data frames.

## Details

For a **single data frame**, the tibble returned contains the columns:\

- `type`, a character vector containing the column types in `df1`.

- `cnt`, integer counts of each type.

- `pcnt`, the percentage of all columns with each type.

- `col_name`, the names of columns with each type.\

For a **pair of data frames**, the tibble returned contains the
columns:\

- `type`, a character vector containing the column types in `df1` and
  `df2`.

- `cnt_1`, `cnt_2`, pair of integer columns containing counts of each
  type - in each of `df1` and `df2`

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
inspect_types(starwars)
#> # A tibble: 4 × 4
#>   type        cnt  pcnt col_name    
#>   <chr>     <int> <dbl> <named list>
#> 1 character     8 57.1  <chr [8]>   
#> 2 list          3 21.4  <chr [3]>   
#> 3 numeric       2 14.3  <chr [2]>   
#> 4 integer       1  7.14 <chr [1]>   

# Paired data frame comparison
inspect_types(starwars, starwars[1:20, ])
#> # A tibble: 4 × 6
#>   type      equal cnt_1 cnt_2 columns           issues
#>   <chr>     <chr> <int> <int> <named list>      <list>
#> 1 character ✔         8     8 <tibble [16 × 2]> <NULL>
#> 2 list      ✔         3     3 <tibble [6 × 2]>  <NULL>
#> 3 numeric   ✔         2     2 <tibble [4 × 2]>  <NULL>
#> 4 integer   ✔         1     1 <tibble [2 × 2]>  <NULL>
```
