# Summary and comparison of the rate of missingness across data frame columns

For a single data frame, summarise the rate of missingness in each
column. If two data frames are supplied, compare missingness for columns
appearing in both data frames. For grouped data frames, summarise the
rate of missingness separately for each group.

## Usage

``` r
inspect_na(df1, df2 = NULL)
```

## Arguments

- df1:

  A data frame

- df2:

  An optional second data frame for making columnwise comparison of
  missingness. Defaults to `NULL`.

## Value

A tibble summarising the count and percentage of columnwise missingness
for one or a pair of data frames.

## Details

For a **single data frame**, the tibble returned contains the columns:  

- `col_name`, a character vector containing column names of `df1`.

- `cnt`, an integer vector containing the number of missing values by
  column.

- `pcnt`, the percentage of records in each columns that is missing.

For a **pair of data frames**, the tibble returned contains the
columns:  

- `col_name`, the name of the columns occurring in either `df1` or
  `df2`.

- `cnt_1`, `cnt_2`, a pair of integer vectors containing counts of
  missing entries for each column in `df1` and `df2`.

- `pcnt_1`, `pcnt_2`, a pair of columns containing percentage of missing
  entries for each column in `df1` and `df2`.

- `p_value`, the p-value associated with test of equivalence of rates of
  missingness. Small values indicate evidence that the rate of
  missingness differs for a column occurring in both `df1` and `df2`.

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
inspect_na(starwars)
#> # A tibble: 14 × 3
#>    col_name     cnt  pcnt
#>    <chr>      <int> <dbl>
#>  1 birth_year    44 50.6 
#>  2 mass          28 32.2 
#>  3 homeworld     10 11.5 
#>  4 height         6  6.90
#>  5 hair_color     5  5.75
#>  6 sex            4  4.60
#>  7 gender         4  4.60
#>  8 species        4  4.60
#>  9 name           0  0   
#> 10 skin_color     0  0   
#> 11 eye_color      0  0   
#> 12 films          0  0   
#> 13 vehicles       0  0   
#> 14 starships      0  0   

# Paired data frame comparison
inspect_na(starwars, starwars[1:20, ])
#> # A tibble: 14 × 6
#>    col_name   cnt_1 pcnt_1 cnt_2 pcnt_2  p_value
#>    <chr>      <int>  <dbl> <int>  <dbl>    <dbl>
#>  1 birth_year    44  50.6      2     10  0.00225
#>  2 mass          28  32.2      1      5  0.0287 
#>  3 homeworld     10  11.5      1      5  0.650  
#>  4 height         6   6.90     0      0  0.503  
#>  5 hair_color     5   5.75     5     25  0.0250 
#>  6 sex            4   4.60     1      5  1.00   
#>  7 gender         4   4.60     1      5  1.00   
#>  8 species        4   4.60     1      5  1.00   
#>  9 name           0   0        0      0 NA      
#> 10 skin_color     0   0        0      0 NA      
#> 11 eye_color      0   0        0      0 NA      
#> 12 films          0   0        0      0 NA      
#> 13 vehicles       0   0        0      0 NA      
#> 14 starships      0   0        0      0 NA      

# Grouped data frame summary
starwars %>% group_by(gender) %>% inspect_na()
#> # A tibble: 39 × 4
#> # Groups:   gender [3]
#>    gender    col_name     cnt  pcnt
#>    <chr>     <chr>      <int> <dbl>
#>  1 masculine birth_year    31 47.0 
#>  2 masculine mass          19 28.8 
#>  3 masculine homeworld      7 10.6 
#>  4 masculine hair_color     5  7.58
#>  5 masculine height         4  6.06
#>  6 masculine name           0  0   
#>  7 masculine skin_color     0  0   
#>  8 masculine eye_color      0  0   
#>  9 masculine sex            0  0   
#> 10 masculine species        0  0   
#> # ℹ 29 more rows
```
