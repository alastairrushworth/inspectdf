# Summary and comparison of the most common levels in categorical columns

For a single data frame, summarise the most common level in each
categorical column. If two data frames are supplied, compare the most
common levels of categorical features appearing in both data frames. For
grouped data frames, summarise the levels of categorical columns in the
data frame split by group.

## Usage

``` r
inspect_imb(df1, df2 = NULL, include_na = FALSE)
```

## Arguments

- df1:

  A data frame.

- df2:

  An optional second data frame for comparing columnwise imbalance.
  Defaults to `NULL`.

- include_na:

  Logical flag, whether to include missing values as a unique level.
  Default is `FALSE` - to ignore `NA` values.

## Value

A tibble summarising and comparing the imbalance for each categorical
column in one or a pair of data frames.

## Details

For a **single data frame**, the tibble returned contains the columns:  

- `col_name`, a character vector containing column names of `df1`.

- `value`, a character vector containing the most common categorical
  level in each column of `df1`.

- `pcnt`, the relative frequency of each column's most common
  categorical level expressed as a percentage.

- `cnt`, the number of occurrences of the most common categorical level
  in each column of `df1`.

For a **pair of data frames**, the tibble returned contains the
columns:  

- `col_name`, a character vector containing names of the unique columns
  in `df1` and `df2`.

- `value`, a character vector containing the most common categorical
  level in each column of `df1`.

- `pcnt_1`, `pcnt_2`, the percentage occurrence of `value` in the column
  `col_name` for each of `df1` and `df2`, respectively.

- `cnt_1`, `cnt_2`, the number of occurrences of of `value` in the
  column `col_name` for each of `df1` and `df2`, respectively.

- `p_value`, p-value associated with the null hypothesis that the true
  rate of occurrence is the same for both data frames. Small values
  indicate stronger evidence of a difference in the rate of occurrence.

For a **grouped data frame**, the tibble returned is as for a single
data frame, but where the first `k` columns are the grouping columns.
There will be as many rows in the result as there are unique
combinations of the grouping variables.

## See also

[`inspect_cat`](https://alastairrushworth.com/inspectdf/reference/inspect_cat.md),
[`show_plot`](https://alastairrushworth.com/inspectdf/reference/show_plot.md)

## Author

Alastair Rushworth

## Examples

``` r
# Load dplyr for starwars data & pipe
library(dplyr)

# Single data frame summary
inspect_imb(starwars)
#> # A tibble: 8 × 4
#>   col_name   value      pcnt   cnt
#>   <chr>      <chr>     <dbl> <int>
#> 1 gender     masculine 75.9     66
#> 2 sex        male      69.0     60
#> 3 hair_color none      43.7     38
#> 4 species    Human     40.2     35
#> 5 eye_color  brown     24.1     21
#> 6 skin_color fair      19.5     17
#> 7 homeworld  Naboo     12.6     11
#> 8 name       Ackbar     1.15     1

# Paired data frame comparison
inspect_imb(starwars, starwars[1:20, ])
#> # A tibble: 8 × 7
#>   col_name   value     pcnt_1 cnt_1 pcnt_2 cnt_2 p_value
#>   <chr>      <chr>      <dbl> <int>  <dbl> <int>   <dbl>
#> 1 gender     masculine  75.9     66     85    17   0.558
#> 2 sex        male       69.0     60     65    13   0.939
#> 3 hair_color none       43.7     38     NA    NA  NA    
#> 4 species    Human      40.2     35     60    12   0.175
#> 5 eye_color  brown      24.1     21     NA    NA  NA    
#> 6 skin_color fair       19.5     17     35     7   0.231
#> 7 homeworld  Naboo      12.6     11     NA    NA  NA    
#> 8 name       Ackbar      1.15     1     NA    NA  NA    

# Grouped data frame summary
starwars %>% group_by(gender) %>% inspect_imb()
#> # A tibble: 21 × 5
#> # Groups:   gender [3]
#>    gender    col_name   value     pcnt   cnt
#>    <chr>     <chr>      <chr>    <dbl> <int>
#>  1 masculine sex        male     90.9     60
#>  2 masculine hair_color none     47.0     31
#>  3 masculine species    Human    39.4     26
#>  4 masculine eye_color  brown    22.7     15
#>  5 masculine skin_color fair     19.7     13
#>  6 masculine homeworld  Tatooine 12.1      8
#>  7 masculine name       Ackbar    1.52     1
#>  8 feminine  sex        female   94.1     16
#>  9 feminine  species    Human    52.9      9
#> 10 feminine  hair_color none     35.3      6
#> # ℹ 11 more rows
```
