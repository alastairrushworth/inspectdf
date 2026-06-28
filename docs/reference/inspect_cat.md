# Summary and comparison of the levels in categorical columns

For a single data frame, summarise the levels of each categorical
column. If two data frames are supplied, compare the levels of
categorical features that appear in both data frames. For grouped data
frames, summarise the levels of categorical features separately for each
group.

## Usage

``` r
inspect_cat(df1, df2 = NULL, include_int = FALSE)
```

## Arguments

- df1:

  A data frame.

- df2:

  An optional second data frame for comparing categorical levels.
  Defaults to `NULL`.

- include_int:

  Logical flag - whether to treat integer columns as categories. Default
  is `FALSE`.

## Value

A tibble summarising or comparing the categorical features in one or a
pair of data frames.

## Details

For a **single data frame**, the tibble returned contains the columns:\

- `col_name`, character vector containing column names of `df1`.

- `cnt` integer column containing count of unique levels found in each
  column, including `NA`.

- `common`, a character column containing the name of the most common
  level.

- `common_pcnt`, the percentage of each column occupied by the most
  common level shown in `common`.

- `levels`, a named list containing relative frequency tibbles for each
  feature.

For a **pair of data frames**, the tibble returned contains the
columns:\

- `col_name`, character vector containing names of columns appearing in
  both `df1` and `df2`.

- `jsd`, a numeric column containing the Jensen-Shannon divergence. This
  measures the difference in relative frequencies of levels in a pair of
  categorical features. Values near to 0 indicate agreement of the
  distributions, while 1 indicates disagreement.

- `pval`, the p-value corresponding to a NHT that the true frequencies
  of the categories are equal. A small p indicates evidence that the the
  two sets of relative frequencies are actually different. The test is
  based on a modified Chi-squared statistic.

- `lvls_1`, `lvls_2`, the relative frequency of levels in each of `df1`
  and `df2`.

For a **grouped data frame**, the tibble returned is as for a single
data frame, but where the first `k` columns are the grouping columns.
There will be as many rows in the result as there are unique
combinations of the grouping variables.

## See also

[`inspect_imb`](https://alastairrushworth.com/inspectdf/reference/inspect_imb.md),
[`show_plot`](https://alastairrushworth.com/inspectdf/reference/show_plot.md)

## Author

Alastair Rushworth

## Examples

``` r
# Load dplyr for starwars data & pipe
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Single data frame summary
inspect_cat(starwars)
#> # A tibble: 8 × 5
#>   col_name     cnt common    common_pcnt levels           
#>   <chr>      <int> <chr>           <dbl> <named list>     
#> 1 eye_color     15 brown           24.1  <tibble [15 × 3]>
#> 2 gender         3 masculine       75.9  <tibble [3 × 3]> 
#> 3 hair_color    12 none            43.7  <tibble [12 × 3]>
#> 4 homeworld     49 Naboo           12.6  <tibble [49 × 3]>
#> 5 name          87 Ackbar           1.15 <tibble [87 × 3]>
#> 6 sex            5 male            69.0  <tibble [5 × 3]> 
#> 7 skin_color    31 fair            19.5  <tibble [31 × 3]>
#> 8 species       38 Human           40.2  <tibble [38 × 3]>

# Paired data frame comparison
inspect_cat(starwars, starwars[1:20, ])
#> # A tibble: 8 × 5
#>   col_name      jsd     pval lvls_1            lvls_2           
#>   <chr>       <dbl>    <dbl> <named list>      <named list>     
#> 1 eye_color  0.0936 7.08e- 1 <tibble [15 × 3]> <tibble [8 × 3]> 
#> 2 gender     0.0132 6.02e- 1 <tibble [3 × 3]>  <tibble [3 × 3]> 
#> 3 hair_color 0.260  5.59e- 4 <tibble [12 × 3]> <tibble [10 × 3]>
#> 4 homeworld  0.394  2.21e- 2 <tibble [49 × 3]> <tibble [11 × 3]>
#> 5 name       0.573  9.35e-11 <tibble [87 × 3]> <tibble [20 × 3]>
#> 6 sex        0.0300 3.04e- 1 <tibble [5 × 3]>  <tibble [5 × 3]> 
#> 7 skin_color 0.299  1.58e- 1 <tibble [31 × 3]> <tibble [10 × 3]>
#> 8 species    0.270  1.75e- 1 <tibble [38 × 3]> <tibble [7 × 3]> 

# Grouped data frame summary
starwars %>% group_by(gender) %>% inspect_cat()
#> # A tibble: 21 × 6
#> # Groups:   gender [3]
#>    gender    col_name     cnt common   common_pcnt levels           
#>    <chr>     <chr>      <int> <chr>          <dbl> <named list>     
#>  1 masculine eye_color     13 brown          22.7  <tibble [13 × 3]>
#>  2 masculine hair_color    10 none           47.0  <tibble [10 × 3]>
#>  3 masculine homeworld     43 Tatooine       12.1  <tibble [43 × 3]>
#>  4 masculine name          66 Ackbar          1.52 <tibble [66 × 3]>
#>  5 masculine sex            3 male           90.9  <tibble [3 × 3]> 
#>  6 masculine skin_color    27 fair           19.7  <tibble [27 × 3]>
#>  7 masculine species       33 Human          39.4  <tibble [33 × 3]>
#>  8 feminine  eye_color      7 blue           35.3  <tibble [7 × 3]> 
#>  9 feminine  hair_color     6 none           35.3  <tibble [6 × 3]> 
#> 10 feminine  homeworld     11 NA             17.6  <tibble [11 × 3]>
#> # ℹ 11 more rows
```
