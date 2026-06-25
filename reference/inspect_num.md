# Summary and comparison of numeric columns

For a single data frame, summarise the numeric columns. If two data
frames are supplied, compare numeric columns appearing in both data
frames. For grouped data frames, summarise numeric columns separately
for each group.

## Usage

``` r
inspect_num(df1, df2 = NULL, breaks = 20, include_int = TRUE)
```

## Arguments

- df1:

  A data frame.

- df2:

  An optional second data frame for comparing numeric columns. Defaults
  to `NULL`.

- breaks:

  Integer number of breaks used for histogram bins, passed to
  [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html). Defaults
  to 20.

- include_int:

  Logical flag, whether to include integer columns in numeric summaries.
  Defaults to `TRUE`.

## Value

A `tibble` containing statistical summaries of the numeric columns of
`df1`, or comparing the histograms of `df1` and `df2`.

## Details

For a **single data frame**, the tibble returned contains the columns:  

- `col_name`, a character vector containing the column names in `df1`

- `min`, `q1`, `median`, `mean`, `q3`, `max` and `sd`, the minimum,
  lower quartile, median, mean, upper quartile, maximum and standard
  deviation for each numeric column.

- `pcnt_na`, the percentage of each numeric feature that is missing

- `hist`, a named list of tibbles containing the relative frequency of
  values falling in bins determined by `breaks`.

For a **pair of data frames**, the tibble returned contains the
columns:  

- `col_name`, a character vector containing the column names in `df1`
  and `df2`

- `hist_1`, `hist_2`, a list column for histograms of each of `df1` and
  `df2`. Where a column appears in both data frames, the bins used for
  `df1` are reused to calculate histograms for `df2`.

- jsd, a numeric column containing the Jensen-Shannon divergence. This
  measures the difference in distribution of a pair of binned numeric
  features. Values near to 0 indicate agreement of the distributions,
  while 1 indicates disagreement.

- `pval`, the p-value corresponding to a NHT that the true frequencies
  of histogram bins are equal. A small p indicates evidence that the the
  two sets of relative frequencies are actually different. The test is
  based on a modified Chi-squared statistic.

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
inspect_num(starwars)
#> # A tibble: 3 × 10
#>   col_name     min    q1 median  mean    q3   max    sd pcnt_na hist        
#>   <chr>      <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <named list>
#> 1 height        66 167      180 175.  191     264  34.8    6.90 <tibble>    
#> 2 mass          15  55.6     79  97.3  84.5  1358 169.    32.2  <tibble>    
#> 3 birth_year     8  35       52  87.6  72     896 155.    50.6  <tibble>    

# Paired data frame comparison
inspect_num(starwars, starwars[1:20, ])
#> # A tibble: 3 × 5
#>   col_name   hist_1            hist_2               jsd  pval
#>   <chr>      <named list>      <named list>       <dbl> <dbl>
#> 1 height     <tibble [21 × 2]> <tibble [21 × 2]> 0.184  0.284
#> 2 mass       <tibble [28 × 2]> <tibble [28 × 2]> 0.0236 0.701
#> 3 birth_year <tibble [18 × 2]> <tibble [18 × 2]> 0.0343 0.252

# Grouped data frame summary
starwars %>% group_by(gender) %>% inspect_num()
#> # A tibble: 9 × 11
#>   gender   col_name   min    q1 median  mean    q3   max     sd pcnt_na hist    
#>   <chr>    <chr>    <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <named >
#> 1 masculi… height      66 171.   183   177.  193     264  37.6     6.06 <tibble>
#> 2 masculi… mass        15  74.5   80   107.   87.5  1358 189.     28.8  <tibble>
#> 3 masculi… birth_y…     8  32.2   53    96.8  82     896 170.     47.0  <tibble>
#> 4 feminine height      96 164    167   167.  178     213  24.8    11.8  <tibble>
#> 5 feminine mass        45  50     55    54.7  56.2    75   8.59   47.1  <tibble>
#> 6 feminine birth_y…    19  44.5   47.5  47.2  50.5    72  15.0    52.9  <tibble>
#> 7 NA       height     157 173.   179   175   181.    185  12.4     0    <tibble>
#> 8 NA       mass        48  66.5   85    81    97.5   110  31.2    25    <tibble>
#> 9 NA       birth_y…    NA  NA     NA    NA    NA      NA  NA     100    <tibble>
```
