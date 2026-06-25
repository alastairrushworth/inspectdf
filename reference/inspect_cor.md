# Tidy correlation coefficients for numeric data frame columns

Summarise and compare Pearson, Kendall and Spearman correlations for
numeric columns in one, two or grouped data frames.

## Usage

``` r
inspect_cor(df1, df2 = NULL, method = "pearson", with_col = NULL, alpha = 0.05)
```

## Arguments

- df1:

  A data frame.

- df2:

  An optional second data frame for comparing correlation coefficients.
  Defaults to `NULL`.

- method:

  a character string indicating which type of correlation coefficient to
  use, one of `"pearson"`, `"kendall"`, or `"spearman"`, which can be
  abbreviated.

- with_col:

  Character vector of column names to calculate correlations with all
  other numeric features. The default `with_col = NULL` returns all
  pairs of correlations.

- alpha:

  Alpha level for correlation confidence intervals. Defaults to 0.05.

## Value

A tibble summarising and comparing the correlations for each numeric
column in one or a pair of data frames.

## Details

When `df2 = NULL`, a tibble containing correlation coefficients for
`df1` is returned:

- `col_1`, `col_2` character vectors containing names of numeric columns
  in `df1`.

- `corr` the calculated correlation coefficient.

- `p_value` p-value associated with a test where the null hypothesis is
  that the numeric pair have 0 correlation.

- `lower`, `upper` lower and upper values of the confidence interval for
  the correlations.

- `pcnt_nna` the number of pairs of observations that were non missing
  for each pair of columns. The correlation calculation used by
  `inspect_cor()` uses only pairwise complete observations.

If `df1` has class `grouped_df`, then correlations will be calculated
within the grouping levels and the tibble returned will have an
additional column corresponding to the group labels.

When both `df1` and `df2` are specified, the tibble returned contains a
comparison of the correlation coefficients across pairs of columns
common to both dataframes.

- `col_1`, `col_2` character vectors containing names of numeric columns
  in either `df1` or `df2`.

- `corr_1`, `corr_2` numeric columns containing correlation coefficients
  from `df1` and `df2`, respectively.

- `p_value` p-value associated with the null hypothesis that the two
  correlation coefficients are the same. Small values indicate that the
  true correlation coefficients differ between the two dataframes.

Note that confidence intervals for `kendall` and `spearman` assume a
normal sampling distribution for the Fisher z-transform of the
correlation.

## See also

[`show_plot`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)

## Author

Alastair Rushworth

## Examples

``` r

# Load dplyr for starwars data & pipe
library(dplyr)

# Single data frame summary
inspect_cor(starwars)
#> # A tibble: 3 × 7
#>   col_1      col_2    corr p_value  lower  upper pcnt_nna
#>   <chr>      <chr>   <dbl>   <dbl>  <dbl>  <dbl>    <dbl>
#> 1 birth_year mass    0.478 0.00602  0.177  0.697     41.4
#> 2 birth_year height -0.404 0.0106  -0.628 -0.118     49.4
#> 3 mass       height  0.131 0.327   -0.130  0.374     67.8
# Only show correlations with 'mass' column
inspect_cor(starwars, with_col = "mass")
#> # A tibble: 2 × 7
#>   col_1 col_2       corr p_value  lower upper pcnt_nna
#>   <chr> <chr>      <dbl>   <dbl>  <dbl> <dbl>    <dbl>
#> 1 mass  birth_year 0.478 0.00602  0.177 0.697     41.4
#> 2 mass  height     0.131 0.327   -0.130 0.374     67.8

# Paired data frame summary
inspect_cor(starwars, starwars[1:10, ])
#> # A tibble: 3 × 5
#>   col_1      col_2  corr_1 corr_2 p_value
#>   <chr>      <chr>   <dbl>  <dbl>   <dbl>
#> 1 birth_year mass    0.478  0.150 0.348  
#> 2 birth_year height -0.404  0.141 0.147  
#> 3 mass       height  0.131  0.866 0.00258

# NOT RUN - change in correlation over time
# library(dplyr)
# tech_grp <- tech %>% 
#         group_by(year) %>%
#         inspect_cor()
# tech_grp %>% show_plot()     
```
