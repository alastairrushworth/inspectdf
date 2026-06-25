# Tech stocks closing prices

Daily closing stock prices of the three tech companies Microsoft, Apple
and IBM between 2007 and 2019.

## Usage

``` r
data(tech)
```

## Format

A `data.frame` with 3158 rows and 6 columns.

## Source

Data gathered using the
[quantmod](https://github.com/joshuaulrich/quantmod) package.

## Examples

``` r
data(tech)
head(tech)
#>         date quarter year    apple microsoft    ibm
#> 1 2007-01-03  2007-1 2007 11.97143     29.86  97.27
#> 2 2007-01-04  2007-1 2007 12.23714     29.81  98.31
#> 3 2007-01-05  2007-1 2007 12.15000     29.64  97.42
#> 4 2007-01-08  2007-1 2007 12.21000     29.93  98.90
#> 5 2007-01-09  2007-1 2007 13.22429     29.96 100.07
#> 6 2007-01-10  2007-1 2007 13.85714     29.66  98.89
# NOT RUN - change in correlation over time
# library(dplyr)
# tech_grp <- tech %>% 
#         group_by(year) %>%
#         inspect_cor()
# tech_grp %>% show_plot()    
```
