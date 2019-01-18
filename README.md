
reporter <img src="man/figures/hex.png" align="right" width="120" />
====================================================================

[![Build Status](https://travis-ci.org/alastairrushworth/reporter.svg?branch=master)](https://travis-ci.org/alastairrushworth/reporter) [![codecov](https://codecov.io/gh/alastairrushworth/reporter/branch/master/graph/badge.svg)](https://codecov.io/gh/alastairrushworth/reporter)

Overview
--------

`reporter` is a collection of functions for quickly summarising and comparing data frames. The package has two aims:
+ to speed up repetitive exploratory tasks typically undertaken when a new data frame is encountered
+ to make it easier to compare data frames for differences and inconsistencies

Functions are provided to explore and compare column types, memory usage, missing values, distributions of categorical and numeric features, correlation and imbalance.

Installation
------------

To install the development version of the package, use the command

``` r
devtools::install_github("alastairrushworth/reporter")
```

Then load the package and the `starwars` data.

``` r
# load reporter
library(reporter)

# some example data
data(starwars, package = "dplyr")
```

Single data frame summaries
---------------------------

#### Column types

To explore the column types in a data frame, use the function `report_types`. The command returns a tibble summarising the percentage of columns with the a particular type. A barplot is also returned when `show_plot = TRUE`.

``` r
# return tibble and visualisation of columns types
report_types(starwars, show_plot = T)
```

![](man/figures/README-unnamed-chunk-4-1.png)

    ## # A tibble: 4 x 3
    ##   col_type  count_type percent
    ##   <chr>          <dbl>   <dbl>
    ## 1 character          7   53.8 
    ## 2 list               3   23.1 
    ## 3 numeric            2   15.4 
    ## 4 integer            1    7.69

#### Memory usage

To explore the memory usage of the columns in a data frame, use the function `report_space`. The command returns a tibble containing the memory usage and percentage of total usage for each column in the data frame. A barplot is also returned when `show_plot = TRUE`.

``` r
report_space(starwars, show_plot = T)
```

![](man/figures/README-unnamed-chunk-5-1.png)

    ## # A tibble: 13 x 3
    ##    col_name   size        pcnt
    ##    <chr>      <chr>      <dbl>
    ##  1 films      19.54 Kb  36.5  
    ##  2 starships  7.27 Kb   13.6  
    ##  3 name       6.13 Kb   11.5  
    ##  4 vehicles   5.8 Kb    10.8  
    ##  5 homeworld  3.52 Kb    6.58 
    ##  6 species    2.88 Kb    5.39 
    ##  7 skin_color 2.59 Kb    4.85 
    ##  8 eye_color  1.57 Kb    2.93 
    ##  9 hair_color 1.41 Kb    2.63 
    ## 10 gender     976 bytes  1.78 
    ## 11 mass       744 bytes  1.36 
    ## 12 birth_year 744 bytes  1.36 
    ## 13 height     400 bytes  0.730

#### Missing values

`report_na` is used to report the number and proportion of missing values contained within each column in a data frame. The command returns a tibble containing the count and overall percentage of missing values by column in the data frame. A barplot is also returned when `show_plot` is set to `TRUE`.

``` r
report_na(starwars, show_plot = T)
```

![](man/figures/README-unnamed-chunk-6-1.png)

    ## # A tibble: 13 x 3
    ##    col_name   cnt_na  pcnt
    ##    <chr>       <int> <dbl>
    ##  1 birth_year     44 50.6 
    ##  2 mass           28 32.2 
    ##  3 homeworld      10 11.5 
    ##  4 height          6  6.90
    ##  5 hair_color      5  5.75
    ##  6 species         5  5.75
    ##  7 gender          3  3.45
    ##  8 name            0  0   
    ##  9 skin_color      0  0   
    ## 10 eye_color       0  0   
    ## 11 films           0  0   
    ## 12 vehicles        0  0   
    ## 13 starships       0  0

#### Correlation

`report_cor` returns a tibble containing Pearson's correlation coefficient, confidence intervals and *p*-values between pairs of numeric columns in a data frame. The function combines the functionality of `cor()` and `cor.test()` into a more useable wrapper. An point and whiskers plot is also returned when `show_plot = TRUE`.

``` r
report_cor(starwars, show_plot = T)
```

![](man/figures/README-unnamed-chunk-7-1.png)

    ## # A tibble: 3 x 7
    ##   col_1      col_2  pair                  corr p_value  lower  upper
    ##   <chr>      <chr>  <chr>                <dbl>   <dbl>  <dbl>  <dbl>
    ## 1 birth_year mass   birth_year & mass    0.478 0.00318  0.177  0.697
    ## 2 birth_year height birth_year & height -0.400 0.00789 -0.625 -0.113
    ## 3 mass       height mass & height        0.134 0.312   -0.127  0.377

\_<Notes:_>
+ The tibble is sorted in descending order of the absolute coefficient.
+ `report_cor` drops missing values prior to calculation of each correlation coefficient.
+ The `p_value` column is associated with the null hypothesis *H*<sub>0</sub> : *ρ* = 0.

#### Feature imbalance

Categorical features where each element is identical (or nearly) are often removed or scrutinised more closely. The function `report_imbalance` helps to find categorical columns that are dominated by a single feature level and returns a tibble containing the columns: `col_name` the categorical column names; `value` the most frequently occurring categorical level in each column; `percent` the percentage frequency with which the value occurs. The tibble is sorted in descending order of the `percent`. A barplot is also returned when `show_plot` is set to `TRUE`.

``` r
report_imbalance(starwars, show_plot = T)
```

![](man/figures/README-unnamed-chunk-8-1.png)

    ## # A tibble: 7 x 3
    ##   col_name   value  percent
    ##   <chr>      <chr>    <dbl>
    ## 1 gender     male     71.3 
    ## 2 hair_color none     42.5 
    ## 3 species    Human    40.2 
    ## 4 eye_color  brown    24.1 
    ## 5 skin_color fair     19.5 
    ## 6 homeworld  Naboo    12.6 
    ## 7 name       Ackbar    1.15

#### Numeric summaries

`report_numeric` generates statistical summaries of numeric columns contained in a data frame, combining some of the functionality of `summary` and `hist`. The tibble returned contains standard numerical summaries (min, max, mean, median etc.), but also the percentage of missing entries (`percent_na`) and a simple histogram (`hist`). If `show_plot = TRUE` a histogram is generated for each numeric feature.

``` r
report_numeric(starwars, show_plot = T)
```

![](man/figures/README-unnamed-chunk-9-1.png)

    ## # A tibble: 3 x 10
    ##   col_name    min    q1 median  mean    q3   max    sd percent_na hist     
    ##   <chr>     <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>      <dbl> <list>   
    ## 1 birth_ye…     8  35       52  87.6  72     896 155.       50.6  <tibble …
    ## 2 height       66 167      180 174.  191     264  34.8       6.90 <tibble …
    ## 3 mass         15  55.6     79  97.3  84.5  1358 169.       32.2  <tibble …

The `hist` column is a list whose elements are tibbles each containing a simple histogram with the relative frequency of counts for each feature. These tibbles are used to generate the histograms shown when `show_plot = TRUE`. For example, the histogram for `starwars$birth_year` is

``` r
report_numeric(starwars)$hist$birth_year
```

    ## # A tibble: 47 x 2
    ##    value        prop
    ##    <chr>       <dbl>
    ##  1 [-Inf, 0)  0     
    ##  2 [0, 20)    0.0930
    ##  3 [20, 40)   0.209 
    ##  4 [40, 60)   0.326 
    ##  5 [60, 80)   0.140 
    ##  6 [80, 100)  0.116 
    ##  7 [100, 120) 0.0465
    ##  8 [120, 140) 0     
    ##  9 [140, 160) 0     
    ## 10 [160, 180) 0     
    ## # … with 37 more rows

#### Categorical levels

`report_levels` returns a tibble summarising categorical features in the data frame. This combines the functionality of `report_imbalance` and the `table` function. The tibble generated contains the columns

-   `col_name`
-   `n_levels` the number of unique levels in the feature
-   `dom_level` the most common level (see also `report_imbalance`)
-   `dom_percent` the percentage occurrence of the most dominant level
-   `levels` a list of tibbles each containing frequency tabulations of all levels

``` r
report_levels(starwars)
```

    ## # A tibble: 7 x 5
    ##   col_name   n_levels dom_level dom_percent levels           
    ##   <chr>         <int> <chr>           <dbl> <list>           
    ## 1 eye_color        15 brown           24.1  <tibble [15 × 2]>
    ## 2 gender            4 male            71.3  <tibble [4 × 2]> 
    ## 3 hair_color       12 none            42.5  <tibble [12 × 2]>
    ## 4 homeworld        48 Naboo           12.6  <tibble [48 × 2]>
    ## 5 name             87 Ackbar           1.15 <tibble [87 × 2]>
    ## 6 skin_color       31 fair            19.5  <tibble [31 × 2]>
    ## 7 species          37 Human           40.2  <tibble [37 × 2]>

For example, the levels for the `hair_color` column are

``` r
report_levels(starwars)$levels$hair_color
```

    ## # A tibble: 12 x 2
    ##    value           prop
    ##    <chr>          <dbl>
    ##  1 none          0.425 
    ##  2 brown         0.207 
    ##  3 black         0.149 
    ##  4 white         0.0460
    ##  5 blond         0.0345
    ##  6 auburn        0.0115
    ##  7 auburn, grey  0.0115
    ##  8 auburn, white 0.0115
    ##  9 blonde        0.0115
    ## 10 brown, grey   0.0115
    ## 11 grey          0.0115
    ## 12 unknown       0.0115
