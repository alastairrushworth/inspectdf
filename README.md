
# inspectdf <img src="man/figures/logo.png" align="right" width="120" />

[![Build
Status](https://travis-ci.org/alastairrushworth/inspectdf.svg?branch=master)](https://travis-ci.org/alastairrushworth/inspectdf)
[![codecov](https://codecov.io/gh/alastairrushworth/inspectdf/branch/master/graph/badge.svg)](https://codecov.io/gh/alastairrushworth/inspectdf)
[![CRAN
status](https://www.r-pkg.org/badges/version/inspectdf)](https://cran.r-project.org/package=inspectdf)
[![](https://cranlogs.r-pkg.org/badges/inspectdf)](https://cran.r-project.org/package=inspectdf)
[![cran
checks](https://cranchecks.info/badges/summary/inspectdf)](https://cran.r-project.org/web/checks/check_results_inspectdf.html)

## Overview

`inspectdf` is collection of utilities for columnwise summary,
comparison and visualisation of data frames. Functions are provided to
summarise missingness, categorical levels, numeric distribution,
correlation, column types and memory usage.

The package has three aims:

  - to speed up repetitive checking and exploratory tasks for data
    frames  
  - to make it easier to compare data frames for differences and
    inconsistencies
  - to support quick visualisation of data frames

## Key functions

  - [`inspect_types()`](#column-types) summary of column types
  - [`inspect_mem()`](#memory-usage) summary of memory usage of columns
  - [`inspect_na()`](#missing-values) columnwise prevalence of missing
    values
  - [`inspect_cor()`](#correlation) correlation coefficients of numeric
    columns
  - [`inspect_imb()`](#feature-imbalance) feature imbalance of
    categorical columns
  - [`inspect_num()`](#numeric-summaries) summaries of numeric columns
  - [`inspect_cat()`](#categorical-levels) summaries of categorical
    columns

## Installation

To install the development version of the package, use

``` r
devtools::install_github("alastairrushworth/inspectdf")

# load the package
library(inspectdf)
```

## Illustrative data: `starwars`

The examples below make use of the `starwars` and `storms` data from the
`dplyr` package

``` r
# some example data
data(starwars, package = "dplyr")
data(storms, package = "dplyr")
```

For illustrating comparisons of dataframes, use the `starwars` data and
produce two new dataframes `star_1` and `star_2` that randomly sample
the rows of the original and drop a couple of columns.

``` r
library(dplyr)
star_1 <- starwars %>% sample_n(50)
star_2 <- starwars %>% sample_n(50) %>% select(-1, -2)
```

#### Column types

##### `inspect_types()` for a single dataframe

To explore the column types in a data frame, use the function
`inspect_types()`. The command returns a `tibble` summarising the counts
and percentages of columns with particular types.

``` r
# return tibble showing columns types
inspect_types(starwars)
```

    ## # A tibble: 4 x 4
    ##   type        cnt  pcnt col_name 
    ##   <chr>     <int> <dbl> <list>   
    ## 1 character     7 53.8  <chr [7]>
    ## 2 list          3 23.1  <chr [3]>
    ## 3 numeric       2 15.4  <chr [2]>
    ## 4 integer       1  7.69 <chr [1]>

A barplot can be produced by passing the result to `show_plot()`:

``` r
# print visualisation of column types
inspect_types(starwars) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

##### `inspect_types()` for two dataframes

When a second dataframe is provided, `inspect_types()` will create a
dataframe comparing the count and percentage of each column type for
each of the input dataframes. The summaries for the first and second
dataframes are show in columns with names appended with `_1` and `_2`,
respectively.

``` r
inspect_types(star_1, star_2)
```

    ## # A tibble: 4 x 5
    ##   type      cnt_1 pcnt_1 cnt_2 pcnt_2
    ##   <chr>     <int>  <dbl> <dbl>  <dbl>
    ## 1 character     7  53.8      6   54.5
    ## 2 list          3  23.1      3   27.3
    ## 3 numeric       2  15.4      2   18.2
    ## 4 integer       1   7.69     0    0

``` r
# print visualisation of column type comparison
inspect_types(star_1, star_2) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

#### Memory usage

##### `inspect_mem()` for a single dataframe

To explore the memory usage of the columns in a data frame, use
`inspect_mem()`. The command returns a `tibble` containing the size of
each column in the dataframe.

``` r
inspect_mem(starwars)
```

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

A barplot can be produced by passing the result to `show_plot()`:

``` r
inspect_mem(starwars) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

##### `inspect_mem()` for two dataframes

When a second dataframe is provided, `inspect_mem()` will create a
dataframe comparing the size of each column for both input dataframes.
The summaries for the first and second dataframes are show in columns
with names appended with `_1` and `_2`, respectively.

``` r
inspect_mem(star_1, star_2)
```

    ## # A tibble: 13 x 5
    ##    col_name   size_1    size_2    pcnt_1 pcnt_2
    ##    <chr>      <chr>     <chr>      <dbl>  <dbl>
    ##  1 films      11.2 Kb   11.28 Kb  34.3    40.2 
    ##  2 starships  4.08 Kb   4.19 Kb   12.5    14.9 
    ##  3 name       3.57 Kb   <NA>      10.9    NA   
    ##  4 vehicles   3.49 Kb   3.41 Kb   10.7    12.1 
    ##  5 homeworld  2.48 Kb   2.01 Kb    7.61    7.15
    ##  6 skin_color 2 Kb      2.01 Kb    6.13    7.15
    ##  7 species    1.88 Kb   1.65 Kb    5.77    5.87
    ##  8 eye_color  1.16 Kb   1.05 Kb    3.57    3.73
    ##  9 hair_color 1 Kb      968 bytes  3.06    3.37
    ## 10 gender     680 bytes 680 bytes  2.03    2.37
    ## 11 mass       448 bytes 448 bytes  1.34    1.56
    ## 12 birth_year 448 bytes 448 bytes  1.34    1.56
    ## 13 height     248 bytes <NA>       0.742  NA

``` r
inspect_mem(star_1, star_2) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-13-1.png)<!-- -->

#### Missing values

##### `inspect_na()` for a single dataframe

`inspect_na()` summarises the prevalence of missing values by each
column in a data frame. A tibble containing the count (`cnt`) and the
overall percentage (`pcnt`) of missing values is returned.

``` r
inspect_na(starwars)
```

    ## # A tibble: 13 x 3
    ##    col_name     cnt  pcnt
    ##    <chr>      <dbl> <dbl>
    ##  1 birth_year    44 50.6 
    ##  2 mass          28 32.2 
    ##  3 homeworld     10 11.5 
    ##  4 height         6  6.90
    ##  5 hair_color     5  5.75
    ##  6 species        5  5.75
    ##  7 gender         3  3.45
    ##  8 name           0  0   
    ##  9 skin_color     0  0   
    ## 10 eye_color      0  0   
    ## 11 films          0  0   
    ## 12 vehicles       0  0   
    ## 13 starships      0  0

A barplot can be produced by passing the result to `show_plot()`:

``` r
inspect_na(starwars) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-15-1.png)<!-- -->

##### `inspect_na()` for two dataframes

When a second dataframe is provided, `inspect_na()` returns a tibble
containing counts and percentage missingness by column, with summaries
for the first and second data frames are show in columns with names
appended with `_1` and `_2`, respectively. In addition, a \(p\)-value is
calculated which provides a measure of evidence of whether the
difference in missing values is significantly different.

``` r
inspect_na(star_1, star_2)
```

    ## # A tibble: 13 x 6
    ##    col_name   cnt_1 pcnt_1 cnt_2 pcnt_2 p_value
    ##    <chr>      <dbl>  <dbl> <dbl>  <dbl>   <dbl>
    ##  1 birth_year    25     50    25     50   1    
    ##  2 mass          17     34    17     34   1.000
    ##  3 homeworld      4      8     8     16   0.356
    ##  4 height         2      4    NA     NA  NA    
    ##  5 hair_color     2      4     5     10   0.433
    ##  6 species        2      4     5     10   0.433
    ##  7 gender         1      2     3      6   0.610
    ##  8 name           0      0    NA     NA  NA    
    ##  9 skin_color     0      0     0      0  NA    
    ## 10 eye_color      0      0     0      0  NA    
    ## 11 films          0      0     0      0  NA    
    ## 12 vehicles       0      0     0      0  NA    
    ## 13 starships      0      0     0      0  NA

``` r
inspect_na(star_1, star_2) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-17-1.png)<!-- -->

Notes:

  - Smaller \(p\)-values indicate stronger evidence of a difference in
    the missingness rate for a single column
  - If a column appears in one data frame and not the other - for
    example `height` appears in `star_1` but nor `star_2`, then the
    corresponding `pcnt_`, `cnt_` and `p_value` columns will contain
    `NA`
  - Where the missingness is identically 0, the `p_value` is `NA`.
  - The visualisation illustrates the significance of the difference
    using a coloured bar overlay. Orange bars indicate evidence of
    equality or missingness, while blue bars indicate inequality. If a
    `p_value` cannot be calculated, no coloured bar is shown.
  - The significance level can be specified using the `alpha` argument
    to `inspect_na()`. The default is `alpha = 0.05`.

#### Correlation

##### `inspect_cor()` for a single dataframe

`inspect_cor()` returns a tibble containing Pearson’s correlation
coefficient, confidence intervals and \(p\)-values for pairs of numeric
columns . The function combines the functionality of `cor()` and
`cor.test()` in a more convenient wrapper.

``` r
inspect_cor(storms)
```

    ## # A tibble: 45 x 6
    ##    col_1       col_2         corr  p_value  lower  upper
    ##    <chr>       <chr>        <dbl>    <dbl>  <dbl>  <dbl>
    ##  1 pressure    wind        -0.942 0.       -0.945 -0.940
    ##  2 hu_diameter pressure    -0.842 0.       -0.853 -0.831
    ##  3 hu_diameter wind         0.774 0.        0.758  0.788
    ##  4 hu_diameter ts_diameter  0.684 0.        0.663  0.704
    ##  5 ts_diameter pressure    -0.683 0.       -0.703 -0.663
    ##  6 ts_diameter wind         0.640 0.        0.617  0.662
    ##  7 ts_diameter lat          0.301 1.25e-73  0.266  0.335
    ##  8 day         month       -0.183 3.59e-76 -0.205 -0.161
    ##  9 hu_diameter lat          0.164 1.59e-22  0.127  0.201
    ## 10 ts_diameter month        0.139 1.67e-16  0.102  0.176
    ## # … with 35 more rows

A plot showing point estimate and confidence intervals is printed when
using the `show_plot()` function. Note that intervals that straddle the
null value of 0 are shown in gray:

``` r
inspect_cor(storms) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-19-1.png)<!-- -->

Notes:

  - The tibble is sorted in descending order of the absolute coefficient
    \(|\rho|\).
  - `inspect_cor` drops missing values prior to calculation of each
    correlation coefficient.  
  - The `p_value` is associated with the null hypothesis
    \(H_0: \rho = 0\).

##### `inspect_cor()` for two dataframes

When a second dataframe is provided, `inspect_cor()` returns a tibble
that compares correlation coefficients of the first dataframe to those
in the second. The `p_value` column contains a measure of evidence for
whether the two correlation coefficients are equal or not.

``` r
inspect_cor(storms, storms[-c(1:200), ])
```

    ## # A tibble: 45 x 5
    ##    col_1       col_2       corr_1 corr_2 p_value
    ##    <chr>       <chr>        <dbl>  <dbl>   <dbl>
    ##  1 pressure    wind        -0.942 -0.942   0.929
    ##  2 hu_diameter pressure    -0.842 -0.842   1    
    ##  3 hu_diameter wind         0.774  0.774   1    
    ##  4 hu_diameter ts_diameter  0.684  0.684   1    
    ##  5 ts_diameter pressure    -0.683 -0.683   1    
    ##  6 ts_diameter wind         0.640  0.640   1    
    ##  7 ts_diameter lat          0.301  0.301   1    
    ##  8 day         month       -0.183 -0.178   0.729
    ##  9 hu_diameter lat          0.164  0.164   1    
    ## 10 ts_diameter month        0.139  0.139   1    
    ## # … with 35 more rows

To plot the comparison of the top 20 correlation coefficients:

``` r
inspect_cor(storms, storms[-c(1:200), ]) %>% 
  slice(1:20) %>%
  show_plot()
```

![](man/figures/README-unnamed-chunk-21-1.png)<!-- -->

Notes:

  - Smaller `p_value` indicates stronger evidence against the null
    hypothesis \(H_0: \rho_1 = \rho_2\) and an indication that the true
    correlation coefficients differ.
  - The visualisation illustrates the significance of the difference
    using a coloured bar underlay. Coloured bars indicate evidence of
    inequality of correlations, while gray bars indicate equality.  
  - For a pair of features, if either coefficient is `NA`, the
    comparison is omitted from the visualisation.
  - The significance level can be specified using the `alpha` argument
    to `inspect_cor()`. The default is `alpha = 0.05`.

#### Feature imbalance

##### `inspect_imb()` for a single dataframe

Understanding categorical columns that are dominated by a single level
can be useful. `inspect_imb()` returns a tibble containing categorical
column names (`col_name`); the most frequently occurring categorical
level in each column (`value`) and `pctn` & `cnt` the percentage and
count which the value occurs. The tibble is sorted in descending order
of `pcnt`.

``` r
inspect_imb(starwars)
```

    ## # A tibble: 7 x 4
    ##   col_name   value   pcnt   cnt
    ##   <chr>      <chr>  <dbl> <int>
    ## 1 gender     male   71.3     62
    ## 2 hair_color none   42.5     37
    ## 3 species    Human  40.2     35
    ## 4 eye_color  brown  24.1     21
    ## 5 skin_color fair   19.5     17
    ## 6 homeworld  Naboo  12.6     11
    ## 7 name       Ackbar  1.15     1

A barplot is printed by passing the result to the `show_plot()`
function:

``` r
inspect_imb(starwars) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-23-1.png)<!-- -->

##### `inspect_imb()` for two dataframes

When a second dataframe is provided, `inspect_imb()` returns a tibble
that compares the frequency of the most common categorical values of the
first dataframe to those in the second. The `p_value` column contains a
measure of evidence for whether the true frequencies are equal or not.

``` r
inspect_imb(star_1, star_2)
```

    ## # A tibble: 7 x 7
    ##   col_name   value  pcnt_1 cnt_1 pcnt_2 cnt_2 p_value
    ##   <chr>      <chr>   <dbl> <int>  <dbl> <int>   <dbl>
    ## 1 gender     male       72    36    64     32   0.520
    ## 2 hair_color none       44    22    34     17   0.412
    ## 3 species    Human      42    21    40     20   1.000
    ## 4 skin_color fair       24    12    22     11   1.000
    ## 5 eye_color  blue       24    12    NA     NA  NA    
    ## 6 homeworld  Naboo      10     5    14.     7   0.758
    ## 7 name       Ackbar      2     1    NA     NA  NA

``` r
inspect_imb(star_1, star_2) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-25-1.png)<!-- -->

  - Smaller `p_value` indicates stronger evidence against the null
    hypothesis that the true frequency of the most common values is the
    same.
  - The visualisation illustrates the significance of the difference
    using a coloured bar overlay. Orange bars indicate evidence of
    equality of the imbalance, while blue bars indicate inequality. If a
    `p_value` cannot be calculated, no coloured bar is shown.
  - The significance level can be specified using the `alpha` argument
    to `inspect_imb()`. The default is `alpha = 0.05`.

#### Numeric summaries

##### `inspect_num()` for a single dataframe

`inspect_num()` combining some of the functionality of `summary()` and
`hist()` by returning summaries of numeric columns. `inspect_num()`
returns standard numerical summaries (`min`, `q1`, `mean`,
`median`,`q3`, `max`, `sd`), but also the percentage of missing entries
(`pcnt_na`) and a simple histogram (`hist`).

``` r
inspect_num(storms, breaks = 10)
```

    ## # A tibble: 10 x 10
    ##    col_name    min     q1 median    mean     q3    max     sd pcnt_na hist 
    ##    <chr>     <dbl>  <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <lis>
    ##  1 day         1      8     16     15.9    24     31     9.01     0   <tib…
    ##  2 hour        0      6     12      9.11   18     23     6.73     0   <tib…
    ##  3 hu_diam…    0      0      0     21.4    28.8  345.   41.3     65.2 <tib…
    ##  4 lat         7.2   17.5   24.4   24.8    31.3   51.9   8.54     0   <tib…
    ##  5 long     -109.   -80.7  -64.5  -64.2   -48.6   -6    19.6      0   <tib…
    ##  6 month       1      8      9      8.78    9     12     1.24     0   <tib…
    ##  7 pressure  882    985    999    992.   1006   1022    19.5      0   <tib…
    ##  8 ts_diam…    0     69.0  138.   167.    242.  1001.  141.      65.2 <tib…
    ##  9 wind       10     30     45     53.5    65    160    26.2      0   <tib…
    ## 10 year     1975   1990   1999   1998.   2006   2015    10.3      0   <tib…

The `hist` column is a list whose elements are tibbles each containing
the relative frequencies of bins for each feature. These tibbles are
used to generate the histograms when `show_plot = TRUE`. For example,
the histogram for `starwars$birth_year` is

``` r
inspect_num(storms)$hist$pressure
```

    ## # A tibble: 31 x 2
    ##    value            prop
    ##    <chr>           <dbl>
    ##  1 [-Inf, 880) 0        
    ##  2 [880, 885)  0.0000999
    ##  3 [885, 890)  0.000200 
    ##  4 [890, 895)  0.000400 
    ##  5 [895, 900)  0.000300 
    ##  6 [900, 905)  0.000300 
    ##  7 [905, 910)  0.000699 
    ##  8 [910, 915)  0.000999 
    ##  9 [915, 920)  0.00130  
    ## 10 [920, 925)  0.00390  
    ## # … with 21 more rows

A histogram is generated for each numeric feature by passing the result
to the `show_plot()` function:

``` r
inspect_num(storms, breaks = 10) %>%
  show_plot()
```

![](man/figures/README-unnamed-chunk-28-1.png)<!-- -->

##### `inspect_num()` for two dataframes

When comparing a pair of dataframes using `inspect_num()`, the
histograms of common numeric features are calculated, using identical
bins. The list columns `hist_1` and `hist_2` contain the histograms of
the features in the first and second dataframes. A formal statistical
comparison of each pair of histograms is calculated using Fisher’s exact
test, the resulting *p* value is reported in the column `fisher_p`.

When `show_plot = TRUE`, heat plot comparisons are returned for each
numeric column in each dataframe. Where a column is present in only one
of the dataframes, grey cells are shown in the comparison. The
significance of Fisher’s test is illustrated by coloured vertical bands
around each plot: if the colour is grey, no *p* value could be
calculated, if blue, the histograms are not found to be significantly
different otherwise the bands are red.

``` r
inspect_num(storms, storms[-c(1:10), -1])
```

    ## # A tibble: 10 x 5
    ##    col_name    hist_1            hist_2                    jsd fisher_p
    ##    <chr>       <list>            <list>                  <dbl>    <dbl>
    ##  1 day         <tibble [18 × 2]> <tibble [18 × 2]> 0.00000119    NA    
    ##  2 hour        <tibble [25 × 2]> <tibble [25 × 2]> 0.000539      NA    
    ##  3 hu_diameter <tibble [20 × 2]> <tibble [20 × 2]> 0              1.000
    ##  4 lat         <tibble [25 × 2]> <tibble [25 × 2]> 0.000000460   NA    
    ##  5 long        <tibble [23 × 2]> <tibble [23 × 2]> 0.00000174    NA    
    ##  6 month       <tibble [24 × 2]> <tibble [24 × 2]> 0.0125        NA    
    ##  7 pressure    <tibble [31 × 2]> <tibble [31 × 2]> 0.00000102    NA    
    ##  8 ts_diameter <tibble [23 × 2]> <tibble [23 × 2]> 0              1.000
    ##  9 wind        <tibble [17 × 2]> <tibble [17 × 2]> 0.000104      NA    
    ## 10 year        <tibble [23 × 2]> <tibble [23 × 2]> 0.0000221     NA

``` r
inspect_num(storms, storms[-c(1:10), -1]) %>% 
  show_plot()
```

![](man/figures/README-unnamed-chunk-30-1.png)<!-- -->

#### Categorical levels

##### `inspect_cat()` for a single dataframe

`inspect_cat()` returns a tibble summarising categorical features in a
data frame, combining the functionality of the `inspect_imb()` and
`table()` functions. The tibble generated contains the columns

  - `col_name` name of each categorical column
  - `cnt` the number of unique levels in the feature
  - `common` the most common level (see also `inspect_imb()`)  
  - `common_pcnt` the percentage occurrence of the most dominant level  
  - `levels` a list of tibbles each containing frequency tabulations of
    all levels

<!-- end list -->

``` r
inspect_cat(starwars)
```

    ## # A tibble: 7 x 5
    ##   col_name     cnt common common_pcnt levels           
    ##   <chr>      <int> <chr>        <dbl> <list>           
    ## 1 eye_color     15 brown        24.1  <tibble [15 × 3]>
    ## 2 gender         5 male         71.3  <tibble [5 × 3]> 
    ## 3 hair_color    13 none         42.5  <tibble [13 × 3]>
    ## 4 homeworld     49 Naboo        12.6  <tibble [49 × 3]>
    ## 5 name          87 Ackbar        1.15 <tibble [87 × 3]>
    ## 6 skin_color    31 fair         19.5  <tibble [31 × 3]>
    ## 7 species       38 Human        40.2  <tibble [38 × 3]>

For example, the levels for the `hair_color` column are

``` r
inspect_cat(starwars)$levels$hair_color
```

    ## # A tibble: 13 x 3
    ##    value           prop   cnt
    ##    <chr>          <dbl> <int>
    ##  1 none          0.425     37
    ##  2 brown         0.207     18
    ##  3 black         0.149     13
    ##  4 <NA>          0.0575     5
    ##  5 white         0.0460     4
    ##  6 blond         0.0345     3
    ##  7 auburn        0.0115     1
    ##  8 auburn, grey  0.0115     1
    ##  9 auburn, white 0.0115     1
    ## 10 blonde        0.0115     1
    ## 11 brown, grey   0.0115     1
    ## 12 grey          0.0115     1
    ## 13 unknown       0.0115     1

Note that by default, if `NA` values are present, they are counted as a
distinct categorical level. A barplot is printed showing the relative
split when passing the result to `show_plot()`:

``` r
inspect_cat(starwars) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-33-1.png)<!-- -->

The argument `high_cardinality` in the `show_plot()` function can be
used to bundle together categories that occur only a small number of
times. For example, to combine categories only occurring once, use:

``` r
inspect_cat(starwars) %>% 
  show_plot(high_cardinality = 1)
```

![](man/figures/README-unnamed-chunk-34-1.png)<!-- -->

The resulting bundles are shown in purple.

##### `inspect_cat()` for two dataframes

When two dataframes are compared using `inspect_cat()`, list columns are
returned for categorical columns common to both: `lvls_1` and `lvl2_2`.
In addition, the Jensen-Shannon divergence (`jsd`) and *p* values
associated with Fisher’s exact test (`fisher_p`) are returned to enable
comparison of the distribution of levels in each pair of columns.

``` r
inspect_cat(star_1, star_2)
```

    ## # A tibble: 7 x 5
    ##   col_name       jsd fisher_p lvls_1            lvls_2           
    ##   <chr>        <dbl>    <dbl> <list>            <list>           
    ## 1 eye_color   0.122     0.703 <tibble [13 × 3]> <tibble [11 × 3]>
    ## 2 gender      0.0106    0.870 <tibble [5 × 3]>  <tibble [5 × 3]> 
    ## 3 hair_color  0.0690    0.921 <tibble [11 × 3]> <tibble [10 × 3]>
    ## 4 homeworld   0.180     0.996 <tibble [36 × 3]> <tibble [28 × 3]>
    ## 5 name       NA        NA     <tibble [50 × 3]> <NULL>           
    ## 6 skin_color  0.122     0.936 <tibble [26 × 3]> <tibble [26 × 3]>
    ## 7 species     0.138     0.990 <tibble [26 × 3]> <tibble [22 × 3]>

``` r
inspect_cat(star_1, star_2) %>% show_plot()
```

![](man/figures/README-unnamed-chunk-36-1.png)<!-- -->
