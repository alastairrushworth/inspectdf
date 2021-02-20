
# inspectdf <img src="man/figures/logo.png" align="right" width="120" />

![build](https://github.com/alastairrushworth/inspectdf/workflows/R-CMD-check/badge.svg)
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

-   to speed up repetitive checking and exploratory tasks for data
    frames  
-   to make it easier to compare data frames for differences and
    inconsistencies
-   to support quick visualisation of data frames

Check out the [package
website](https://alastairrushworth.github.io/inspectdf/) for further
documentation and examples.

## Installation

To install the development version of the package, use

``` r
devtools::install_github("alastairrushworth/inspectdf")
```

To install the CRAN version of the package, use

``` r
install.packages("inspectdf")
```

## Key functions

-   [`inspect_types()`](https://alastairrushworth.github.io/inspectdf/articles/pkgdown/inspect_types_examples.html)
    summary of column types
-   [`inspect_mem()`](https://alastairrushworth.github.io/inspectdf/articles/pkgdown/inspect_mem_examples.html)
    summary of memory usage of columns
-   [`inspect_na()`](https://alastairrushworth.github.io/Missing-values-in-dataframes-with-inspectdf/)
    columnwise prevalence of missing values
-   [`inspect_cor()`](https://alastairrushworth.github.io/Calculating-and-visualising-correlation-coefficients-with-inspectdf/)
    correlation coefficients of numeric columns
-   [`inspect_imb()`](https://alastairrushworth.github.io/inspectdf/articles/pkgdown/inspect_imb_examples.html)
    feature imbalance of categorical columns
-   [`inspect_num()`](https://alastairrushworth.github.io/inspectdf/articles/pkgdown/inspect_num_examples.html)
    summaries of numeric columns
-   [`inspect_cat()`](https://alastairrushworth.github.io/Exploring-categorical-data-with-inspectdf/)
    summaries of categorical columns

## Comments? Suggestions? Issues?

Any feedback is welcome! Feel free to write a github issue or send me a
message on [twitter](https://twitter.com/rushworth_a).
