# Changelog

## `inspectdf` 0.0.13

- Fixed compatibility with `dplyr` \>= 1.1.0 by replacing deprecated
  functions:
  [`select_if()`](https://dplyr.tidyverse.org/reference/select_all.html)
  replaced with `select(where())`, and
  [`mutate_if()`](https://dplyr.tidyverse.org/reference/mutate_all.html)
  replaced with `mutate(across(where()))`.
- Fixed critical bug in `plot_cat()` where `bind_rows(.id = )` with
  unnamed lists caused failures in newer `dplyr` versions. Function now
  properly assigns column names to list elements.
- Fixed issue in `plot_cat()` where filtering by non-existent `jsd`
  column removed all rows when plotting single dataframe summaries.
- Fixed
  [\#48](https://github.com/alastairrushworth/inspectdf/issues/48),
  `inspect_num(df1, df2)` with different ranges and different set of
  columns. Thanks to [cregouby](https://github.com/cregouby) for the
  [\#51](https://github.com/alastairrushworth/inspectdf/pull/51) fix.
- Fixed
  [\#45](https://github.com/alastairrushworth/inspectdf/issues/45),
  partial argument matching warning in `format_size()`. Changed
  `unit = "auto"` to `units = "auto"` in call to
  [`format()`](https://rdrr.io/r/base/format.html). Thanks to
  [@salim-b](https://github.com/salim-b) for the report.
- Updated CRAN checks badge URL from deprecated
  `cranchecks.info/badges/` to new `badges.cranchecks.info/` service.
- Fixed `ggplot2` deprecation warning by replacing `size` parameter with
  `linewidth` in `geom_bar()`.

## `inspectdf` 0.0.11

CRAN release: 2021-04-02

- Bug fixes to
  [`inspect_types()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_types.md)
  for pairwise comparison plots
- Updated tests for
  [`inspect_types()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_types.md)
  for pairwise comparisons

## `inspectdf` 0.0.10

CRAN release: 2021-02-20

- Add `include_int` option in
  [`inspect_cat()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cat.md)
  to allow treatment of integer columns as categorical.
- Improved p-values associated with binned categorical and numeric
  comparisons. This is now based on a modified chi-squared test and is
  labelled as `pval` in the resulting output.
- Fixed [\#27](https://github.com/alastairrushworth/inspectdf/issues/27)
  ensuring plots for
  [`inspect_cat()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cat.md)
  respect any filtering or sorting of the summary output prior to
  [`show_plot()`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md).
  Thanks to [Roel Verbelen](https://github.com/RoelVerbelen) for the
  report.
- Additional detail in `inspect_type()` comparison of two dataframes to
  make it easier to see which columns and types differ.

## `inspectdf` 0.0.9

CRAN release: 2020-09-07

- Minor change, ensuring all functions use `return` properly.

## `inspectdf` 0.0.8

CRAN release: 2020-06-25

- **Important change:** the `show_plot` argument has been removed from
  all `inspect_*()` functions. To generate visualisations of data frame
  summaries, please use the more flexible `show_plot(inspect_*())` or
  via the pipe `inspect_*() %>% show_plot()`.
- [`show_plot()`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)
  improvement that nudges points that might otherwise have coincided for
  dataframe comparisons of imbalance (for example, with
  `inspect_imb(df1, df2) %>% show_plot()`)\
- Plots for grouped summaries:
  [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md),
  [`inspect_na()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_na.md)
  and
  [`inspect_num()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_num.md).
- [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md)
  slight speed up for dataframes with large numbers of columns.
- [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md)
  can be filtered prior to plotting, for example
  `inspect_cor(starwars) %>% filter(abs(corr) > 0.2) %>% show_plot()`.
  Thanks to [Roel Verbelen](https://github.com/RoelVerbelen) for the
  [suggestion](https://github.com/alastairrushworth/inspectdf/issues/24)
- Fixed bug causing
  [`inspect_imb()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_imb.md)
  to fail on certain types of factor columns. Thanks to [Roel
  Verbelen](https://github.com/RoelVerbelen) for the
  [report](https://github.com/alastairrushworth/inspectdf/issues/26).
- [`show_plot()`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)
  has new arguments `label_size`, `label_angle` and `label_color`. Each
  provide adjustments to text annotation where applicable. Thanks to
  [Bartosz Bursa](https://github.com/bartekbursa) for the
  [suggestion](https://github.com/alastairrushworth/inspectdf/issues/20).
- changes to text annotation to improve how `coord_flip()` works on
  resulting plots. Thanks to [Roel
  Verbelen](https://github.com/RoelVerbelen) for the report.

## `inspectdf` 0.0.7

CRAN release: 2019-11-05

- Added `bytes` column to
  [`inspect_mem()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_mem.md)
  output, for downstream numeric comparison and consistency with
  `inspectpd`.
- Added `pcnt_nna` column to
  [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md)
  output containing the percentage of pairwise complete observations
  used calculated correlations. Thanks to Theo Broekman for the
  suggestion.
- Fixed bug causing order of grouping variable in grouped `inspect_`
  statements to be incorrect. Thanks to the report from Theo Broekman.
- Removed erroneous print statement from
  [`inspect_num()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_num.md).

## `inspectdf` 0.0.6

CRAN release: 2019-09-29

- Updates to documentation throughout.
- `inspect_*` functions now returns results by group grouped dataframes.
- Added option for `inspect_num() %>% show_plot()` to show histograms
  with color palettes specified by the `col_palette` argument.
- Fixed bug causing
  [`inspect_imb()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_imb.md)
  to sometimes fail when factors present. Thanks to [Doug
  Friedman](https://github.com/doug-friedman) for the
  [report](https://github.com/alastairrushworth/inspectdf/issues/19).

## `inspectdf` 0.0.5

CRAN release: 2019-08-26

- Fixed error causing
  [`inspect_num()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_num.md)
  to fail when columns contained all `NA` values. Thanks to [Ryan
  Tanner](https://github.com/ryanatanner) for the
  [report](https://github.com/alastairrushworth/inspectdf/issues/18)
- Speed-up of
  [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md)
  for large data frames with many numeric columns.
- Added approximate confidence intervals and tests for
  `method = 'kendall'` and `method = 'spearman'` in
  [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md).

## `inspectdf` 0.0.4

CRAN release: 2019-07-27

- Fix issue causing `inspect_na() %>% show_plot()` to fail when 0 `NA`
  present. Thanks to the
  [report](https://github.com/alastairrushworth/inspectdf/issues/13) by
  [Metin Yazici](https://github.com/strboul).
- [`show_plot()`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)
  now returns a `ggplot2` object rather than printing the plot - thanks
  to [Garrick Aden-Buie](https://github.com/gadenbuie) for the
  [suggestion](https://github.com/alastairrushworth/inspectdf/issues/14).
- Dramatic speed up of `inspect_cat` plotting by avoiding text labels
  for small regions.
- Added `tech` dataset.
- Fix for text annotation of
  [`inspect_cat()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cat.md)
  plots when labels are empty strings. By default `""` will be shown.
  Thanks to [Michael Swenson](https://github.com/mwswenson) for the
  [report](https://github.com/alastairrushworth/inspectdf/issues/12)
- `inspect_cor(method = ...)` argument added, thanks to suggestion from
  [George Dontas](https://github.com/gd047). Options for `pearson`,
  `spearman` and `kendall`. Note that confidence intervals and tests
  currently only supported for `pearson`.
- Fix error when duplicate factor labels present in
  [`inspect_cat()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cat.md)
  &
  [`inspect_imb()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_imb.md).

## `inspectdf` 0.0.3

CRAN release: 2019-06-27

- `text_labels` autoscale size using
  [`ggfittext::geom_fit_text()`](https://wilkox.org/ggfittext/reference/geom_fit_text.html).
  For an example see
  [`inspect_cat()`](https://github.com/alastairrushworth/inspectdf#categorical-levels).
  Thanks to [David Wilkins](https://github.com/wilkox) for the
  [PR](https://github.com/alastairrushworth/inspectdf/pull/9).
- 6 different color palettes supported in
  [`show_plot()`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)
  via `col_palette` argument. Colorblind friendly option specified via
  `show_plot(col_palette = 1)` - thanks to [Richard
  Careaga](https://github.com/technocrat) [for the
  suggestion](https://github.com/alastairrushworth/inspectdf/pull/3).
- [`inspect_imb()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_imb.md).
  - `include_na` option for categorical columns that are 100% missing,
    or constant are underlined in plot for easier comprehension.
- [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md)
  - Points and whiskers changed to coloured bands for single dataframe
    summaries - these are easier to see when CIs are narrow.\
  - Points changed to bars for
    [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md)
    comparison plots - makes it easier to see smaller differences in
    correlations.\
  - `NA` correlations omitted from
    [`inspect_cor()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cor.md)
    comparison when plotted. Ordering of correlations reversed to be
    consistent with returned tibble.

## `inspectdf` 0.0.2

CRAN release: 2019-05-23

- [`show_plot()`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)
  function (`show_plot` argument in `inspect_` functions will be dropped
  in a future version)
- `high_cardinality` argument in
  [`show_plot()`](https://alastairrushworth.github.io/inspectdf/reference/show_plot.md)
  for combining unique or near-unique categories for plotting
  [`inspect_cat()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cat.md).
- `progress` bars shown when processing larger datasets
- Improvements to plots throughout

## `inspectdf` 0.0.1

CRAN release: 2019-04-24

- Initial CRAN release
