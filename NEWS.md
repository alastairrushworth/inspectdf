
# `inspectdf` 0.0.9

  - Minor change, ensuring all functions use `return` properly.

# `inspectdf` 0.0.8

  - **Important change:** the `show_plot` argument has been removed from
    all `inspect_*()` functions. To generate visualisations of data
    frame summaries, please use the more flexible
    `show_plot(inspect_*())` or via the pipe `inspect_*() %>%
    show_plot()`.
  - `show_plot()` improvement that nudges points that might otherwise
    have coincided for dataframe comparisons of imbalance (for example,
    with `inspect_imb(df1, df2) %>% show_plot()`)  
  - Plots for grouped summaries: `inspect_cor()`, `inspect_na()` and
    \`inspect.
  - `inspect_cor()` slight speed up for dataframes with large numbers of
    columns.
  - `inspect_cor()` can be filtered prior to plotting, for example
    `inspect_cor(starwars) %>% filter(abs(corr) > 0.2) %>% show_plot()`.
    Thanks to [Roel Verbelen](https://github.com/RoelVerbelen) for the
    [suggestion](https://github.com/alastairrushworth/inspectdf/issues/24)
  - Fixed bug causing `inspect_imb()` to fail on certain types of factor
    columns. Thanks to [Roel Verbelen](https://github.com/RoelVerbelen)
    for the
    [report](https://github.com/alastairrushworth/inspectdf/issues/26).
  - `show_plot()` has new arguments `label_size`, `label_angle` and
    `label_color`. Each provide adjustments to text annotation where
    applicable. Thanks to [Bartosz
    Bursa](https://github.com/bartekbursa) for the
    [suggestion](https://github.com/alastairrushworth/inspectdf/issues/20).
  - changes to text annotation to improve how `coord_flip()` works on
    resulting plots. Thanks to [Roel
    Verbelen](https://github.com/RoelVerbelen) for the report.

# `inspectdf` 0.0.7

  - Added `bytes` column to `inspect_mem()` output, for downstream
    numeric comparison and consistency with `inspectpd`.
  - Added `pcnt_nna` column to `inspect_cor()` output containing the
    percentage of pairwise complete observations used calculated
    correlations. Thanks to Theo Broekman for the suggestion.
  - Fixed bug causing order of grouping variable in grouped `inspect_`
    statements to be incorrect. Thanks to the report from Theo Broekman.
  - Removed erroneous print statement from `inspect_num()`.

# `inspectdf` 0.0.6

  - Updates to documentation throughout.
  - `inspect_*` functions now returns results by group grouped
    dataframes.
  - Added option for `inspect_num() %>% show_plot()` to show histograms
    with color palettes specified by the `col_palette` argument.
  - Fixed bug causing `inspect_imb()` to sometimes fail when factors
    present. Thanks to [Doug Friedman](https://github.com/doug-friedman)
    for the
    [report](https://github.com/alastairrushworth/inspectdf/issues/19).

# `inspectdf` 0.0.5

  - Fixed error causing `inspect_num()` to fail when columns contained
    all `NA` values. Thanks to [Ryan
    Tanner](https://github.com/ryanatanner) for the
    [report](https://github.com/alastairrushworth/inspectdf/issues/18)
  - Speed-up of `inspect_cor()` for large data frames with many numeric
    columns.
  - Added approximate confidence intervals and tests for `method =
    'kendall'` and `method = 'spearman'` in `inspect_cor()`.

# `inspectdf` 0.0.4

  - Fix issue causing `inspect_na() %>% show_plot()` to fail when 0 `NA`
    present. Thanks to the
    [report](https://github.com/alastairrushworth/inspectdf/issues/13)
    by [Metin Yazici](https://github.com/strboul).
  - `show_plot()` now returns a `ggplot2` object rather than printing
    the plot - thanks to [Garrick
    Aden-Buie](https://github.com/gadenbuie) for the
    [suggestion](https://github.com/alastairrushworth/inspectdf/issues/14).
  - Dramatic speed up of `inspect_cat` plotting by avoiding text labels
    for small regions.
  - Added `tech` dataset.
  - Fix for text annotation of `inspect_cat()` plots when labels are
    empty strings. By default `""` will be shown. Thanks to [Michael
    Swenson](https://github.com/mwswenson) for the
    [report](https://github.com/alastairrushworth/inspectdf/issues/12)
  - `inspect_cor(method = ...)` argument added, thanks to suggestion
    from [George Dontas](https://github.com/gd047). Options for
    `pearson`, `spearman` and `kendall`. Note that confidence intervals
    and tests currently only supported for `pearson`.
  - Fix error when duplicate factor labels present in `inspect_cat()` &
    `inspect_imb()`.

# `inspectdf` 0.0.3

  - `text_labels` autoscale size using `ggfittext::geom_fit_text()`. For
    an example see
    [`inspect_cat()`](https://github.com/alastairrushworth/inspectdf#categorical-levels).
    Thanks to [David Wilkins](https://github.com/wilkox) for the
    [PR](https://github.com/alastairrushworth/inspectdf/pull/9).
  - 6 different color palettes supported in `show_plot()` via
    `col_palette` argument. Colorblind friendly option specified via
    `show_plot(col_palette = 1)` - thanks to [Richard
    Careaga](https://github.com/technocrat) [for the
    suggestion](https://github.com/alastairrushworth/inspectdf/pull/3).
  - `inspect_imb()`.
      - `include_na` option for categorical columns that are 100%
        missing, or constant are underlined in plot for easier
        comprehension.
  - `inspect_cor()`
      - Points and whiskers changed to coloured bands for single
        dataframe summaries - these are easier to see when CIs are
        narrow.  
      - Points changed to bars for `inspect_cor()` comparison plots -
        makes it easier to see smaller differences in correlations.  
      - `NA` correlations omitted from `inspect_cor()` comparison when
        plotted. Ordering of correlations reversed to be consistent with
        returned tibble.

# `inspectdf` 0.0.2

  - `show_plot()` function (`show_plot` argument in `inspect_` functions
    will be dropped in a future version)
  - `high_cardinality` argument in `show_plot()` for combining unique or
    near-unique categories for plotting `inspect_cat()`.
  - `progress` bars shown when processing larger datasets
  - Improvements to plots throughout

# `inspectdf` 0.0.1

  - Initial CRAN release
