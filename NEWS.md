
<!-- # `inspectdf` 0.0.3.9000 -->

<!-- - `show_plot()` now returns a `ggplot2` object rather than printing the plot - thanks to [Garrick Aden-Buie](https://github.com/gadenbuie) for the [suggestion](https://github.com/alastairrushworth/inspectdf/issues/14). -->

<!-- - Dramatic speed up of `inspect_cat` plotting by avoiding text labels for small regions. -->

<!-- - Added `tech` dataset. -->

<!-- - Fix for text annotation of `inspect_cat()` plots when labels are empty strings.  By default `""` will be shown.  Thanks to [Michael Swenson](https://github.com/mwswenson) for the [report](https://github.com/alastairrushworth/inspectdf/issues/12) -->

<!-- - `inspect_cor(method = ...)` argument added.  Options for `pearson`, `spearman` and `kendall`. -->

<!-- - Fix error when duplicate factor labels present in `inspect_cat()` & `inspect_imb()`. -->

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
