# Simple graphical inspection of data frame summaries

Easily visualise output from `inspect_*()` functions.

## Usage

``` r
show_plot(x, ...)
```

## Arguments

- x:

  Data frame resulting from the output of an `inspect_*()` function.

- ...:

  Optional arguments that modify the plot output, see Details.

## Value

A `ggplot2` object visualizing the inspection results.

## Details

**Generic arguments for all plot type**

- `text_labels`:

  Boolean. Whether to show text annotation on plots. Defaults to `TRUE`.

- `label_color`:

  Character string or character vector specifying colors for text
  annotation, if applicable. Usually defaults to white and gray.

- `label_angle`:

  Numeric value specifying angle with which to rotate text annotation,
  if applicable. Defaults to 90 for most plots.

- `label_size`:

  Numeric value specifying font size for text annotation, if applicable.

- `col_palette`:

  Integer indicating the colour palette to use: `0`: (default)
  \`ggplot2\` color palette, `1`: [colorblind friendly
  palette](http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/), `2`:
  [80s theme](https://www.color-hex.com/color-palette/25888), `3`:
  [rainbow theme](https://www.color-hex.com/color-palette/79261), `4`:
  [mario theme](https://www.color-hex.com/color-palette/78663), `5`:
  [pokemon theme](https://www.color-hex.com/color-palette/78664)

**Arguments for plotting
[`inspect_cat()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cat.md)**

- `high_cardinality`:

  Minimum number of occurrences of category to be shown as a distinct
  segment in the plot
  ([`inspect_cat()`](https://alastairrushworth.github.io/inspectdf/reference/inspect_cat.md)
  only). Default is 0 - all distinct levels are shown. Setting
  `high_cardinality > 0` can speed up plot rendering when categorical
  columns contain many near-unique values.

- `label_thresh`:

  Minimum occurrence frequency of category for a text label to be shown.
  Smaller values of `label_thresh` will show labels for less common
  categories but at the expense of increased plot rendering time.
  Defaults to 0.1.

**Other arguments**

- `plot_type`:

  Experimental. Integer determining plot type to print. Defaults to 1.

- `plot_layout`:

  Vector specifying the number of rows and columns in the plotting grid.
  For example, 3 rows and 2 columns would be specified as
  `plot_layout = c(3, 2)`.

## Author

Alastair Rushworth

## Examples

``` r
# Load 'starwars' data
data("starwars", package = "dplyr")

# Horizontal bar plot for categorical column composition
x <- inspect_cat(starwars) 
show_plot(x)


# Correlation between numeric columns + confidence intervals
x <- inspect_cor(starwars)
show_plot(x)


# Bar plot of most frequent category for each categorical column
x <- inspect_imb(starwars)
show_plot(x)


# Bar plot showing memory usage for each column
x <- inspect_mem(starwars)
show_plot(x)


# Occurence of NAs in each column ranked in descending order
x <- inspect_na(starwars)
show_plot(x)


# Histograms for numeric columns
x <- inspect_num(starwars)
show_plot(x)


# Barplot of column types
x <- inspect_types(starwars)
show_plot(x)
```
