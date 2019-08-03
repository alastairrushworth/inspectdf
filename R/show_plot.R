#' Visualise summaries and comparisons of one or two dataframes.
#' 
#' @param x Dataframe resulting from a call to an `inspect_` function.
#' @param alpha Alpha level for performing significance tests.  Defaults to 0.05.
#' @param text_labels Whether to show text annotation on plots (when \code{show_plot = T}). 
#' @param high_cardinality Minimum number of occurrences of category to be shown as a distinct segment 
#' in the plot (\code{inspect_cat} only).  Default is 0.  This can help when some columns 
#' contain many unique or near-unique levels that take a long time to render.
#' @param col_palette Integer indicating the colour palette to use.
#' 
#'  - `0`: (default) `ggplot2` color palette
#'  - `1`: a [colorblind friendly palette](http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
#'  - `2`: [80s theme](https://www.color-hex.com/color-palette/25888)
#'  - `3`: [rainbox theme](https://www.color-hex.com/color-palette/79261)
#'  - `4`: [mario theme](https://www.color-hex.com/color-palette/78663)
#'  - `5`: [pokemon theme](https://www.color-hex.com/color-palette/78664)
#' @param plot_type String determining the type of plot to show.  Defaults to `"bar"`.  
#' @param plot_layout Vector specifying the number of rows and columns 
#' in the plotting grid.  For example, 3 rows and 2 columns would be specified as 
#' \code{plot_layout = c(3, 2)}.
#' Default is \code{TRUE}.
#' @param label_thresh Minimum percentage frequency of category for a text label to be shown.
#' Defaults to 0.1.  Smaller values will show potentially smaller labels, but at the expense of longer
#' rendering time.
#' @export
#' @examples 
#' # Load 'starwars' data
#' data("starwars", package = "dplyr")
#' 
#' # categorical plot
#' x <- inspect_cat(starwars) 
#' show_plot(x)
#' 
#' # correlations in numeric columns
#' x <- inspect_cor(starwars)
#' show_plot(x)
#' 
#' # feature imbalance bar plot
#' x <- inspect_imb(starwars)
#' show_plot(x)
#' 
#' # memory usage barplot
#' x <- inspect_mem(starwars)
#' show_plot(x)
#' 
#' # missingness barplot
#' x <- inspect_na(starwars)
#' show_plot(x)
#' 
#' # histograms for numeric columns
#' x <- inspect_num(starwars)
#' show_plot(x)
#' 
#' # barplot of column types
#' x <- inspect_types(starwars)
#' show_plot(x)

show_plot <- function(x, text_labels = TRUE, alpha = 0.05, 
                      high_cardinality = 0, plot_layout = NULL,
                      col_palette = 0, plot_type = "bar", label_thresh = 0.1){
  type     <- attr(x, "type")
  df_names <- attr(x, "df_names")
  
  # categorical plots
  if(type$method == "cat"){
    plt <- plot_cat(x, df_names = df_names,
                    text_labels = text_labels, 
                    high_cardinality = high_cardinality, 
                    col_palette = col_palette, 
                    label_thresh = label_thresh)
  }
  
  # correlation plots
  if(type$method == "cor"){
    method   <- attr(x, "method")
    if(type$input_type == "single"){
      x$pair   <- attr(x, "pair")
      plt <- plot_cor_single(x, 
                             df_names = df_names, 
                             alpha = alpha,
                             text_labels = text_labels, 
                             col_palette = col_palette,
                             method      = method)
    }
    if(type$input_type == "pair"){
      plt <- plot_cor_pair(x, 
                           df_names = df_names, 
                           alpha = alpha,
                           text_labels = text_labels, 
                           col_palette = col_palette, 
                           method      = method)
    }
    if(type$input_type == "grouped"){
      plt <- plot_cor_grouped(x, 
                              df_names = df_names,
                              text_labels = text_labels, 
                              col_palette = col_palette, 
                              method      = method, 
                              plot_type   = plot_type)
    }
  }
  
  # imbalance plots
  if(type$method == "imb"){
    if(type$input_type == "single"){
      plt <- plot_imb_1(x, df_names = df_names,
                        text_labels = text_labels, 
                        col_palette = col_palette)
    }
    if(type$input_type == "pair"){
      plt <- plot_imb_2(x, df_names = df_names, alpha = alpha,
                        text_labels = text_labels, 
                        col_palette = col_palette)
    }
    if(type$input_type == "grouped"){
      plt <- plot_na_grouped(x, df_names = df_names,
                             text_labels = text_labels, 
                             col_palette = col_palette, 
                             plot_type = plot_type)
    }
  }
  
  # memory plots
  if(type$method == "mem"){
    sizes <- attr(x, "sizes")
    if(type[[2]] == 1){
      plt <- plot_mem_1(x, df_names = df_names, 
                        text_labels = text_labels, 
                        sizes = sizes, 
                        col_palette = col_palette)
    } else {
      plt <- plot_mem_2(x, df_names = df_names,
                        text_labels = text_labels, 
                        sizes = sizes, 
                        col_palette = col_palette)
    }
  }
  
  # missingness plots
  if(type$method == "na"){
    if(type$input_type == "single"){
      plt <- plot_na_single(x, df_names = df_names,
                            text_labels = text_labels, 
                            col_palette = col_palette)
    }
    if(type$input_type == "pair"){
      plt <- plot_na_pair(x, df_names = df_names, 
                          alpha = alpha,
                          text_labels = text_labels, 
                          col_palette = col_palette)
    }
    if(type$input_type == "grouped"){
      plt <- plot_na_grouped(x, df_names = df_names,
                             text_labels = text_labels, 
                             col_palette = col_palette, 
                             plot_type = plot_type)
    }
  }
  
  # numeric plots
  if(type$method == "num"){
    if(type[[2]] == 1){
      plt <- plot_num_1(x, df_names = df_names,
                        text_labels = text_labels, 
                        plot_layout = plot_layout)
    } else {
      plt <- plot_num_2(x, df_names = df_names, alpha = alpha,
                        text_labels = text_labels, 
                        plot_layout = plot_layout)
    }
  }
  
  # types plots
  if(type$method == "types"){
    if(type[[2]] == 1){
      plt <- plot_types_1(x, df_names = df_names,
                          text_labels = text_labels, 
                          col_palette = col_palette)
    } else {
      plt <- plot_types_2(x, df_names = df_names,
                          text_labels = text_labels,
                          col_palette = col_palette)
    }
  }
  suppressWarnings(plt)
}
