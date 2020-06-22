#' Simple graphical inspection of dataframe summaries
#' 
#' @description  Easily visualise output from \code{inspect_*()} functions.
#' 
#' @param x Dataframe resulting from a call to an \code{inspect_*()} function.
#' @param alpha Alpha level for performing any significance tests.  Defaults to 0.05.
#' @param text_labels Boolean.  Whether to show text annotation on plots.  Defaults to \code{TRUE}.
#' @param high_cardinality Minimum number of occurrences of category to be shown as a distinct segment 
#' in the plot (\code{inspect_cat()} only).  Default is 0 - all distinct levels are shown.  Setting 
#' \code{high_cardinality > 0} can speed up plot rendering when categorical columns contain 
#' many near-unique values.
#' @param col_palette Integer indicating the colour palette to use:
#' \itemize{
#' \item \code{0}: (default) `ggplot2` color palette
#' \item \code{1}: a \href{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}{colorblind friendly palette}
#' \item \code{2}: \href{https://www.color-hex.com/color-palette/25888}{80s theme}
#' \item \code{3}: \href{https://www.color-hex.com/color-palette/79261}{rainbox theme}
#' \item \code{4}: \href{https://www.color-hex.com/color-palette/78663}{mario theme}
#' \item \code{5}: \href{https://www.color-hex.com/color-palette/78664}{pokemon theme}
#' }
#' @param plot_type Experimental.  Integer determining plot type to print.  Defaults to 1.
#' @param plot_layout Vector specifying the number of rows and columns 
#' in the plotting grid.  For example, 3 rows and 2 columns would be specified as 
#' \code{plot_layout = c(3, 2)}.
#' @param label_color Character string or character vector specifying colors for text annotation, 
#' if applicable.  Usually defaults to white and gray.
#' @param label_angle Numeric value specifying angle with which to rotate text annotation, 
#' if applicable.  Defaults to 90 for most plots.
#' @param label_size Numeric value specifying font size for text annotation, if applicable.
#' @param label_thresh (\code{inspect_cat()} only.  Minimum occurrence frequency of category for 
#' a text label to be shown.  Smaller values of \code{label_thresh} will show labels 
#' for less common categories but at the expense of increased plot rendering time.  Defaults to 0.1. 
#' @export
#' @examples 
#' # Load 'starwars' data
#' data("starwars", package = "dplyr")
#' 
#' # Horizontal bar plot for categorical column composition
#' x <- inspect_cat(starwars) 
#' show_plot(x)
#' 
#' # Correlation betwee numeric columns + confidence intervals
#' x <- inspect_cor(starwars)
#' show_plot(x)
#' 
#' # Bar plot of most frequent category for each categorical column
#' x <- inspect_imb(starwars)
#' show_plot(x)
#' 
#' # Bar plot showing memory usage for each column
#' x <- inspect_mem(starwars)
#' show_plot(x)
#' 
#' # Occurence of NAs in each column ranked in descending order
#' x <- inspect_na(starwars)
#' show_plot(x)
#' 
#' # Histograms for numeric columns
#' x <- inspect_num(starwars)
#' show_plot(x)
#' 
#' # Barplot of column types
#' x <- inspect_types(starwars)
#' show_plot(x)

show_plot <- function(x, text_labels = TRUE, 
                      alpha = 0.05, 
                      high_cardinality = 0,
                      plot_layout = NULL,
                      col_palette = 0, 
                      plot_type = 1, 
                      label_thresh = 0.1, 
                      label_angle = NULL, 
                      label_color = NULL,
                      label_size = NULL
                      ){
  type     <- attr(x, "type")
  df_names <- attr(x, "df_names")
  
  # categorical plots
  if(type$method == "cat"){
    if(type$input_type == "grouped") stop("Grouped plots for inspect_cat() not yet implemented.")
    plt <- plot_cat(x, df_names = df_names,
                    text_labels = text_labels, 
                    high_cardinality = high_cardinality, 
                    col_palette = col_palette, 
                    label_thresh = label_thresh, 
                    label_angle = label_angle, 
                    label_color = label_color,
                    label_size  = label_size)
  }
  
  # correlation plots
  if(type$method == "cor"){
    method   <- attr(x, "method")
    if(type$input_type == "single"){
      x$pair <- paste(x$col_1, x$col_2, sep = ' & ')
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
                        col_palette = col_palette, 
                        label_angle = label_angle, 
                        label_color = label_color,
                        label_size  = label_size)
    }
    if(type$input_type == "pair"){
      plt <- plot_imb_2(x, df_names = df_names, alpha = alpha,
                        text_labels = text_labels, 
                        col_palette = col_palette)
    }
    if(type$input_type == "grouped"){
      plt <- plot_imb_grouped(x, df_names = df_names,
                              text_labels = text_labels, 
                              col_palette = col_palette, 
                              plot_type = plot_type)
    }
  }
  
  # memory plots
  if(type$method == "mem"){
    sizes <- attr(x, "sizes")
    if(type$input_type == "single"){
      plt <- plot_mem_1(x, df_names = df_names, 
                        text_labels = text_labels, 
                        sizes       = sizes, 
                        col_palette = col_palette,
                        label_angle = label_angle, 
                        label_color = label_color,
                        label_size  = label_size)
    }
    if(type$input_type == "pair"){
      plt <- plot_mem_2(x, df_names = df_names,
                        text_labels = text_labels, 
                        sizes = sizes, 
                        col_palette = col_palette, 
                        label_angle = label_angle, 
                        label_color = label_color,
                        label_size  = label_size)
    }
    if(type$input_type == "grouped") stop("Grouped plots for inspect_mem() not yet implemented.")
  }
  
  # missingness plots
  if(type$method == "na"){
    if(type$input_type == "single"){
      plt <- plot_na_single(x, df_names = df_names,
                            text_labels = text_labels, 
                            col_palette = col_palette, 
                            label_angle = label_angle, 
                            label_color = label_color,
                            label_size = label_size)
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
    if(type$input_type == "single"){
      plt <- plot_num_1(x, df_names = df_names,
                        text_labels = text_labels, 
                        plot_layout = plot_layout, 
                        col_palette = col_palette)
    }
    if(type$input_type == "pair"){
      plt <- plot_num_2(x, df_names = df_names, alpha = alpha,
                        text_labels = text_labels, 
                        plot_layout = plot_layout)
    }
    if(type$input_type == "grouped"){
      plt <- plot_num_3(x, df_names = df_names, alpha = alpha,
                        text_labels = text_labels, 
                        plot_layout = plot_layout, 
                        col_palette = col_palette)
    }
  }
  
  # types plots
  if(type$method == "types"){
    if(type[[2]] == 1){
      plt <- plot_types_1(x, df_names = df_names,
                          text_labels = text_labels, 
                          col_palette = col_palette, 
                          label_angle = label_angle, 
                          label_color = label_color,
                          label_size  = label_size)
    } else {
      plt <- plot_types_2(x, df_names = df_names,
                          text_labels = text_labels,
                          col_palette = col_palette, 
                          label_angle = label_angle, 
                          label_color = label_color,
                          label_size  = label_size)
    }
  }
  suppressWarnings(plt)
}
