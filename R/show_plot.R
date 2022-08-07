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
#' \item \code{3}: \href{https://www.color-hex.com/color-palette/79261}{rainbow theme}
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

show_plot <- function(x, ...){
  type     <- attr(x, "type")
  stat_type <- type$method
  inspect_type <- type$input_type
  
  # categorical plots
  if(stat_type == "cat"){
    if(inspect_type == "grouped") ("Grouped plots for inspect_cat() not yet implemented.")
    plt <- plot_cat(x, ...)
  }
  
  # correlation plots
  if(stat_type == "cor"){
    plt <- switch(
      inspect_type, 
      single = plot_cor_single(x, ...), 
      pair = plot_cor_pair(x, ...), 
      grouped = plot_cor_grouped(x, ...)
    )
  }
  
  # imbalance plots
  if(stat_type == "imb"){
    plt <- switch(
      inspect_type, 
      single = plot_imb_single(x, ...), 
      pair = plot_imb_pair(x, ...), 
      grouped = plot_imb_grouped(x, ...)
    )
  }

  # memory plots
  if(stat_type == "mem"){
    plt <- switch(
      inspect_type, 
      single = plot_mem_single(x, ...), 
      pair = plot_mem_pair(x, ...), 
      grouped = stop("Grouped plots for inspect_mem() not yet implemented.")
    )
  }

  # missingness plots
  if(stat_type == "na"){
    plt <- switch(
      inspect_type, 
      single = plot_na_single(x, ...), 
      pair = plot_na_pair(x, ...), 
      grouped = plot_na_grouped(x, ...)
    )
  }
  
  # numeric plots
  if(stat_type == "num"){
    plt <- switch(
      inspect_type, 
      single = plot_num_single(x, ...), 
      pair = plot_num_pair(x, ...), 
      grouped = plot_num_grouped(x, ...)
    )
  }
  
  # types plots
  if(stat_type == "types"){
    print(inspect_type)
    plt <- switch(
      inspect_type, 
      single = plot_types_single(x, ...), 
      pair = plot_types_pair(x, ...), 
      grouped = plot_num_grouped(x, ...)
    )
  }
  suppressWarnings(plt)
}
