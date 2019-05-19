#' Visualise summaries and comparisons of one or two dataframes.
#' 
#' @param x Dataframe resulting from a call to an `inspect_` function.
#' @param alpha Alpha level for performing significance tests.  Defaults to 0.05.
#' @param text_labels Whether to show text annotation on plots (when \code{show_plot = T}). 
#' @param high_cardinality Minimum number of occurrences of category to be shown as a distinct segment 
#' in the plot (\code{inspect_cat} only).  Default is 0.  This can help when some columns 
#' contain many unique or near-unique levels that take a long time to render.
#' @param cols Vector containing names or integers indicating colours for the plotted bars for levels,
#' missing values and for high cardinality values, respectively.
#' @param plot_layout Vector specifying the number of rows and columns 
#' in the plotting grid.  For example, 3 rows and 2 columns would be specified as 
#' \code{plot_layout = c(3, 2)}.
#' Default is \code{TRUE}.
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
                      cols = c("tomato3", "gray65", "darkmagenta")){
  type     <- attr(x, "type")
  df_names <- attr(x, "df_names")
  
  # categorical plots
  if(type[[1]] == "cat"){
      plot_cat(x, df_names = df_names,
               text_labels = text_labels, 
               high_cardinality = high_cardinality, 
               cols = cols)
  }
  
  # correlation plots
  if(type[[1]] == "cor"){
    if(type[[2]] == 1){
      x$pair <- attr(x, "pair")
      plot_cor_1(x, df_names = df_names, alpha = alpha,
                 text_labels = text_labels)
    } else {
      plot_cor_2(x, df_names = df_names, alpha = alpha,
                 text_labels = text_labels)
    }
  }
  
  # imbalance plots
  if(type[[1]] == "imb"){
    if(type[[2]] == 1){
      plot_imb_1(x, df_names = df_names,
                 text_labels = text_labels)
    } else {
      plot_imb_2(x, df_names = df_names, alpha = alpha,
                 text_labels = text_labels)
    }
  }
  
  # memory plots
  if(type[[1]] == "mem"){
    sizes <- attr(x, "sizes")
    if(type[[2]] == 1){
      plot_mem_1(x, df_names = df_names, 
                 text_labels = text_labels, sizes = sizes)
    } else {
      plot_mem_2(x, df_names = df_names,
                 text_labels = text_labels, sizes = sizes)
    }
  }
  
  # missingness plots
  if(type[[1]] == "na"){
    if(type[[2]] == 1){
      plot_na_1(x, df_names = df_names,
                text_labels = text_labels)
    } else {
      plot_na_2(x, df_names = df_names, alpha = alpha,
                text_labels = text_labels)
    }
  }
  
  # missingness plots
  if(type[[1]] == "num"){
    if(type[[2]] == 1){
      plot_num_1(x, df_names = df_names,
                 text_labels = text_labels, 
                 plot_layout = plot_layout)
    } else {
      plot_num_2(x, df_names = df_names, alpha = alpha,
                 text_labels = text_labels, 
                 plot_layout = plot_layout)
    }
  }
  
  # types plots
  if(type[[1]] == "types"){
    if(type[[2]] == 1){
      plot_types_1(x, df_names = df_names,
                   text_labels = text_labels)
    } else {
      plot_types_2(x, df_names = df_names,
                  text_labels = text_labels)
    }
  }
}
