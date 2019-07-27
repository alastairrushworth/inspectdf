#' @importFrom ggplot2 ylim
# bar plot function
bar_plot <- function(df_plot, x, y, fill, label, xlb = "", ylb = "", 
                     ttl = "", sttl = "", lgnd = NULL, rotate = FALSE, 
                     col_palette = NULL, ylim_range = NULL){
  
  # basic bar plot
  plt <- df_plot %>% 
    ggplot(aes_string(x = x, y = y, fill = fill, label = label)) +
    geom_bar(stat = "identity") + 
    labs(x = xlb, y = ylb, title = ttl, subtitle = sttl) +
    scale_fill_manual(values = user_colours(nrow(df_plot), col_palette))
    
  # if legend is required give a name, otherwise remove
  if(is.null(lgnd)){
    plt <- plt + guides(fill = FALSE)
  } else {
    plt <- plt + scale_fill_discrete(name = lgnd)
  }
  # apply lower limit to the y-axis?
  if(!is.null(ylim_range)){
    plt <- plt + ylim(ylim_range)
  }
  # rotate x-axis labels if requested
  if(rotate){
    plt <- plt + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(plt)
}