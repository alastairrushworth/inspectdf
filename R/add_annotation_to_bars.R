#' @importFrom ggplot2 aes
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme

# annotate a bar plot
add_annotation_to_bars <- function(x, y, z, dodged = 0, plt, thresh = 0.05, 
                                   nudge = 1, 
                                   angle = 90, 
                                   hjust = c("left", "right"), 
                                   size = 4, inherit.aes = FALSE, 
                                   position = "identity", 
                                   fill = NA){
  # two different label series
  z_white <- z_grey <- z
  z_white[y < (thresh * max(y, na.rm = T))] <- NA
  z_grey[y >= (thresh * max(y, na.rm = T))] <- NA
  # manual text nudge 
  nudge <- abs(diff(range(y, na.rm = T))) / 80
  # add a white series to the bigger bars
  plt <- plt + geom_text(aes(x = x, y = y - nudge, label = z_white, group = fill),
                         #nudge_y = -nudge, 
                         color = "white",
                         angle = angle, 
                         hjust = hjust[2], inherit.aes = inherit.aes, 
                         na.rm = TRUE, size = size, 
                         position = position_dodge(width = dodged))
  # add a grey series to the smaller bars
  plt <- plt + geom_text(aes(x = x, y = y + nudge, label = z_grey, group = fill),
                         #nudge_y = nudge, 
                         color = "lightsteelblue4", 
                         angle = angle, 
                         hjust = hjust[1], inherit.aes = inherit.aes, 
                         na.rm = TRUE, size = size, 
                         position = position_dodge(width = dodged))
  # return the plot
  return(plt)
}

# bar plot function
bar_plot <- function(df_plot, x, y, fill, label, xlb = "", ylb = "", 
                     ttl = "", sttl = "", lgnd = NULL, rotate = FALSE){
  # basic bar plot
  plt <- df_plot %>% 
    ggplot(aes_string(x = x, y = y, fill = fill, label = label)) + 
    geom_bar(stat = "identity") + 
    labs(x = xlb, y = ylb, title = ttl, subtitle = sttl)
  # if legend is required give a name, otherwise remove
  if(is.null(lgnd)){
    plt <- plt + guides(fill = FALSE)
  } else {
    plt <- plt + scale_fill_discrete(name = lgnd)
  }
  # rotate x-axis labels if requested
  if(rotate){
    plt <- plt + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(plt)
}