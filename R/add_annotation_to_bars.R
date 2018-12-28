add_annotation_to_bars <- function(x, y, z, plt, thresh = 0.05){
  # two different label series
  z_white <- z_grey <- z
  z_white[y < (thresh * max(y))] <- NA
  z_grey[y >= (thresh * max(y))] <- NA
  # add a white series to the bigger bars
  plt <- plt + ggplot2::geom_text(aes(x = x, y = y, label = z_white),
                                  nudge_y = -1, color = "white", angle = 90, 
                                  hjust = "right", inherit.aes = F, na.rm = TRUE)
  # add a grey series to the smaller bars
  plt <- plt + ggplot2::geom_text(aes(x = x, y = y, label = z_grey),
                                  nudge_y = 1, color = "lightsteelblue4", angle = 90, 
                                  hjust = "left", inherit.aes = F, na.rm = TRUE)
  # return the plot
  return(plt)
}