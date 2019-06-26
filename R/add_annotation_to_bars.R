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
add_annotation_to_bars <- function(x, y, z, dodged = 0, plt, thresh = 0.07, 
                                   nudge = 1, 
                                   angle = 90, 
                                   hjust = c("left", "right"), 
                                   size = 24, inherit.aes = FALSE, 
                                   position = "identity", 
                                   fill = NA, parse = FALSE){
 
  # two different label series
  z_white <- z_grey <- z
  z_white[y < (thresh * max(y, na.rm = T))] <- NA
  z_grey[y >= (thresh * max(y, na.rm = T))] <- NA
  nudge <- abs(diff(range(y, na.rm = T))) / 80
  
  if(parse){
    # if percentage is exactly 0 or 100, then underline
    under_txt <- function(v){
      sprintf("underline(%s)", gsub(" ", "~", v, fixed = TRUE))
    }
    is_under <- y == 100 | y == 0
    z[is_under] <- under_txt(z[is_under])
    z[!is_under] <- gsub(" ", "~", z[!is_under]) 
  }

  # add a white series to the bigger bars
  plt <- plt + ggfittext::geom_fit_text(aes(x = x, y = y - nudge, label = z_white, 
                                            group = fill),
                                 colour = "white",
                                 angle = angle,
                                 inherit.aes = inherit.aes,
                                 na.rm = TRUE,
                                 size = 12,
                                 min.size = 6,
                                 place = "bottom",
                                 contrast = TRUE,
                                 position = "dodge",
                                 outside = TRUE)
  
  # add a grey series to the smaller bars
  plt <- plt + ggfittext::geom_fit_text(aes(x = x, y = y + nudge, label = z_grey, 
                                            group = fill),
                                 colour = "lightsteelblue4",
                                 angle = angle,
                                 inherit.aes = inherit.aes,
                                 na.rm = TRUE,
                                 size = 12,
                                 min.size = 6,
                                 place = "top",
                                 contrast = TRUE,
                                 position = "dodge",
                                 outside = TRUE)

  # return the plot
  return(plt)
}
