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
                                   fill = NA, parse = FALSE){
  if(parse){
    # if percentage is exactly 0 or 100, then underline
    under_txt <- function(v) {
      sprintf("underline(%s)", gsub(" ", "~", v, fixed = TRUE))
    }
    is_under <- y == 100 | y == 0
    z[is_under] <- under_txt(z[is_under])
    z[!is_under] <- gsub(" ", "~", z[!is_under]) 
  }
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
                         position = position_dodge(width = dodged), 
                         parse = parse)
  # add a grey series to the smaller bars
  plt <- plt + geom_text(aes(x = x, y = y + nudge, label = z_grey, group = fill),
                         #nudge_y = nudge, 
                         color = "lightsteelblue4", 
                         angle = angle, 
                         hjust = hjust[1], inherit.aes = inherit.aes, 
                         na.rm = TRUE, size = size, 
                         position = position_dodge(width = dodged), 
                         parse = parse)
  # return the plot
  return(plt)
}