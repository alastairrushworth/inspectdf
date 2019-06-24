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
  plt <- plt + ggfittext::geom_bar_text(
    angle = 90,
    size = 24,
    min.size = 6,
    position = "dodge",
    contrast = TRUE,
    colour = "gray20"
  )
  # return the plot
  return(plt)
}
