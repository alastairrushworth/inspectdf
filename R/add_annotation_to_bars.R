#' @importFrom ggfittext geom_fit_text
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
                                   fill = NULL, parse = FALSE, 
                                   label_color = NULL, 
                                   label_size = NULL){
 
  # if any zero length characters, replace with double quotes
  z[nchar(z) == 0] <- NA
  # whether ys are zero or not
  y_zr <- y == 0
  big_bar <- thresh * max(y, na.rm = T)
  # label_df
  label_df <- tibble(x = x, y = y, z = z)
  if(is.null(fill)) label_df$fill <- NA else label_df$fill <- fill
  if(is.null(fill)){
    # labels white 
    label_white <- label_df %>% filter(y > big_bar) 
    max_lab <- ifelse(all(is.na(label_white$y)), NA, max(label_white$y, na.rm = T))
    # labels grey
    label_grey <- label_df %>% filter(y <= big_bar, !y_zr) %>%
      mutate(ymax = y + 0.5 * max_lab)
    # labels zero
    label_zero <- label_df %>% filter(y_zr)
  } else {
    label_white <- label_grey <- label_zero <- label_df
    label_white$z[y < big_bar] <- NA
    label_grey$z[y >= big_bar | y_zr] <- NA
    label_grey$ymax <- max(label_grey$y, na.rm = T)
    label_zero$z[!y_zr] <- NA
  }
  
  if(parse){
    # if percentage is exactly 0 or 100, then underline
    under_txt <- function(v){
      sprintf("underline(%s)", gsub(" ", "~", v, fixed = TRUE))
    }
    is_under <- y == 100 | y == 0
    z[is_under] <- under_txt(z[is_under])
    z[!is_under] <- gsub(" ", "~", z[!is_under]) 
  }
  # add white labels at the top of the bigger bars
  if(nrow(label_white) > 0){
    plt <- plt + suppressWarnings(
      geom_fit_text(
        aes(x = x, y = y, 
            label = z, 
            group = fill, 
            ymin = 0, ymax = y),
        data = label_white,
        color = 'white',
        angle = angle,
        inherit.aes = inherit.aes,
        na.rm = TRUE,
        size = ifelse(is.null(label_size), 12, label_size),
        min.size = 6,
        place = "top",
        contrast = TRUE,
        position = "dodge",
        outside = FALSE, 
        padding.y = grid::unit(2, "mm"))
    )
  }
  # add grey labels to relatively short bars, if any
  if(nrow(label_grey) > 0){
    # add a grey series to the smaller bars
    plt <- plt + suppressWarnings(
      geom_fit_text(aes(x = x, 
                        y = y,
                        label = z,
                        group = fill,
                        ymin = y, 
                        ymax = ymax),
                    data = label_grey,
                    colour = ifelse(is.null(label_color), "lightsteelblue4", label_color[2]),
                    angle = angle,
                    inherit.aes = inherit.aes,
                    na.rm = TRUE,
                    size = ifelse(is.null(label_size), 12, label_size),
                    place = "bottom",
                    min.size = 8,
                    contrast = TRUE,
                    position = "dodge",
                    outside = TRUE, 
                    padding.y = grid::unit(2, "mm"))
    )
  }
  # add 0 labels, if any
  if(nrow(label_zero) > 0){
    label_zero$y <- ifelse(all(y_zr), 0.01, 0.5 * max(y, na.rm = T))
    # add a grey series to the smaller bars
    plt <- plt + suppressWarnings(
      geom_fit_text(aes(x = x,
                        y = y,
                        label = z,
                        group = fill,
                        ymin = 0,
                        ymax = y),
                    data = label_zero,
                    colour = ifelse(is.null(label_color), "lightsteelblue4", label_color[2]),
                    angle = angle,
                    inherit.aes = inherit.aes,
                    na.rm = TRUE,
                    min.size = 8,
                    size = ifelse(is.null(label_size), 12, label_size),
                    place = "bottom",
                    contrast = TRUE,
                    position = "dodge",
                    outside = TRUE, 
                    padding.y = grid::unit(2, "mm"))
    )
  }
  # return the plot
  return(plt)
}
