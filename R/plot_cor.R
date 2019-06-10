#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_blank
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw

plot_cor_1 <- function(out, alpha, df_names, text_labels, col_palette){
  # get a vector of theme colours
  vcols <- c("gray50", user_colours(9, col_palette)[9])
  # factorise pairs, add a sign variable and an index
  out <- out %>% 
    filter(!is.na(corr)) %>%
    mutate(pair = factor(pair, levels = as.character(pair)),
           sign = as.factor(c("Negative", "Positive")[as.numeric(corr > 0) + 1]), 
           index = nrow(.):1) 
  # generate coloured CIs and point estimate plot
  plt <- ggplot(out, aes(x = pair, y = corr)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    geom_rect(
      alpha = 0.4,
      xmin = out$index - 0.4, xmax = out$index + 0.4,
      ymin = out$lower, ymax = out$upper, linetype = 1, 
      fill = vcols[(out$p_value < alpha) + 1]) +
    geom_segment(x = out$index - 0.4, y = out$corr, xend = out$index + 0.4, 
                 yend = out$corr, col = "gray40") +
    coord_flip() + ylim(min(out$lower), max(out$upper)) + 
    labs(x = "", y = bquote("Pearson correlation (\u03C1)"),
         title =  paste0("Correlation of columns in df::", df_names$df1))
  # print plot
  print(plt)
}


plot_cor_2 <- function(out, alpha, df_names, text_labels){
  out <- out %>%
    mutate(pair = paste(col_1, col_2, sep = " & ")) %>%
    mutate(pair = factor(pair, levels = as.character(pair))) %>%
    select(-col_1, -col_2) %>% 
    gather(key = "data_frame", value = "corr", -pair, -p_value) %>%
    mutate(data_frame = unlist(df_names)[as.integer(gsub("corr_", "", data_frame))])
  p_val_tab <- out %>% 
    mutate(is_sig = as.integer(p_value < alpha) + 1, index = 1:nrow(out)) %>%
    select(is_sig, index) 
  # generate basic plot
  plt <- ggplot(out, aes(x = as.factor(pair), y = corr, 
                             colour = data_frame)) +
    geom_blank() + theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
    geom_rect(
      fill = c("darkorange2", "royalblue1")[p_val_tab$is_sig], alpha = 0.2,
      xmin = p_val_tab$index - 0.4, xmax = p_val_tab$index + 0.4,
      ymin = -2, ymax = 2, linetype = "blank") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    geom_point(size = 1.25 * dot_size(nrow(out)), color = "black", na.rm = TRUE) + 
    geom_point(size = dot_size(nrow(out)), na.rm = TRUE) +
    coord_flip() + 
    labs(y = bquote("Pearson correlation (\u03C1)"), x = "",
         title =  paste0("Comparison of \u03C1 between df::", df_names$df1, 
                         " and ", df_names$df2),
         subtitle = bquote("Blue/orange stripes represent inequality/equality of \u03C1")) + 
    scale_color_discrete(name = "Data frame")
  # print plot
  print(plt)
}
