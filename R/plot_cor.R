#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_blank
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw

plot_cor_1 <- function(out, alpha, df_names, text_labels, col_palette, method){
  
  # xlabels
  mth_ind <- grep(paste0("^", method), c("pearson", "kendall", "spearman"), ignore.case = TRUE)
  xlab    <- c("Pearson's correlation", 
               "Kendall's rank correlation", 
               "Spearman's rank correlation")[mth_ind]
  # get a vector of signficance colors from theme
  vcols <- c("gray50", user_colours(9, col_palette)[9])
  # factorise pairs, add a sign variable and an index
  out <- out %>% 
    filter(!is.na(corr)) %>%
    mutate(pair = factor(pair, levels = rev(unique(as.character(pair)))),
           sign = as.factor(c("Negative", "Positive")[as.numeric(corr > 0) + 1])) %>%
    arrange(desc(pair)) %>%
    mutate(index = nrow(.):1)
  # generate coloured CIs and point estimate plot
  plt <- ggplot(out, aes(x = pair, y = corr)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    geom_rect(
      alpha = 0.4,
      xmin = out$index - 0.4, xmax = out$index + 0.4,
      ymin = out$lower, ymax = out$upper, linetype = 1, 
      fill = vcols[(out$p_value < alpha) + 1], 
      na.rm = TRUE) +
    geom_segment(x = out$index - 0.4, y = out$corr, xend = out$index + 0.4, 
                 yend = out$corr, col = "gray40") +
    coord_flip() + ylim(min(out$lower), max(out$upper)) + 
    labs(x = "", y = xlab, title = paste0("Correlation of columns in df::", 
                                          df_names$df1))
  # print plot
  print(plt)
}


plot_cor_2 <- function(out, alpha, df_names, text_labels, col_palette, method){
  # xlabels
  mth_ind <- grep(tolower(paste0("^", method)), c("pearson", "kendal", "spearman"))
  xlab    <- c("Pearson's correlation", 
               "Kendall's rank correlation", 
               "Spearman's rank correlation")[mth_ind]
  # create tall data from correlation table
  out <- out %>%
    filter(!is.na(corr_1), !is.na(corr_2)) %>%
    mutate(pair = paste(col_1, col_2, sep = " & ")) %>%
    mutate(pair = factor(pair, levels = rev(unique(as.character(pair))))) %>%
    select(-col_1, -col_2) %>% 
    gather(key = "data_frame", value = "corr", -pair, -p_value) %>%
    mutate(data_frame = unlist(df_names)[as.integer(gsub("corr_", "", data_frame))],
           data_frame_n = as.integer(as.factor(data_frame)))
  # number of dataframes
  n_df <- length(unique(out$data_frame))
  # get a vector of signficance colors from theme
  vcols <- c("gray50", user_colours(9, col_palette)[9])
  # get a vector of bar colors from theme
  if(n_df == 2){
    bcols <- get_best_pair(col_palette)
  } else {
    bcols <- user_colours(n_df, col_palette)
  }
  # add bar colors, significance & bar positions
  out <- out %>% 
    mutate(index = as.integer(pair)) %>%
    mutate(data_frame_n = n_df - data_frame_n + 1) %>%
    mutate(bar_dn = index - 0.8 + data_frame_n * (0.8 / n_df)) %>%
    mutate(bar_up = index - 0.8 + (data_frame_n + 1) * (0.8 / n_df)) %>%
    mutate(bar_bg = ifelse(corr < 0, corr, 0), bar_en = ifelse(corr < 0, 0, corr)) 
  # generate basic plot
  plt <- out %>%
    ggplot(aes(x = pair, y = corr, fill = data_frame)) +
    geom_blank() + theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
    geom_rect(
      fill = vcols[as.integer(out$p_value < alpha) + 1], alpha = 0.2,
      xmin = out$index - 0.4, xmax = out$index + 0.4,
      ymin = -2, ymax = 2, linetype = "blank") +
    geom_rect(
      linetype = "blank",
      xmin = out$bar_dn, xmax = out$bar_up,
      ymin = out$bar_bg, ymax = out$bar_en) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    coord_flip() + 
    labs(y = xlab, x = "",
         title = paste0("Comparison of \u03C1 between df::", df_names$df1, 
                        " and ", df_names$df2)) +
    scale_fill_manual(name = "Data frame", values = bcols)
  # print plot
  print(plt)
}

# > new_out
# # A tibble: 63 x 5
# corr   p_value pair                   data_frame data_frame_n
# <dbl>     <dbl> <fct>                  <fct>             <int>
#   1  0.244    9.31e-36 pressure & long        -1                    1
# 2 -0.212    2.98e-27 pressure & month       -1                    1
# 3 -0.198    7.59e-24 pressure & year        -1                    1
# 4 -0.192    1.88e-22 pressure & wind        -1                    1
# 5  0.0341   8.55e- 2 pressure & hour        -1                    1
# 6  0.0288   1.46e- 1 pressure & day         -1                    1
# 7  0.00199  9.20e- 1 pressure & lat         -1                    1
# 8 NA       NA        pressure & ts_diameter -1                    1
# 9 NA       NA        pressure & hu_diameter -1                    1
# 10 -0.695    0.       pressure & wind        0                     2


plot_cor_3 <- function(out, df_names, text_labels, col_palette, method, 
                       plot_type){
  # group variable name
  group_name <- colnames(out)[1]
  # if group column is not factor, coerce to one
  if(sapply(out, class)[1] != "factor"){
    out[group_name] <- as.factor(unlist(out[group_name]))
  }
  new_out <- out %>%
    mutate(pair = paste(col_1, col_2, sep = " & ")) %>%
    mutate(pair = factor(pair, levels = rev(unique(as.character(pair))))) %>%
    select(-col_1, -col_2, -lower, -upper) %>%
    mutate(data_frame =  .[[1]], data_frame_n = as.integer(as.factor(data_frame))) %>% 
    select(-1)
  if(plot_type == "line"){
    n_df  <- length(unique(new_out$pair))
    vcols <- c("gray50", user_colours(9, col_palette)[9])
    bcols <- rep(user_colours(n_df, col_palette), 2)
    # get the ylocations of the last grouping val
    yloc_lab <- new_out %>% 
      filter(data_frame == levels(data_frame)[length(levels(data_frame))]) %>%
      mutate(pair = gsub("&", "&\n", pair)) %>%
      select(corr, pair, data_frame) %>%
      mutate()
    # add a blank level to data_frame to extend axis to give space to labels
    levels(new_out$data_frame) <- c(levels(new_out$data_frame),'')
    plt <- new_out %>%
      ggplot(aes(x = data_frame, y = corr, 
                 colour = pair, group = pair)) +
      geom_blank() + theme_bw() + 
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank()) + 
      geom_line(size = 1.5) + 
      scale_colour_manual(name = "Data frame", values = bcols) +
      guides(colour = FALSE) + 
      labs(y = "Correlation", x = group_name)
    plt + 
      geom_text(aes(x = data_frame, y = corr, 
                    label = pair, colour = pair), 
                data = yloc_lab, inherit.aes = FALSE, 
                hjust = "left", size = 3, nudge_x = 0.1) +
      # scale_colour_manual(name = "Data frame", values = bcols) +
      scale_x_discrete(drop = FALSE)
  }
  if(plot_type == "bar"){
    n_df  <- length(unique(new_out$data_frame))
    vcols <- c("gray50", user_colours(9, col_palette)[9])
    bcols <- user_colours(n_df, col_palette)
    plt <- new_out %>%
      mutate(pair = gsub("&", "\n&", pair)) %>%
      ggplot(aes(x = pair, y = corr, fill = data_frame, group = data_frame)) +
      geom_blank() + theme_bw() +
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            axis.text.x = element_text(angle = 45)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(name = "Data frame", values = bcols) +
      guides(fill = FALSE) + 
      labs(y = "Correlation", x = "")
  }
  print(plt)
}





