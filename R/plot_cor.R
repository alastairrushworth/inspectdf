plot_cor_1 <- function(df_plot, absolute = F, df_names){
  # preprocess data a bit
  df_plot <- df_plot %>% 
    mutate(pair = factor(pair, levels = as.character(pair)),
           sign = as.factor(c("Negative", 
                              "Positive")[as.numeric(corr > 0) + 1]))
  if(absolute){
    df_plot$lower <- ifelse(df_plot$corr < 0, -df_plot$lower,
                             df_plot$lower)
    df_plot$upper <- ifelse(df_plot$corr < 0, -df_plot$upper, df_plot$upper)
    df_plot$corr <- ifelse(df_plot$corr < 0, -df_plot$corr, 
                            df_plot$corr)
  }
  
  # generate points and error bars for correlations
  plt <- ggplot(df_plot, aes(x = pair, y = corr, colour = sign)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black", width = .1) +
    geom_point(size = 3.7, color = "black") + 
    geom_point(size = 3) +
    coord_flip() + 
    labs(x = "", 
         title =  paste0("Correlation of columns in df::", df_names$df1), 
         subtitle = bquote("Whiskers show 95% confidence regions"))
  if(absolute){
    plt <- plt + 
      guides(colour = guide_legend(title = bquote("\u03C1 Sign"))) + 
      labs(y = bquote("Absolute Pearson correlation (|\u03C1|)"), x = "")
  } else {
    plt <- plt + 
      guides(colour = FALSE) +
      labs(y = bquote("Pearson correlation (\u03C1)"), x = "")
  }
  
  # print plot
  print(plt)
}


plot_cor_2 <- function(df_plot, absolute = F, alpha, df_names){
  df_plot <- df_plot %>%
    mutate(pair = paste(col_1, col_2, sep = " & ")) %>%
    mutate(pair = factor(pair, levels = as.character(pair))) %>%
    select(-col_1, -col_2) %>% 
    gather(key = "data_frame", value = "corr", -pair, -p_value) %>%
    mutate(sign = as.factor(c("Negative", "Positive")[as.numeric(corr > 0) + 1]))
  # if only showing the absolute correlations
  if(absolute){
    # if showing absolute values, flip the sign of any negative values 
    df_plot$corr <- ifelse(df_plot$corr < 0, -df_plot$corr, 
                                df_plot$corr)
  }
  p_val_tab <- df_plot %>% 
    mutate(is_sig = as.integer(p_value < alpha) + 1, index = 1:nrow(df_plot)) %>%
    select(is_sig, index)
  # generate basic plot
  plt <- ggplot(df_plot, aes(x = as.factor(pair), y = corr, colour = sign, 
                                  group = data_frame)) +
    geom_blank() + theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
    geom_rect(
      fill = c("darkorange2", "royalblue1")[p_val_tab$is_sig], alpha = 0.2,
      xmin = p_val_tab$index - 0.4, xmax = p_val_tab$index + 0.4,
      ymin = -2, ymax = 2, linetype = "blank") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    geom_point(size = 3.7, color = "black", na.rm = TRUE) + 
    geom_point(size = 3, na.rm = TRUE) +
    coord_flip() + 
    labs(x = "", 
         title =  paste0("Comparison of \u03C1 between df::", df_names$df1, 
                         " and ", df_names$df2),
         subtitle = bquote("Blue/orange stripes represent inequality/equality of \u03C1"))
  # if absolute value requested then label accordingly
  if(absolute){
    plt <- plt + 
      guides(colour = guide_legend(title = bquote("\u03C1 sign"))) + 
      labs(y = bquote("Absolute Pearson correlation (|\u03C1|)"), x = "")
  } else {
    plt <- plt + 
      guides(colour = FALSE) +
      labs(y = bquote("Pearson correlation (\u03C1)"), x = "")
  }
  print(plt)
}