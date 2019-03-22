plot_na_1 <- function(df_plot, df_names){
  # convert col_name to factor
  df_plot <- df_plot %>% 
    mutate(col_name = factor(col_name, levels = as.character(col_name)))
  # construct bar plot of missingess
  plt <- bar_plot(df_plot = df_plot, x = "col_name", y = "pcnt", 
                  fill = "col_name", label = "cnt",
                  ttl = paste0("Prevalance of missing values in df::", df_names$df1),
                  sttl = paste0("df::", df_names$df1,  " has ", nrow(df_plot), 
                                " columns, of which ", sum(df_plot$cnt > 0), 
                                " have missing values"),
                  ylb = "% of column that is NA", rotate = TRUE)
  # add text annotation to plot
  plt <- add_annotation_to_bars(x = df_plot$col_name, y = df_plot$pcnt, 
                                z = df_plot$cnt, plt = plt)
  print(plt)
}

plot_na_2 <- function(df_plot, df_names, alpha){
  na_tab  <- df_plot
  df_plot <- df_plot %>% 
    select(-starts_with("cnt")) %>% 
    gather(key = "data_frame", value = "pcnt", -col_name, -p_value) %>%
    mutate(data_frame = gsub("pcnt_", "", data_frame))
  df_plot <- df_plot[seq(dim(df_plot)[1],1),]
  p_val_tab <- df_plot %>% 
    mutate(is_sig = as.integer(p_value < alpha) + 2, index = 1:nrow(df_plot)) %>%
    replace_na(list(is_sig = 1)) %>%
    select(is_sig, index) 
  
  plt <- ggplot(df_plot, aes(x = factor(col_name, 
                                            levels = rev(as.character(na_tab$col_name))), 
                                 y = pcnt, colour = data_frame)) +
    geom_blank() + theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
    geom_rect(
      fill = c("grey85", "darkorange2", "royalblue1")[p_val_tab$is_sig], alpha = 0.2,
      xmin = p_val_tab$index - 0.4, xmax = p_val_tab$index + 0.4,
      ymin = -100, ymax = 200, linetype = "blank") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    geom_point(size = 3.7, color = "black") + 
    geom_point(size = 3) +
    coord_flip()
  plt <- plt + labs(x = "", 
                    title =  paste0("Comparison of % missingness between ",
                                    df_names$df1, " and ", df_names$df2),
                    subtitle = bquote("Coloured stripes represent evidence of inequality (blue) or equality (orange) of missingness at \u03B1 = 0.05")) + 
    guides(colour = guide_legend(title = bquote("Data frame"))) + 
    labs(y = "Percent missing", x = "") %>% suppressWarnings()
  
  print(plt)
}