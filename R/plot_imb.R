plot_imb_1 <- function(df_plot, df_names){
  # convert col_name to factor
  df_plot <- df_plot %>% 
    mutate(col_name = factor(col_name, levels = as.character(col_name))) %>%
    mutate(label = paste0(value, " - ", round(percent, 1), "%"))
  # construct bar plot of missingess
  plt <- bar_plot(df_plot = df_plot, x = "col_name", y = "percent", fill = "col_name", label = "label", 
                  ttl = paste0("Categorical columns with single dominant levels in df::", df_names$df1), 
                  sttl = "Names of dominant levels are shown next to columns",
                  ylb = "% of column entries with single value", rotate = TRUE)
  # add text annotation to plot
  plt <- add_annotation_to_bars(x = df_plot$col_name, y = df_plot$percent, z = df_plot$value, plt = plt, thresh = 0.5)
  print(plt)
}

plot_imb_2 <- function(df_plot, df_names){
  
}