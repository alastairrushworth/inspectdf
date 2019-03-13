plot_types_1 <- function(df_plot, df_names){
  # convert column names to factor
  df_plot <- df_plot %>% 
    mutate(col_type = factor(col_type, levels = as.character(col_type)))
  # construct bar plot of column types
  plt <- bar_plot(df_plot = df_plot, x = "col_type", y = "percent", 
                  fill = "col_type", label = "count_type", 
                  ttl = paste0("Column type composition of df::", df_names$df1), 
                  sttl = paste0("df::", df_names$df1,  " contains ", sum(df_plot$count_type), 
                                " columns.  Count of each type shown on bar."), 
                  ylb = "Percentage of columns (%)", lgnd = "Column types")
  # add text annotation to plot
  plt <- add_annotation_to_bars(x = df_plot$col_type, 
                                y = df_plot$percent, 
                                z = df_plot$count_type, 
                                plt = plt, thresh = 0.1)
  # print plot
  print(plt)
}

plot_types_2 <- function(df_plot, df_names){
  # convert to a taller df
  d1 <- df_plot %>% select(1, 2:3) %>% mutate(df_input = df_names$df1)
  d2 <- df_plot %>% select(1, 4:5) %>% mutate(df_input = df_names$df2)
  colnames(d1) <- colnames(d2) <- c("col_type", "count", "percent", "df_input")
  z_tall <- bind_rows(d1, d2)

  # make axis names
  ttl_plt <- paste0("Column type composition of df::", 
                    df_names$df1, " & ", "df::", df_names$df2)
  sttl_plt1 <- paste0("df::", df_names$df1,  " contains ",
                     sum(d1$count), " columns & ")
  sttl_plt2 <- paste0("df::", df_names$df2,  " contains ", 
                      sum(d2$count), " columns.")
  # plot the result
  plt <- z_tall %>%
    mutate(col_type = factor(col_type, levels = df_plot$col_type)) %>%
    ggplot(aes(x = col_type, y = percent, 
               fill = as.factor(df_input), label = count)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    labs(x = "", y = "Percentage of columns (%)", title = ttl_plt, 
         subtitle = paste0(sttl_plt1, sttl_plt2)) + 
    scale_fill_discrete(name = "Data frame")
  print(plt)
}