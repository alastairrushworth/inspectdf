
plot_mem_1 <- function(df_plot, df_names, sizes){
  # convert column names to factor
  df_plot <- df_plot %>% 
    mutate(col_name = factor(col_name, levels = as.character(col_name)))
  # construct bar plot of column memory usage
  plt <- bar_plot(df_plot = df_plot, x = "col_name", y = "pcnt", 
                  fill = "col_name", label = "size", 
                  ttl = paste0("Column sizes in df::", df_names$df1), 
                  sttl = paste0("df::", df_names$df1,  " has ", sizes$ncl_1, 
                                " columns, ", sizes$nrw_1, 
                                " rows & total size of ", sizes$sz_1), 
                  ylb = "% of total size", rotate = TRUE)
  # add text annotation to plot
  plt <- add_annotation_to_bars(x = df_plot$col_name, 
                                y = df_plot$pcnt, 
                                z = df_plot$size, 
                                plt = plt, thresh = 0.2)
  # print plot
  print(plt)
}


plot_mem_2 <- function(df_plot, df_names, sizes){
  # convert to a tall df
  leg_text <- as.character(unlist(df_names))
  z1 <- df_plot %>% select(-contains("size")) %>% 
    gather(key = "df_input", value = "pcnt", -col_name) %>% 
    mutate(df_input = gsub("space_", "", df_input))
  z2 <- df_plot %>% select(-contains("space")) %>% 
    gather(key = "df_input", value = "size", -col_name) %>% 
    mutate(df_input = gsub("size_", "", df_input))
  z_tall <- z1 %>% left_join(z2, by = c("col_name", "df_input"))
  # make axis names
  ttl_plt <- paste0("Column sizes in df::", df_names$df1, 
                    " & df::", df_names$df2)
  sttl_plt1 <- paste0("df::", df_names$df1,  " has ", sizes$ncl_1, 
                      " columns, ", sizes$nrw_1, 
                      " rows & total size of ", sizes$sz_1)
  sttl_plt2 <- paste0("df::", df_names$df2,  " has ", sizes$ncl_2, 
                      " columns, ", sizes$nrw_2, 
                      " rows & total size of ", sizes$sz_2)
  # plot the result
  plt <- z_tall %>%
    mutate(col_name = factor(col_name, levels = df_plot$col_name)) %>%
    ggplot(aes(x = col_name, y = pcnt, fill = df_input)) + 
    geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) + 
    labs(x = "", y = "Percentage of total space (%)", 
         title = ttl_plt, 
         subtitle = paste0(sttl_plt1, "\n", sttl_plt2)) + 
    scale_fill_discrete(name = "Data frame", labels = leg_text) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(plt)
}