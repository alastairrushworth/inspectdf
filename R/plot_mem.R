#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme

plot_mem_1 <- function(df_plot, df_names, sizes, text_labels, col_palette){
  # convert column names to factor
  df_plot <- df_plot %>% 
    mutate(col_name = factor(col_name, levels = as.character(col_name)))
  # set NAs to 0
  df_plot$size <- ifelse(is.na(df_plot$size), "", df_plot$size)
  df_plot$pcnt <- ifelse(is.na(df_plot$pcnt), 0, df_plot$pcnt)

  # construct bar plot of column memory usage
  plt <- bar_plot(df_plot = df_plot, x = "col_name", y = "pcnt", 
                  fill = "col_name", label = "size", 
                  ttl = paste0("Column sizes in df::", df_names$df1), 
                  sttl = paste0("df::", df_names$df1,  " has ", sizes$ncl_1, 
                                " columns, ", sizes$nrw_1, 
                                " rows & total size of ", sizes$sz_1), 
                  ylb = "% of total size", rotate = TRUE, 
                  col_palette = col_palette)
  # add text annotation to plot  
  if(text_labels){
    plt <- add_annotation_to_bars(x = df_plot$col_name, 
                                  y = df_plot$pcnt, 
                                  z = df_plot$size, 
                                  plt = plt, thresh = 0.2)
  }
  # print plot
  print(plt)
}


plot_mem_2 <- function(df_plot, df_names, sizes, text_labels, col_palette){

  leg_text <- as.character(unlist(df_names))
  # gather percents
  z1 <- df_plot %>% select(-contains("size")) %>% 
    gather(key = "df_input", value = "pcnt", -col_name)
  # gather sizes
  z2 <- df_plot %>% 
    select(-contains("pcnt")) %>%
    gather(key = "df_input", value = "size", -col_name) %>% 
    mutate(df_input = gsub("size_", "pcnt_", df_input))
  # convert to a tall df
  z_tall <- z1 %>% 
    left_join(z2, by = c("col_name", "df_input")) 
    
  # make axis names
  ttl_plt <- paste0("Column sizes in df::", df_names$df1, 
                    " & df::", df_names$df2)
  sttl_plt1 <- paste0("df::", df_names$df1,  " has ", sizes$ncl_1, 
                      " columns, ", sizes$nrw_1, 
                      " rows & total size of ", sizes$sz_1)
  sttl_plt2 <- paste0("df::", df_names$df2,  " has ", sizes$ncl_2, 
                      " columns, ", sizes$nrw_2, 
                      " rows & total size of ", sizes$sz_2)
  # tidy the factor 
  z_tall <- z_tall %>%
    mutate(col_name = factor(col_name, levels = df_plot$col_name)) 

  # set NAs to 0
  z_tall$size <- ifelse(is.na(z_tall$size), "", z_tall$size)
  z_tall$pcnt <- ifelse(is.na(z_tall$pcnt), 0, z_tall$pcnt)

  # plot the result
  plt <- z_tall %>%
    ggplot(aes(x = col_name, y = pcnt, fill = df_input, label = size)) + 
    geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) + 
    labs(x = "", y = "% of total size", 
         title = ttl_plt, 
         subtitle = paste0(sttl_plt1, "\n", sttl_plt2)) + 
    scale_fill_manual(name = "Data frame", labels = leg_text, 
                      values = get_best_pair(col_palette)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if(text_labels){
    plt <- add_annotation_to_bars(x = z_tall$col_name, 
                                  y = z_tall$pcnt, 
                                  z = z_tall$size, 
                                  plt = plt, thresh = 0.2, 
                                  dodged = 1, fill = z_tall$df_input)
  }
  
  print(plt)
}
