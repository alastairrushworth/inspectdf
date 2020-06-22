#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme

plot_mem_1 <- function(df_plot, df_names, sizes, text_labels, col_palette, 
                       label_angle = NULL, label_color, label_size){
  # define a plot wide nudge interval
  nudge <- max(df_plot$pcnt) / 50
  # convert column names to factor
  df_plot <- df_plot %>% 
    mutate(col_name = factor(col_name, levels = as.character(col_name)))
  # set NAs to 0
  df_plot$size <- ifelse(is.na(df_plot$size), "", df_plot$size)
  df_plot$pcnt <- ifelse(is.na(df_plot$pcnt), 0, df_plot$pcnt)

  # construct bar plot of missingness
  plt <- df_plot %>% 
    ggplot(aes(x = col_name, y = pcnt, fill = col_name, label = size)) +
    geom_bar(stat = "identity") + 
    labs(x = '', y = "% of total size", 
         title = paste0("Column sizes in df::", df_names$df1), 
         subtitle = paste0("df::", df_names$df1,  " has ", sizes$ncl_1, 
                           " columns, ", sizes$nrw_1,
                           " rows & total size of ", sizes$sz_1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = user_colours(nrow(df_plot), col_palette)) +
    guides(fill = FALSE)
  
  if(text_labels){
    x = df_plot$col_name
    y = df_plot$pcnt
    z = df_plot$size
    # if any zero length characters, replace with double quotes
    z[nchar(z) == 0] <- NA
    # whether ys are zero or not
    big_bar <- 0.2 * max(y, na.rm = T)
    # label_df
    label_df <- tibble(col_name = x, pcnt = y, label = z)
    label_df$fill <- NA
    # labels white 
    label_white <- label_df %>% filter(pcnt > big_bar) 
    max_lab <- ifelse(all(is.na(label_white$pcnt)), NA, max(label_white$pcnt, na.rm = T))
    # labels grey
    label_grey <- label_df %>% 
      filter(pcnt <= big_bar, pcnt > 0) %>%
      mutate(ymax = pcnt + 0.5 * max_lab)
    # labels zero
    label_zero <- label_df %>% filter(pcnt == 0)
    
    # add white labels at the top of the bigger bars
    if(nrow(label_white) > 0){
      plt <- plt + 
        annotate('text',
                 x = label_white$col_name,
                 y = label_white$pcnt - nudge,
                 label = label_white$label,
                 color = ifelse(is.null(label_color), "white", label_color),
                 angle = ifelse(is.null(label_angle), 90, label_angle), 
                 size  = ifelse(is.null(label_size), 3.5, label_size), 
                 hjust = 1, 
                 
        )
    }
    # add grey labels to relatively short bars, if any
    if(nrow(label_grey) > 0){
      plt <- plt + 
        annotate('text',
                 x = label_grey$col_name,
                 y = label_grey$pcnt + nudge,
                 label = label_grey$label,
                 color = ifelse(is.null(label_color), "gray50", label_color),
                 angle = ifelse(is.null(label_angle), 90, label_angle), 
                 size  = ifelse(is.null(label_size), 3.5, label_size), 
                 hjust = 0
        )
    }
    # add 0 labels, if any
    if(nrow(label_zero) > 0){
      plt <- plt + 
        annotate('text',
                 x = label_zero$col_name,
                 y = nudge,
                 label = 0,
                 color = ifelse(is.null(label_color), "gray50", label_color),
                 angle = ifelse(is.null(label_angle), 90, label_angle), 
                 size  = ifelse(is.null(label_size), 3.5, label_size), 
                 hjust = 0
        )
    }
  }
  # print plot
  plt
}


plot_mem_2 <- function(df_plot, df_names, sizes, text_labels, col_palette, 
                       label_angle = NULL, label_color, label_size){

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
                                  dodged = 1, 
                                  fill = z_tall$df_input, 
                                  label_color = label_color, 
                                  label_size  = label_size)
  }
  
  plt
}
