#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme

plot_types_1 <- function(df_plot, df_names, text_labels, col_palette, 
                         label_angle, label_color, label_size){
  # define a plot wide nudge interval
  nudge <- max(df_plot$cnt) / 50
  # convert column names to factor
  df_plot <- df_plot %>% 
    mutate(type = factor(type, levels = as.character(type)))
  # construct bar plot of column types
  plt <- df_plot %>% 
    ggplot(aes(x = type, y = cnt, fill = type, label = cnt)) +
    geom_bar(stat = "identity") + 
    labs(x = 'Type', y = "Number of columns", 
         title = paste0("df::", df_names$df1, " column types"), 
         subtitle = paste0("df::", df_names$df1,  " has ", 
                           sum(df_plot$cnt), " columns.")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = user_colours(nrow(df_plot), col_palette)) +
    guides(fill = FALSE)
  # add text annotation to plot if requested
  if(text_labels){
    x = df_plot$type
    y = df_plot$cnt
    z = df_plot$cnt
    # if any zero length characters, replace with double quotes
    z[nchar(z) == 0] <- NA
    # whether ys are zero or not
    big_bar <- 0.15 * max(y, na.rm = T)
    # label_df
    label_df <- tibble(type = x, cnt = y)
    label_df$fill <- NA
    # labels white 
    label_white <- label_df %>% filter(cnt > big_bar) 
    max_lab <- ifelse(all(is.na(label_white$cnt)), NA, max(label_white$cnt, na.rm = T))
    # labels grey
    label_grey <- label_df %>% 
      filter(cnt <= big_bar, cnt > 0) %>%
      mutate(ymax = cnt + 0.5 * max_lab)
    # labels zero
    label_zero <- label_df %>% filter(cnt == 0)
    
    # add white labels at the top of the bigger bars
    if(nrow(label_white) > 0){
      plt <- plt + 
        annotate('text',
                 x = label_white$type,
                 y = label_white$cnt - nudge,
                 label = label_white$cnt,
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
                 x = label_grey$type,
                 y = label_grey$cnt + nudge,
                 label = label_grey$cnt,
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
                 x = label_zero$type,
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

plot_types_2 <- function(df_plot, df_names, text_labels, col_palette, 
                         label_angle, label_color, label_size){
  # convert to a taller df for plotting
  d1 <- df_plot %>% select(1, 2:3) %>% mutate(df_input = df_names$df1)
  d2 <- df_plot %>% select(1, 4:5) %>% mutate(df_input = df_names$df2)
  colnames(d1) <- colnames(d2) <- c("type", "cnt", "pcnt", "df_input")
  z_tall <- bind_rows(d1, d2)

  # make axis names
  ttl_plt <- paste0(df_names$df1, " & ", df_names$df2, " column types.")
  # if same number of columns, print different subtitle
  if(sum(d1$cnt) == sum(d2$cnt)){
    sttl <- paste0("Both have ",  sum(d1$cnt), " columns")
  } else {
    sttl <- paste(paste0(unlist(df_names), " has ", 
                   c(sum(d1$cnt), sum(d2$cnt)), " columns"), 
                  collapse = " & ")
  }
  
  # labels above 0.8 max
  z_tall$black_labs <- z_tall$white_labs <- z_tall$cnt
  z_tall$black_labs[z_tall$black_labs >  0.7 * max(z_tall$black_labs)] <- NA
  z_tall$white_labs[z_tall$white_labs <= 0.7 * max(z_tall$white_labs)] <- NA
  
  # plot the result
  plt <- z_tall %>%
    mutate(type = factor(type, levels = df_plot$type)) %>%
    ggplot(aes(x = type, y = cnt, fill = as.factor(df_input),
               group = as.factor(df_input))) + 
    geom_bar(stat = "identity", position = "dodge", 
             na.rm = TRUE)
  
  # add anotations if requested
  if(text_labels){
    plt <- add_annotation_to_bars(x = z_tall$type, 
                                  y = z_tall$cnt, 
                                  z = z_tall$cnt, 
                                  plt = plt, thresh = 0.1, 
                                  dodged = 1,
                                  fill = as.factor(z_tall$df_input), 
                                  label_color = label_color, 
                                  label_size  = label_size)
  }
  
  # labels the axes, add title and subtitle
  plt <- plt + 
    labs(x = "", y = "Number of columns", 
         title = ttl_plt, 
         subtitle = sttl) + 
    # label the legend 
    scale_fill_manual(name = "Data frame",  
                      values = user_colours(3, col_palette)[c(1, 3)])
  
  # return plot
  plt
}
