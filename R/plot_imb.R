#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 position_nudge

plot_imb_1 <- function(df_plot, df_names, text_labels, label_angle = NULL, 
                       label_color, label_size, col_palette){
  # define a plot wide nudge interval
  nudge <- max(df_plot$pcnt) / 50
  # convert col_name to factor
  df_plot <- df_plot %>% 
    mutate(col_name = factor(col_name, levels = as.character(col_name))) %>%
    mutate(label = paste0(value, " - ", round(pcnt, 1), "%")) %>%
    mutate(value = case_when(is.na(value) ~ "NA", TRUE ~ value))
  
  # construct bar plot of missingness
  plt <- df_plot %>% 
    ggplot(aes(x = col_name, y = pcnt, fill = col_name, label = value)) +
    geom_bar(stat = "identity") + 
    labs(x = '', y = "% of values", 
         title = paste0("df::", df_names$df1, " most common levels by column")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = user_colours(nrow(df_plot), col_palette)) +
    guides(fill = FALSE)
  
  # add text annotation to plot if requested
  if(text_labels){
    x = df_plot$col_name
    y = df_plot$pcnt
    z = paste0(df_plot$value, ':  ', round(df_plot$pcnt, 2))
    # if any zero length characters, replace with double quotes
    z[nchar(z) == 0] <- NA
    # whether ys are zero or not
    big_bar <- 0.2 * max(y, na.rm = T)
    # label_df
    label_df <- tibble(col_name = x, pcnt = y , label = z)
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
  # plt
  # # add text annotation to plot
  # if(text_labels){
  #   plt <- add_annotation_to_bars(
  #     x = df_plot$col_name,
  #     y = df_plot$pcnt,
  #     z = df_plot$value,
  #     plt = plt, 
  #     thresh = 0.5, 
  #     parse = TRUE)
  # }
  # return plot
  plt
}

plot_imb_2 <- function(df_plot, df_names, alpha, text_labels, col_palette){
  
  # combine col_name and value
  df_plot <- df_plot %>% 
    mutate(col_name = paste0(col_name, "\n(", value, ")"))
  # save a version of plotting data for later
  na_tab  <- df_plot
  # convert to tall
  df_plot <- df_plot %>% 
    select(-starts_with("cnt")) %>% 
    gather(key = "data_frame", value = "pcnt", -col_name, -p_value, -value) %>%
    mutate(data_frame = as.integer(gsub("pcnt_", "", data_frame))) %>%
    mutate(col_name = factor(col_name, levels = as.character(na_tab$col_name))) %>%
    mutate(data_frame = unlist(df_names)[data_frame])
  # reverse row order of plotting data
  df_plot <- df_plot[nrow(df_plot):1, ]
  # calculate significance 
  p_val_tab <- df_plot %>% 
    mutate(is_sig = as.integer(p_value < alpha) + 2, index = 1:nrow(df_plot)) %>%
    replace_na(list(is_sig = 1)) %>%
    select(is_sig, index) 
  # max extent of percentages
  yrange <- abs(diff(range(df_plot$pcnt, na.rm = TRUE)))
  df_plot <- df_plot %>%
    group_by(col_name) %>%
    arrange(data_frame) %>%
    mutate(nudge = as.integer((abs(diff(pcnt)) / yrange) < 0.02)) %>%
    ungroup 
  df_plot$nudge[(df_plot$data_frame == unique(df_plot$data_frame)[1]) & (df_plot$nudge == 1)] <- -1
  nudge_vec <- df_plot$nudge
  nudge_vec[is.na(nudge_vec)] <- 0
  # generate plot
  df_plot_col_name <- df_plot$col_name
  plt <- df_plot %>%
    ggplot(aes(x = factor(col_name, levels = unique(df_plot_col_name)), 
               y = pcnt, 
               colour = data_frame)) +
    geom_blank() + theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
    geom_rect(
      fill = c(NA, "gray50", user_colours(9, col_palette)[9])[p_val_tab$is_sig], alpha = 0.2,
      xmin = p_val_tab$index - 0.4, xmax = p_val_tab$index + 0.4,
      ymin = -100, ymax = 200, linetype = "blank") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
    geom_point(size = 3.7, color = "black", na.rm = TRUE, position = position_nudge(x = -0.15 * nudge_vec)) + 
    geom_point(size = 3, na.rm = TRUE, position = position_nudge(x = -0.15 * nudge_vec)) +
    coord_flip() +
    scale_colour_manual(values = get_best_pair(col_palette), name = "Data frame")
  
  # title & subtitle
  ttl <- paste0("Comparison of most common levels")
  sttl <- bquote("Color/gray stripes mean different/equal imbalance")
  plt <- plt + 
    labs(x = "", title = ttl, subtitle = sttl) + 
    guides(color = guide_legend(override.aes = list(fill = NA))) + 
    labs(y = "% of column", x = "")
  
  # return the plot 
  plt
}

plot_imb_grouped <- function(df_plot, df_names, text_labels, col_palette, plot_type){
  # group variable name
  group_name <- colnames(df_plot)[1]
  if(plot_type == 1){
    # get ordering of variable pairs by median correlation
    col_ord <- df_plot %>% 
      ungroup %>%
      group_by(col_name) %>%
      summarize(md_pcnt = median(pcnt, na.rm = T)) %>%
      arrange(md_pcnt) %>%
      .$col_name
    # create pair columns and arrange by col_ord
    out <- df_plot %>% 
      ungroup %>%
      mutate(col_name = factor(col_name, levels = col_ord)) %>%
      arrange(col_name) 
    # jitter points if number of column pairs <= 10
    jitter_width <- ifelse(length(unique(out$col_name)) > 10, 0, 0.25) 
    plt <- out %>%
      ggplot(aes_string(x = 'col_name', y = 'pcnt', col = 'col_name', group = group_name)) + 
      geom_jitter(alpha = 0.5, width = jitter_width, height = 0, size = 1.8) + 
      theme(legend.position='none') + 
      coord_flip() + 
      ylab("Imbalance by group") +
      xlab("")
  } else {
    plt <- plot_grouped(df = df_plot, 
                        value = "pcnt", 
                        series = "col_name", 
                        group = group_name, 
                        plot_type = plot_type, 
                        col_palette = col_palette, 
                        text_labels = text_labels, 
                        ylab = "% imbalance")
  }
  plt
}
