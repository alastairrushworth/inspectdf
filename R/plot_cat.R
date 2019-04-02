#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 scale_x_discrete
plot_cat <- function(levels_df, df_names){
  # plotting pallete
  b <- colorRampPalette(c("tomato3", "white"))
  zcols <- b(1001)

  # select levels columns
  lvl_df <- levels_df %>% select_if(is.list) 
  # either there are one or two columns
  # if there are two then need an extra column
  is_onedf <- ncol(lvl_df) == 1
  if(is_onedf){
    lvl_df <- pull_collapse(lvl_df, 1)
    lvl_df$dfi <- df_names[[1]]
  } else {
    # first remove column
    if(anyNA(levels_df$psi)){
      levels_df <- levels_df[-which(is.na(levels_df$psi)), ]
    }
    # levels_df <- levels_df %>%
    #   filter(!is.na(psi))
    lvl_df <- levels_df %>% select_if(is.list) 
    a1 <- pull_collapse(lvl_df, 1)
    a2 <- pull_collapse(lvl_df, 2)
    a1$dfi <- df_names[[1]]
    a2$dfi <- df_names[[2]]
    lvl_df <- bind_rows(a1, a2)
    # combine df into col_name
    lvl_df <- lvl_df %>% 
      mutate(col_name = paste0(col_name, ": ", dfi))
  }

  # add new keys and arrange
  lvl_df2 <- lvl_df %>% 
    arrange(col_name, prop, value) %>%
    mutate(new_level_key = paste0(level_key, "-", dfi)) %>%
    mutate(new_level_key = factor(new_level_key, 
                                  levels = unique(new_level_key))) %>%
    mutate(level_key = factor(level_key, levels = unique(level_key))) %>%
    mutate(col_name = factor(col_name, levels = rev(sort(unique(col_name)))))

  # generate plot
  plt <- lvl_df2 %>%
    ggplot(aes(x = col_name, y = prop, fill = new_level_key)) +
    geom_bar(position = "stack", stat = "identity", 
             colour = "black", size = 0.2) +
    scale_fill_manual(
      values = ifelse(is.na(lvl_df2$value), "gray65", 
                      zcols[round(lvl_df2$colvalstretch * 1000, 0)])) +
    coord_flip() +
    guides(fill = FALSE) + 
    theme(axis.title.y = element_blank(), panel.background = element_blank(),
          axis.ticks.y = element_blank(), panel.border = element_blank(), 
          panel.grid.major = element_blank(), axis.title.x = element_blank(), 
          axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = "", y = "", 
         subtitle = bquote("Gray segments are missing values")) 
  
  # if this is a comparison, then add x-axis labels and descriptive title
  if(is_onedf){
    ttl <- paste0("Frequency of categorical levels in df::", 
                  df_names$df1)
  } else {
    split_labs <- strsplit(levels(lvl_df2$col_name), ": ")
    newlabs   <- sapply(split_labs, function(v) paste(rev(v), collapse = ": "))
    ttl <- paste0("Categorical levels in df::", 
                  df_names$df1, " vs. df:: ", df_names$df1)
    plt <- plt + 
      scale_x_discrete(labels = newlabs)
  }
  # add title
  plt <- plt + labs(title = ttl)
  
  # label bars with category name if bar is long enough
  annts <- lvl_df2 %>% 
    mutate(col_num = as.integer(col_name))
  annts$value[annts$prop < 0.15] <- NA
  col_vec <- ifelse((annts$colvalstretch > 0.7), 2, 1)
  # if bars are dark, label white, otherwise gray
  plt <- plt + geom_text(aes(x = annts$col_num, 
                             y = annts$colval - (annts$prop/2), 
                             label = annts$value), 
                         color = c("white", "gray55")[col_vec], 
                         inherit.aes = FALSE, na.rm = T, hjust = 0.5)
  return(plt)
}


# the only thing that is used to plot is the levels field
collapse_levels <- function(list_col){
  suppressWarnings(bind_rows(list_col, .id = 'col_name')) %>% 
    group_by(col_name) %>%
    arrange(col_name, desc(prop), desc(value)) %>%
    mutate(colval = cumsum(prop)) %>% 
    mutate(colvalstretch = (colval - min(colval) + 0.001)/
             (max(colval) - min(colval) + 0.001)) %>%
    ungroup %>%
    arrange(col_name, prop, value) %>%
    mutate(level_key = paste0(value, "-", col_name)) %>% return()
}

put_na_top <- function(dfb){
  if(anyNA(dfb$value)){
    na_row <- which(is.na(dfb$value))[1]
    return(bind_rows(dfb[na_row, ], dfb[-na_row, ]))
  } else {
    return(dfb)
  }
}

# pull out the list column containing levels 
# and collapse the list into a dataframe
pull_collapse <- function(dfi, i){
  dfi %>% 
    dplyr::pull(i) %>% 
    collapse_levels()
}