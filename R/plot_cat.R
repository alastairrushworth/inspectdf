#' @importFrom dplyr pull
#' @importFrom ggplot2 scale_x_discrete
plot_cat <- function(levels_df, df_names){
  # plotting pallete
  b <- colorRampPalette(c("tomato3", "white"))
  zcols <- b(1001)

  # select levels columns
  lvl_df <- levels_df %>% 
    select_if(is.list)
  
  # either there are one or two columns
  # if there are two then need an extra column
  is_onedf <- ncol(lvl_df) == 1
  if(is_onedf){
    lvl_df <- pull_collapse(lvl_df, 1)
    lvl_df$dfi <- df_names[[1]]
  } else {
    a1 <- pull_collapse(lvl_df, 1)
    a2 <- pull_collapse(lvl_df, 2)
    a1$dfi <- df_names[[1]]
    a2$dfi <- df_names[[2]]
    lvl_df <- bind_rows(a1, a2)
    # combine df into col_name
    lvl_df <- lvl_df %>% 
      mutate(col_name = paste0(col_name, ": ", dfi))
  }

  # ensure that NA levels are places in the first row
  plt <- lvl_df %>% 
    group_by(col_name, dfi) %>%
    # do(put_na_top(.)) %>% 
    arrange(col_name, prop, value) %>%
    ungroup %>% 
    mutate(level_key = factor(level_key, levels = unique(level_key))) %>%
    ggplot(aes(x = col_name,  y = prop, fill = level_key)) + 
    geom_bar(position = "stack", stat = "identity", colour = "black", 
             size = 0.2) +
    scale_fill_manual(
      values = ifelse(is.na(lvl_df$value), "gray65", 
                      zcols[round(lvl_df$colval * 1000, 0)])) +
    coord_flip() +
    guides(fill = FALSE) + 
    theme(axis.title.y = element_blank(), panel.background = element_blank(),
          axis.ticks.y = element_blank(), panel.border = element_blank(), 
          panel.grid.major = element_blank()) +
    labs(x = "", y = "", 
         subtitle = bquote("Gray segments correspond to missing values")) 
  
  # if this is a comparison, then add x-axis labels and descriptive title
  if(is_onedf){
    plt <- plt + 
      labs(title = paste0("Frequency of levels in categorical columns of df::", 
                          df_names$df1))
  } else {
    split_labs <- strsplit(sort(unique(lvl_df$col_name)), ": ")
    new_labs  <- lapply(split_labs, function(v) paste(rev(v), collapse = ": "))
    plt <- plt + 
      scale_x_discrete(labels = new_labs) + 
      labs(title = paste0("Comparison of levels in categorical columns of df::", 
                          df_names$df1, " and df:: ", df_names$df1))
  }

  # two different label series
  annts <- lvl_df %>% 
    mutate(col_num = as.integer(factor(col_name, levels = levels_df$col_name)))
  annts$value[annts$prop < 0.15] <- NA
  col_vec <- ifelse((annts$colval > 0.7) & (annts$prop < 0.7), 2, 1)
  # add a white series to the bigger bars
  plt <- plt + geom_text(aes(x = annts$col_num, 
                             y = annts$colval - (annts$prop/2), 
                             label = annts$value), 
                         color = c("white", "gray70")[col_vec], 
                         inherit.aes = FALSE, na.rm = T)
  return(plt)
}


# the only thing that is used to plot is the levels field
collapse_levels <- function(list_col){
  suppressWarnings(bind_rows(list_col, .id = 'col_name')) %>% 
    group_by(col_name) %>%
    arrange(col_name, desc(prop), desc(value)) %>%
    mutate(colval = cumsum(prop)) %>% ungroup %>%
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