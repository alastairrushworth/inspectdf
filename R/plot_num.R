plot_num_1 <- function(df_plot, df_names, plot_layout){
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # get bin midpoints for plotting
  for(i in 1:length(df_plot$hist)){
    df_plot$hist[[i]]$col_name <- df_plot$col_name[i]
    diff_nums <- lapply(strsplit(gsub("\\[|,|\\)", "", df_plot$hist[[i]]$value), " "), function(v) diff(as.numeric(v))) %>% unlist %>% unique
    df_plot$hist[[i]]$mid <- lapply(strsplit(gsub("\\[|,|\\)", "", df_plot$hist[[i]]$value), " "), function(v) diff(as.numeric(v))/2 + as.numeric(v)[1]) %>% unlist
    if(is.nan(df_plot$hist[[i]]$mid[1]) | is.infinite(df_plot$hist[[i]]$mid[1])){
      df_plot$hist[[i]]$mid[1] <- df_plot$hist[[i]]$mid[2] - (diff_nums[is.finite(diff_nums)])[1]
    } 
    last_n <- length(df_plot$hist[[i]]$mid)
    if(is.nan(df_plot$hist[[i]]$mid[last_n]) | is.infinite(df_plot$hist[[i]]$mid[last_n])){
      df_plot$hist[[i]]$mid[last_n] <- df_plot$hist[[i]]$mid[last_n - 1] + (diff_nums[is.finite(diff_nums)])[1]
    }
  }
  df_plot <- bind_rows(df_plot$hist)
  # generate plot
  plt <- df_plot %>%
    ggplot(aes(x = mid, y = prop)) + 
    geom_col(fill = "blue") + 
    labs(x = "", y = "Probability", 
         title =  paste0("Histograms of numeric columns in df::", df_names$df1), 
         subtitle = "") +
    facet_wrap(~ col_name, scales = "free", 
               nrow = plot_layout[[1]], 
               ncol = plot_layout[[2]])
  # print plot
  print(plt)
}



plot_num_2 <- function(df_plot, df_names, plot_layout){
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # chop stuff off
  df_plot <- df_plot %>% 
    select(-psi, -fisher_p) 
  # add the variable name to the histograms as an extra column
  for(i in 1:nrow(df_plot)) df_plot[[2]][[i]]$cname <- df_plot$col_name[i] 
  for(i in 1:nrow(df_plot)) df_plot[[3]][[i]]$cname <- df_plot$col_name[i] 
  # combine the histograms
  trns_plot <- bind_rows(bind_rows(df_plot[[2]]), 
                         bind_rows(df_plot[[3]]))
  trns_plot$dfn <- rep(unlist(df_names), each = nrow(trns_plot) / 2)
  # apply an ordering to the categories
  get_num <- function(st) as.integer(gsub("\\[|\\(", "", unlist(strsplit(st, ","))[1]))
  get_numV <- Vectorize(get_num)
  ord_vals <- trns_plot %>%
    select(value) %>%
    distinct() %>%
    mutate(first_num = suppressWarnings(get_numV(value))) %>%
    arrange(first_num)
  # generate a heatplot
  plt <- trns_plot %>%
    ggplot(aes(x = dfn, y = factor(value, levels = ord_vals$value), fill = prop)) + 
    geom_tile(colour = "white") + 
    geom_text(aes(label = round(prop * 100, 1)), col = "gray30") + 
    scale_fill_gradient(low = "white", high = "steelblue") + 
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "", 
         title =  paste0("Heat plot comparison of numeric columns")) + 
    facet_wrap(~ cname, scales = "free", 
               nrow = plot_layout[[1]], 
               ncol = plot_layout[[2]])  
  print(plt)
}
