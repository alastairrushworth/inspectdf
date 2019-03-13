plot_num_1 <- function(df_plot, df_names){
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
  plt <- df_plot %>%
    ggplot(aes(x = mid, y = prop)) + 
    geom_col(fill = "blue") + 
    facet_grid(. ~ col_name, scales = "free") + 
    labs(x = "", y = "Probability", 
         title =  paste0("Histograms of numeric columns in df::", df_names$df1), 
         subtitle = "")
  # print plot
  print(plt)
}



plot_num_2 <- function(df_plot, df_names){
  df_plot <- df_plot %>% 
    select(-psi, -fisher_p) 
  # add the variable name to the histograms as an extra column
  for(i in 1:nrow(df_plot)) df_plot$levels.x[[i]]$cname <- df_plot$col_name[i] 
  for(i in 1:nrow(df_plot)) df_plot$levels.y[[i]]$cname <- df_plot$col_name[i] 
  # combine the histograms
  trns_plot <- bind_rows(bind_rows(df_plot$levels.x), 
                         bind_rows(df_plot$levels.y))
  trns_plot$dfn <- rep(unlist(df_names), each = nrow(trns_plot) / 2)
  # drop rows where both dfs are zero
  zro_drop <- trns_plot %>%
    group_by(cname, value) %>%
    summarise(zs = as.integer(sum(prop == 0) == 2)) %>%
    ungroup
  trns_plot <- left_join(trns_plot, zro_drop, 
                         by = c("cname", "value")) %>%
    filter(zs == 0)
  # apply an ordering to the categories
  get_num <- function(st) as.integer(gsub("\\[|\\(", "", unlist(strsplit(st, ","))[1]))
  get_numV <- Vectorize(get_num)
  ord_vals <- trns_plot %>%
    select(value) %>%
    distinct() %>%
    mutate(first_num = get_numV(value)) %>%
    arrange(first_num)
  # generate a heatplot
  plt <- trns_plot %>%
    ggplot(aes(x = dfn, y = factor(value, levels = ord_vals$value), fill = prop)) + 
    geom_tile(colour = "white") + 
    geom_text(aes(label = round(prop * 100, 1)), col = "gray30") + 
    scale_fill_gradient(low = "white", high = "steelblue") + 
    theme(legend.position = "none") +
    labs(x = "", y = "") + 
    facet_wrap(~ cname, scales = "free", ncol = 4)
  print(plt)
}