#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 theme

plot_num_1 <- function(df_plot, df_names, plot_layout, text_labels){
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # get bin midpoints for plotting
  for(i in 1:length(df_plot$hist)){
    # check first if the variable is completely missing
    if(!(nrow(df_plot$hist[[i]]) == 1 & is.na(df_plot$hist[[i]]$value[1]))){
      df_plot$hist[[i]]$col_name <- df_plot$col_name[i]
      diff_nums <- lapply(strsplit(gsub("\\[|,|\\)", "", df_plot$hist[[i]]$value), " "), 
                          function(v) diff(as.numeric(v))) %>% unlist %>% unique
      df_plot$hist[[i]]$mid <- lapply(strsplit(gsub("\\[|,|\\)", "", df_plot$hist[[i]]$value), " "), 
                                      function(v) diff(as.numeric(v))/2 + as.numeric(v)[1]) %>% unlist
      if(is.nan(df_plot$hist[[i]]$mid[1]) | is.infinite(df_plot$hist[[i]]$mid[1])){
        df_plot$hist[[i]]$mid[1] <- df_plot$hist[[i]]$mid[2] - (diff_nums[is.finite(diff_nums)])[1]
      } 
      last_n <- length(df_plot$hist[[i]]$mid)
      if(is.nan(df_plot$hist[[i]]$mid[last_n]) | is.infinite(df_plot$hist[[i]]$mid[last_n])){
        df_plot$hist[[i]]$mid[last_n] <- df_plot$hist[[i]]$mid[last_n - 1] + (diff_nums[is.finite(diff_nums)])[1]
      }
    } else {
      df_plot$hist[[i]] <- NULL
    }
  }
  df_plot <- bind_rows(df_plot$hist)
  bin_width <- df_plot %>% 
    group_by(col_name) %>%
    summarise(bar_width = diff(mid)[1]/1.5)
  df_plot <- df_plot %>% 
    left_join(bin_width, by = "col_name")
  # generate plot
  plt <- df_plot %>%
    ggplot(aes(x = mid, y = prop, width = bar_width)) + 
    geom_col(fill = "blue") + 
    labs(x = "", y = "Probability", 
         title =  paste0("Histograms of numeric columns in df::", df_names$df1), 
         subtitle = "") +
    facet_wrap(~ col_name, scales = "free", 
               nrow = plot_layout[[1]], 
               ncol = plot_layout[[2]])
  # print plot
  plt
}



plot_num_2 <- function(df_plot, df_names, plot_layout, text_labels, alpha){
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # chop stuff off
  df_plot <- df_plot %>% 
    select(-jsd) 
  # if columns missing in either dataframe, use buckets from the other
  # replace frequencies with NA.
  x_1 <- which(unlist(lapply(df_plot$hist_1, is.null)))
  x_2 <- which(unlist(lapply(df_plot$hist_2, is.null)))
  if(length(x_1) > 0){
    for(i in x_1){
      df_plot$hist_1[[i]] <- df_plot$hist_2[[i]]
      df_plot$hist_1[[i]]$prop <- NA
    }
  }
  if(length(x_2) > 0){
    for(i in x_2){
      df_plot$hist_2[[i]] <- df_plot$hist_1[[i]]
      df_plot$hist_2[[i]]$prop <- NA
    }
  }

  # add the variable name to the histograms as an extra column
  for(i in 1:nrow(df_plot)){
    df_plot[[2]][[i]]$cname <- df_plot[[3]][[i]]$cname <- df_plot$col_name[i] 
  }

  # combine the histograms
  trns_plot <- bind_rows(bind_rows(df_plot[[2]]), 
                         bind_rows(df_plot[[3]]))
  trns_plot$dfn <- rep(unlist(df_names), each = nrow(trns_plot) / 2)
  # apply an ordering to the categories
  get_num <- function(st){
    first_dig <- gsub("\\[|\\(", "", unlist(strsplit(st, ","))[1])
    first_dig <- ifelse(grepl("Inf", first_dig), 
                        as.numeric(first_dig), 
                        as.integer(first_dig))
    return(first_dig)
  }
  xy <- df_plot %>% 
    select(cname = col_name, fisher_p) %>%
    mutate(significant = as.integer(fisher_p < alpha) + 2) %>%
    replace_na(list(significant = 1)) %>%
    mutate(significant = c("gray30", "lightskyblue1", "red3")[significant])
  
  get_numV <- Vectorize(get_num)
  ord_vals <- trns_plot %>%
    select(value) %>%
    distinct() %>%
    mutate(first_num = suppressWarnings(get_numV(value))) %>%
    arrange(first_num)
  # generate a heatplot
  plt <- trns_plot %>%
    ggplot(aes(x = dfn, y = factor(value, levels = ord_vals$value), fill = prop)) +
    geom_rect(data = xy, fill = xy$significant,  
              xmin = -Inf, xmax = Inf, 
              ymin = -Inf, ymax = Inf, alpha = 0.5, 
              inherit.aes = FALSE) +
    geom_tile(colour = "white") + 
    ggfittext::geom_fit_text(
      aes(label = round(prop * 100, 1)),
      contrast = TRUE,
      na.rm = TRUE
    ) + 
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme(legend.position = "none") +
    labs(x = "", y = "", 
         title =  paste0("Heat plot comparison of numeric columns")) + 
    facet_wrap(~ cname, scales = "free", 
               nrow = plot_layout[[1]], 
               ncol = plot_layout[[2]])  
  plt
}
