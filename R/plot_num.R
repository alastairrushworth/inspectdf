#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme
#' @importFrom tidyr unnest

plot_num_1 <- function(df_plot, df_names, plot_layout, text_labels, col_palette){
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # add histogram midpoints
  df_plot <- add_midpoints(df_plot)
  df_plot <- bind_rows(df_plot$hist)
  bin_width <- df_plot %>% 
    group_by(col_name) %>%
    summarise(bar_width = diff(mid)[1]/1.5)
  df_plot <- df_plot %>% 
    left_join(bin_width, by = "col_name")
  # add a colour scale variable in df_plot
  # scale densities to have max of 1 and min 0
  if(!is.na(col_palette)){
    df_plot <- df_plot %>%
      group_by(col_name) %>%
      mutate(prop_z  = prop / max(prop)) %>%
      ungroup
  } else {
    df_plot['prop_z'] = 'blue'
  }

  # generate plot
  plt <- df_plot %>%
    ggplot(aes(x = mid, y = prop, width = bar_width, fill = prop_z)) + 
    geom_col() + 
    labs(x = "", y = "Probability", 
         title =  paste0("Histograms of numeric columns in df::", df_names$df1), 
         subtitle = "") +
    facet_wrap(~ col_name, 
               scales = "free", 
               nrow = plot_layout[[1]], 
               ncol = plot_layout[[2]])
  
  if(!is.na(col_palette)){
    plt <- plt + 
      scale_fill_gradientn(colours = print_palette_pairs(col_palette)) +
      theme(legend.position = "none")
  } else {
    plt <- plt + 
      scale_fill_manual(values = 'blue') + 
      theme(legend.position = "none")
  }
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


add_midpoints <- function(df_plot){
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
  return(df_plot)
}


plot_num_3 <- function(df_plot, df_names, plot_layout, text_labels, alpha, col_palette){
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # number of grouping columns
  n_groups <- ncol(df_plot) - 10
  # add the variable name to the histograms as an extra column
  for(i in 1:nrow(df_plot)) df_plot$hist[[i]]$cname <- df_plot$col_name[i] 
  df_plot <- bind_cols(
    tibble( grouping = df_plot %>% select(1:n_groups) %>%
              apply(., 1, paste, collapse = "-")), 
    df_plot %>% 
      select(-c(1:n_groups)))
  # add histogram midpoints
  df_plot <- add_midpoints(df_plot)

  # unnest the histograms
  df_hist <- df_plot %>% 
    select(grouping, hist) %>%
    unnest(cols = hist)
  
  colour_vector <- user_colours(length(unique(df_hist$grouping)), col_palette)
  # add breakpoints to the df_hist
  df_hist <- df_hist %>%
    group_by(grouping) %>%
    mutate(breaks = mid - (mid[2] - mid[1])/2)
  # plot the histograms as grouped freq polygons
  #plt <- 

  plt <- df_hist %>%
    ggplot(aes(x = mid, y = prop, fill = grouping, breaks = breaks)) +
    geom_histogram(na.rm = TRUE, stat = 'identity', binwidth = mid[1] - mid[2]) + 
    scale_colour_manual(values = colour_vector) +
    facet_wrap(~ col_name, 
               scales = "free", 
               nrow = plot_layout[[1]], 
               ncol = plot_layout[[2]]) + 
    labs(x = "", y = "Probability", 
         title =  paste0("Histograms of numeric columns in df::", df_names$df1), 
         subtitle = paste0("Split by grouping variable: ", colnames(df_plot)[1])) + 
    theme(legend.position = "none") 

  plt
}





