#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme

plot_num_single <- function(df_plot, df_names, plot_layout, text_labels, col_palette){
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # pull out breaks attribute from df_plot
  brks <- attr(df_plot, 'brks_list')
  # loop over rows in df_plot, add midpoints to the hist df
  for(nm in df_plot$col_name){
    hist_i     <- df_plot$hist[[nm]]
    brks_i     <- brks[[nm]]
    hist_i$mid <- brks_i[1:(length(brks_i) - 1)] + diff(brks_i ) / 2
    hist_i$col_name <- nm
    df_plot$hist[[nm]] <- hist_i
  }
  # add histogram midpoints
  df_plot <- bind_rows(df_plot$hist)
  bin_width <- df_plot %>% 
    group_by(col_name) %>%
    summarise(bar_width = diff(mid)[1] * 0.9)
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
  
  # generate basic plot with columns
  plt <- df_plot %>%
    ggplot(aes(x = mid, y = prop, width = bar_width, fill = prop_z)) + 
    geom_col() + 
    labs(
      x = "", y = "Probability", 
      title =  paste0("Histograms of numeric columns in df::", df_names$df1), 
      subtitle = ""
    ) +
    facet_wrap(
      ~ col_name, 
      scales = "free", 
      nrow = plot_layout[[1]], 
      ncol = plot_layout[[2]]
    )
  # colour fill can depend on user input, if provided
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

