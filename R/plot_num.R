#' @importFrom dplyr all_of
#' @importFrom dplyr pull
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_label
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 sec_axis
#' @importFrom ggplot2 theme
#' @importFrom rlang :=
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unnest

plot_num_single <- function(
    df_plot, 
    plot_layout = NULL, 
    text_labels = TRUE, 
    col_palette = 0
){
  df_names <- attr(df_plot, "df_names")
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
  return(plt)
}




plot_num_pair <- function(
    df_plot, 
    plot_layout = NULL, 
    text_labels = TRUE, 
    col_palette = 0,
    alpha = 0.05
){
  df_names <- attr(df_plot, "df_names")
  # set the plot_layout if not specified
  if(is.null(plot_layout)) plot_layout <- list(NULL, 3)
  # chop stuff off
  df_plot <- df_plot %>% select(-jsd) 
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
  trns_plot <- bind_rows(bind_rows(df_plot[[2]]), bind_rows(df_plot[[3]]))
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
    select(cname = col_name, pval) %>%
    mutate(significant = as.integer(pval < alpha) + 2) %>%
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
    geom_tile(colour = "white")
  # check for ggfittext install
  if(requireNamespace("ggfittext", quietly = TRUE)){
    plt <- plt + ggfittext::geom_fit_text(
      aes(label = round(prop * 100, 1)),
      contrast = TRUE,
      na.rm = TRUE)
  } else {
    plt <- plt + ggfittext::geom_fit_text(
      aes(label = round(prop * 100, 1)),
      contrast = TRUE,
      na.rm = TRUE)
  }
  
  plt <- plt + 
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme(legend.position = "none") +
    labs(x = "", y = "", 
         title =  paste0("Heat plot comparison of numeric columns")) + 
    facet_wrap(~ cname, scales = "free", 
               nrow = plot_layout[[1]], 
               ncol = plot_layout[[2]])  
  return(plt)
}






plot_num_grouped <- function(
    df_plot,
    plot_layout = 1, 
    text_labels = TRUE, 
    col_palette = 0, 
    alpha = 0.05
){
  pretty_print_num <- function(x){
    out <- 
      ifelse(x >= 1e12, sprintf("~%.1fT", x/1e12),
             ifelse(x>=1e9, sprintf("~%.1fB", x/1e9), 
                    ifelse(x>=1e6, sprintf("~%.1fM", x/1e6), 
                           ifelse(x>=1e3, sprintf("~%.1fk", x/1e3), 
                                  prettyNum(x, digits = 4)))))
    return(out)
  }
  df_names <- attr(df_plot, "df_names")
  # extract plot type
  plot_type   <- attr(df_plot, 'type')$input_type
  # grouping variable
  grp_var     <- colnames(df_plot)[1]
  # grouping values
  grp_nms     <- df_plot[[grp_var]]
  # extract breaks attributes
  brks        <- attr(df_plot, 'brks_list')
  
  # convert first column of df_plot to a factor
  # ensure order is correct if grouping variable was numeric
  if(inherits(grp_nms, 'numeric')){
    group_order  <- as.character(sort(unique(grp_nms)))
  } else {
    # group_order  <- as.character(unique(grp_nms))
    group_order <- attr(df_plot, 'group_lengths') %>%
      arrange(rank_mean) %>%
      pull(grp_var)
  }
  attr(df_plot, 'group_lengths') %>%
    mutate(!!(grp_var) := as.character(.data[[grp_var]]))
  
  df_plot <- df_plot %>%
    mutate(!!(grp_var) := factor(as.character(grp_nms), levels = group_order)) %>%
    mutate(df_int = as.integer(.data[[grp_var]]))
  # get global (ungrouped) statistics 
  grp_lengths <- tibble(!!(grp_var) := group_order) %>% 
    left_join(attr(df_plot, 'group_lengths') %>%
                mutate(!!(grp_var) := as.character(.data[[grp_var]])), by = grp_var) %>%
    mutate(!!(grp_var) := factor(as.character(.data[[grp_var]]), levels = group_order))
  
  # get global mean, min and max for graphics
  global_stats <- df_plot %>% 
    left_join(grp_lengths, by = grp_var) %>%
    group_by(col_name) %>%
    summarise(
      xmin = min(min, na.rm = TRUE), 
      xmax = max(max, na.rm = TRUE), 
      xmn  = sum((rows * mean)) / sum(rows)
    )
  
  # if the plot is paired, then rename the hist comparison cols and 
  if(plot_type == 'pair'){
    df_names <- as.character(unlist(attr(df_plot, 'df_names')))
    hist_col_names <- c('hist_1', 'hist_2')
    names(hist_col_names) <- df_names
    df_plot <- df_plot %>% 
      rename(all_of(hist_col_names)) %>%
      pivot_longer(cols = c(as.character(df_names)), values_to = 'hist')
  }
  
  # loop over rows in df_plot, add midpoints to each hist df for plotting
  for(i in seq_along(1:nrow(df_plot))){
    nm      <- df_plot$col_name[i]
    hist_i  <- df_plot$hist[[i]]
    brks_i  <- brks[[nm]]
    hist_i$mid <- brks_i[1:(length(brks_i) - 1)] + diff(brks_i ) / 2
    df_plot$hist[[i]] <- hist_i
  }
  
  # pull out the histograms into a separate object
  # unnest to long format
  hists_long <- df_plot %>% 
    select(.data[[grp_var]], col_name, hist) %>%
    unnest(hist)
  # work out the bin widths for each column and join back
  bin_width <- hists_long %>% 
    group_by(col_name) %>%
    summarise(bar_width = diff(sort(unique(mid)))[1] * 0.9)
  hists_long <- hists_long %>%
    left_join(bin_width, by = 'col_name') %>%
    mutate(df_int = as.integer(.data[[grp_var]])) %>%
    mutate(key_to_color = as.factor(1:nrow(.))) %>% 
    filter(prop > 0)
  hists_long$prop[is.na(hists_long$prop)] <- 0
  # number of columns in the data
  ncolumns   <- length(unique(hists_long$col_name))
  vcols      <- sapply(user_colours(ncolumns, col_palette), get_shade_ramp)
  col_inds   <- cbind(
    round((1 - sqrt(hists_long$prop)) * 1000, 0), 
    as.integer(as.factor(hists_long$col_name))
  )
  col_inds[col_inds[,1] == 0, 1] <- 1
  colour_vector <- vcols[col_inds]
  
  # statistics associated with each data frame
  stats <- df_plot %>% 
    filter(col_name %in% df_plot$col_name) %>% 
    left_join(bin_width, by = 'col_name') %>% 
    select(.data[[grp_var]], col_name, min, mean, max, bar_width, df_int)
  
  # pivot the stats dataframe to long
  stats_mn <- stats %>%
    pivot_longer(cols = c('mean'), names_to = "stat_type") %>%
    mutate(linetype = 'solid')
  xlabs <- df_plot %>% distinct(.data[[grp_var]], df_int)
  
  # global stats - pivot long and add a print friendly label string
  # format text for printing on the graphic
  fmt <- function(x) prettyNum(x, digits = 4)
  global_stats <- global_stats %>%
    pivot_longer(-col_name) %>% 
    left_join(bin_width, by = 'col_name') %>%
    mutate(print_value = fmt(value)) %>%
    mutate(xcoord = mean(unique(hists_long$df_int)) + 0.87/2 - 1)
  
  # set up the plot regions
  pp <- hists_long %>%
    ggplot(
      aes(
        ymin = (mid - bar_width / 2) / bar_width, 
        ymax = (mid + bar_width / 2) / bar_width, 
        xmin = (df_int - 1),
        xmax = (df_int - 1 + 0.87),
        fill = key_to_color
      )
    ) 
  
  # add gray max / min / mean lines as global guides
  pp <- 
    pp + 
    geom_hline(
      na.rm = TRUE,
      data = global_stats, 
      aes(
        yintercept = value / bar_width,
        group = col_name
      ), 
      color = 'gray60'
    )
  
  # add rectangles histograms where there are non-zero probs
  pp <- 
    pp + 
    geom_rect(na.rm = TRUE) +
    scale_fill_manual(values = colour_vector) +
    facet_grid(col_name~., scales = 'free', switch = 'y') +
    theme(
      legend.position  = 'none',
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      axis.text.y      = element_blank(), 
      axis.text.x      = element_blank()
    ) + 
    labs(
      x = grp_var,
      y = ''  
    )
  # scale_x_continuous(
  #   breaks = xlabs$df_int - 0.55, 
  #   labels = xlabs[[grp_var]],
  #   sec.axis = sec_axis(
  #     trans ~.,
  #     breaks = xlabs$df_int - 0.55,
  #     labels = pretty_print_num(grp_lengths$rows)
  #   )
  # )
  # xlabs %>% arrange(df_int) %>% print(n = 50)
  xlabs$yax <- rep(-0.5, nrow(xlabs))
  xlabs$col_name <- rev(sort(hists_long$col_name))[1]
  # pp <- pp + geom_label(
  #   inherit.aes = FALSE,
  #   data = xlabs,
  #   aes(x = df_int - 0.55, y = yax, label = .data[[grp_var]])
  #   )
  
  pp <- pp + 
    geom_fit_text(
      inherit.aes = FALSE, 
      data = xlabs,
      aes(
        ymin = -10, 
        ymax = 0, 
        xmin = df_int - 1, 
        xmax = df_int, 
        label = .data[[grp_var]]
      )) +
    coord_cartesian(clip = 'off')
  
  # ggplot(presidential, aes(ymin = start, ymax = end, x = party, label = name)) +
  #   geom_fit_text(grow = TRUE) +
  #   geom_errorbar(alpha = 0.5)
  
  # add line segments for group means
  pp <- 
    pp + 
    geom_segment(
      color = 'gray60',
      data = stats_mn, 
      na.rm = TRUE,
      aes(
        y    = value / bar_width, 
        yend = value / bar_width,
        x    = df_int - 1,
        xend = df_int - 1 + 0.87,
        linetype = linetype
      ), 
      inherit.aes = FALSE
    )
  
  # add max / mins line segments
  stats_max_min <- stats %>% 
    select(.data[[grp_var]], col_name, min, max, bar_width, df_int) %>%
    distinct() %>%
    pivot_longer(cols = c('min', 'max'), names_to = 'stat_type')
  pp <- 
    pp + 
    geom_segment(
      na.rm = TRUE,
      data = stats_max_min, 
      aes(
        y    = value / bar_width, 
        yend = value / bar_width,
        x    = (df_int - 1), 
        xend = (df_int - 1 + 0.87), 
        group = col_name
      ), 
      color = 'gray60',
      inherit.aes = FALSE)
  max_min_stats <- stats %>% 
    select(min, max, bar_width, df_int, col_name) %>% 
    distinct()
  max_min_stats <- bind_rows(
    max_min_stats, 
    (max_min_stats %>% mutate(df_int = df_int + 0.87))
  )
  pp <- 
    pp + 
    geom_segment(
      na.rm = TRUE,
      data = max_min_stats, 
      aes(
        y    = min / bar_width, 
        yend = max / bar_width,
        x    = (df_int - 1), 
        xend = (df_int - 1), 
        group = col_name
      ), 
      color = 'gray60',
      inherit.aes = FALSE)
  
  # add text label to max min lines
  pp <-
    pp +
    geom_label(
      na.rm = TRUE,
      data = global_stats %>% filter(name != 'xmn'),
      aes(
        y = value / bar_width,
        x = xcoord,
        label = print_value
      ),
      color = 'gray60', 
      inherit.aes = FALSE, 
      label.size = NA, 
      size = 3, 
      hjust = 0.5,
      fontface = 'italic'
    )
  return(pp)
  
}

