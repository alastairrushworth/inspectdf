#' @importFrom dplyr pull
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_blank
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer


plot_cor_single <- function(
    out, alpha = 0.05, 
    text_labels = TRUE, 
    col_palette = 0
){
  out$pair <- paste(out$col_1, out$col_2, sep = ' & ')
  # xlabels
  df_names <- attr(out, "df_names")
  method   <- attr(out, "method")
  mth_ind <- grep(paste0("^", method), c("pearson", "kendall", "spearman"), ignore.case = TRUE)
  xlab    <- c("Pearson's correlation", 
               "Kendall's rank correlation", 
               "Spearman's rank correlation")[mth_ind]
  # get a vector of signficance colors from theme
  vcols <- c("gray50", user_colours(9, col_palette)[9])
  # factorise pairs, add a sign variable and an index
  out <- out %>%
    filter(!is.na(.data$corr)) %>%
    mutate(pair = factor(.data$pair, levels = rev(unique(as.character(.data$pair)))),
           sign = as.factor(c("Negative", "Positive")[as.numeric(.data$corr > 0) + 1])) %>%
    arrange(desc(.data$pair)) %>%
    mutate(index = rev(row_number()))
  # generate coloured CIs and point estimate plot
  plt <- ggplot(out, aes(x = .data$pair, y = .data$corr)) +
    geom_hline(yintercept = 0, linetype = "dashed", 
               color = "lightsteelblue4", na.rm = TRUE) + 
    geom_rect(
      alpha = 0.4,
      xmin = out$index - 0.4, xmax = out$index + 0.4,
      ymin = out$lower, ymax = out$upper, linetype = 1, 
      fill = vcols[(out$p_value < alpha) + 1], 
      na.rm = TRUE) +
    geom_segment(x = out$index - 0.4, y = out$corr, 
                 xend = out$index + 0.4, 
                 yend = out$corr, col = "gray40", 
                 na.rm = TRUE) +
    coord_flip() + ylim(min(out$lower), max(out$upper)) + 
    labs(x = "", y = xlab, 
         title = paste0("Correlation of columns in df::", df_names$df1))
  plt
}


plot_cor_pair <- function(
    out, 
    alpha = 0.05, 
    text_labels = TRUE, 
    col_palette = 0
){
  df_names <- attr(out, "df_names")
  method   <- attr(out, "method")
  # xlabels
  mth_ind <- grep(tolower(paste0("^", method)), c("pearson", "kendal", "spearman"))
  xlab    <- c("Pearson's correlation", 
               "Kendall's rank correlation", 
               "Spearman's rank correlation")[mth_ind]
  # create tall data from correlation table
  out <- out %>%
    filter(!is.na(.data$corr_1), !is.na(.data$corr_2)) %>%
    mutate(pair = paste(.data$col_1, .data$col_2, sep = " & ")) %>%
    mutate(pair = factor(.data$pair, levels = rev(unique(as.character(.data$pair))))) %>%
    select(-"col_1", -"col_2") %>%
    pivot_longer(cols = c(-.data$pair, -.data$p_value), names_to = "data_frame", values_to = "corr") %>%
    mutate(data_frame = unlist(df_names)[as.integer(gsub("corr_", "", .data$data_frame))],
           data_frame_n = as.integer(as.factor(.data$data_frame)))
  # number of dataframes
  n_df <- length(unique(out$data_frame))
  # get a vector of signficance colors from theme
  vcols <- c("gray50", user_colours(9, col_palette)[9])
  # get a vector of bar colors from theme
  if(n_df == 2){
    bcols <- get_best_pair(col_palette)
  } else {
    bcols <- user_colours(n_df, col_palette)
  }
  # add bar colors, significance & bar positions
  out <- out %>%
    mutate(index = as.integer(.data$pair)) %>%
    mutate(data_frame_n = n_df - .data$data_frame_n + 1) %>%
    mutate(bar_dn = .data$index - 0.8 + .data$data_frame_n * (0.8 / n_df)) %>%
    mutate(bar_up = .data$index - 0.8 + (.data$data_frame_n + 1) * (0.8 / n_df)) %>%
    mutate(bar_bg = ifelse(.data$corr < 0, .data$corr, 0), bar_en = ifelse(.data$corr < 0, 0, .data$corr))
  # generate basic plot
  plt <- out %>%
    ggplot(aes(x = .data$pair, y = .data$corr, fill = .data$data_frame)) +
    geom_blank() + theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
    geom_rect(
      fill = vcols[as.integer(out$p_value < alpha) + 1], alpha = 0.2,
      xmin = out$index - 0.4, xmax = out$index + 0.4,
      ymin = -1, ymax = 1, linetype = "blank") +
    geom_rect(
      linetype = "blank",
      xmin = out$bar_dn, xmax = out$bar_up,
      ymin = out$bar_bg, ymax = out$bar_en) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "lightsteelblue4", na.rm = TRUE) +
    scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    coord_flip() +
    labs(y = xlab, x = "",
         title = paste0("Comparison of \u03C1 between df::", df_names$df1,
                        " and ", df_names$df2)) +
    scale_fill_manual(name = "Data frame", values = bcols)
  plt
}

plot_cor_grouped <- function(
    out, 
    alpha = 0.05, 
    text_labels = TRUE, 
    col_palette = 0, 
    plot_type = 1
){
  df_names <- attr(out, "df_names")
  method   <- attr(out, "method")
  # group variable name
  group_name <- colnames(out)[1]
  if(plot_type == 1){
    # get ordering of variable pairs by median correlation
    col_ord <- out %>%
      ungroup %>%
      mutate(pair = paste0(.data$col_1, ' & ', .data$col_2)) %>%
      group_by(.data$pair) %>%
      summarize(md_cor = median(.data$corr, na.rm = T)) %>%
      arrange(.data$md_cor) %>%
      pull(.data$pair)
    # create pair columns and arrange by col_ord
    out <- out %>%
      ungroup %>%
      mutate(pair = paste0(.data$col_1, ' & ', .data$col_2)) %>%
      mutate(pair = factor(.data$pair, levels = col_ord)) %>%
      arrange(.data$pair)
    # jitter points if number of column pairs <= 10
    jitter_width <- ifelse(length(unique(out$pair)) > 10, 0, 0.25)
    plt <- out %>%
      ggplot(aes(x = .data$pair, y = .data$corr, col = .data$pair, group = .data[[group_name]])) +
      geom_jitter(alpha = 0.5, width = jitter_width, size = 1.8, na.rm = TRUE) +
      theme(legend.position='none') +
      coord_flip() +
      ylab("Correlation by group") +
      xlab("")
  } else {
    new_out <- out %>%
      mutate(pair = paste(.data$col_1, .data$col_2, sep = " & ")) %>%
      mutate(pair = factor(.data$pair, levels = rev(unique(as.character(.data$pair))))) %>%
      select(-"col_1", -"col_2", -"lower", -"upper", -"p_value")
    
    plt <- plot_grouped(
      df = new_out, 
      value = "corr", 
      series = "pair", 
      group = group_name, 
      plot_type = plot_type, 
      col_palette = col_palette, 
      text_labels = text_labels,
      ylab = "Correlation")
    plt <- plt + 
      geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") + 
      geom_hline(yintercept = 1, alpha = 0.3, linetype = "dashed") + 
      geom_hline(yintercept = -1, alpha = 0.3, linetype = "dashed")
  }
  return(plt)
}





