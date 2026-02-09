#' @importFrom dplyr matches
#' @importFrom dplyr count
#' @importFrom dplyr row_number
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_void
#' @importFrom ggfittext geom_fit_text
#' @importFrom rlang .data
#' @importFrom stats chisq.test
#' @importFrom stats na.omit
#' @importFrom utils head

plot_types_single <- function(
  df_plot, 
  text_labels = TRUE, 
  col_palette = 0, 
  label_angle = NULL, 
  label_color = NULL, 
  label_size = NULL, 
  plot_type = 1
){
  df_names <- attr(df_plot, "df_names")
  # Get summary of the columns ready for radial plot
  column_layout <- df_plot %>%
    unnest("col_name") %>%
    select(-"cnt", -"pcnt") %>%
    mutate(
      ones       = 1,
      tops       = cumsum(.data$ones) / sum(.data$ones),
      bottoms    = c(0, head(.data$tops, n = -1)),
      label_pos  = (.data$tops + .data$bottoms) / 2,
      text_just  = ifelse(.data$label_pos > 0.5, 'right', 'left'),
      text_rotn  = ifelse(.data$label_pos > 0.5, -1, 1) * 90 - (.data$label_pos * 360)) 
  # Get summary of the column types ready for radial plot
  types_layout <-
    column_layout %>%
    group_by(.data$type) %>%
    count() %>%
    ungroup %>%
    arrange(desc(.data$n)) %>%
    mutate(
      tops       = cumsum(.data$n) / sum(.data$n),
      bottoms    = c(0, head(.data$tops, n = -1)),
      label_pos  = (.data$tops + .data$bottoms) / 2,
      text_just  = ifelse(.data$label_pos > 0.5, 'center', 'center'),
      text_rotn  = ifelse(.data$label_pos > 0.5, -1, 1) * 90 - (.data$label_pos * 360),
      type_label = ifelse(.data$label_pos > 0.5, paste0(.data$type, ' (', .data$n, ')'),
                          paste0('(', .data$n, ') ', .data$type))
    )
  # Generate radial plot
  plt <- column_layout %>%
    ggplot(aes(ymax = .data$tops, ymin = .data$bottoms, xmax = 4, xmin = 3, fill = .data$type)) +
    geom_rect() +
    geom_rect(
      aes(ymax = .data$tops, ymin = .data$bottoms, xmax = 3, xmin = -1, fill = .data$type),
      alpha = 0.7) +
    geom_text(x = 5,
              aes(y = .data$label_pos, label = .data$col_name, color = .data$type,
                  hjust = .data$text_just, angle = .data$text_rotn), size = 4) +
    geom_text(x = 1.5, data = types_layout,
              aes(y = .data$label_pos, label = .data$type_label,
                  hjust = .data$text_just, angle = .data$text_rotn),
              inherit.aes = FALSE,
              color = 'white',
              size = 3) +
    scale_fill_manual(values = user_colours(nrow(types_layout), col_palette)) + 
    coord_polar(theta = "y") +
    xlim(c(-1, 8)) +
    theme_void() +
    theme(legend.position = "none")
  # Return plot object
  plt
}

plot_types_pair <- function(
    df_plot, 
    text_labels = TRUE, 
    col_palette = 0, 
    label_angle = NULL, 
    label_color = NULL, 
    label_size = NULL, 
    plot_type = 1
){
  df_names <- attr(df_plot, "df_names")
  # Get summary of the columns ready for radial plot
  column_list <-
    df_plot %>%
    select("type", "columns") %>%
    unnest("columns")
  column_list <- split(column_list, f = column_list$data_arg)
  # df_names for labelling
  df_names_labels <- tibble(df = df_names, y = 1.04, x = c(2.5, 3.5))
  
  
  # Join column names and types across the pair of inputs
  column_layout <-
    full_join(
      column_list[[df_names[[1]]]],
      column_list[[df_names[[2]]]],
      by = c('col_name')
    )

  # filter out non-issues if plottype != 1
  if(plot_type != 1) {
    column_layout <-
      column_layout %>%
      filter((.data$type.x != .data$type.y) | is.na(.data$type.x) | is.na(.data$type.y))
  }

  column_layout <- column_layout %>%
    select(-starts_with('data_arg'), df1 = "type.x", df2 = "type.y", "col_name") %>%
    mutate(ones       = 1,
           tops       = cumsum(.data$ones) / sum(.data$ones),
           bottoms    = c(0, head(.data$tops, n = -1)),
           label_pos  = (.data$tops + .data$bottoms) / 2) 
  
  # extract tibble of issue comments and combine with column df
  issue_vec <- unlist(df_plot$issues)
  if(!is.null(issue_vec)){
    column_layout <- 
      column_layout %>%
      left_join(tibble(
        col_name = names(issue_vec), 
        cmmnt = issue_vec) %>%
          distinct(.keep_all = TRUE), by = 'col_name')
  } else {
    column_layout <- column_layout %>% mutate(cmmnt = NA)
  }
  
  # create an issue indicator
  column_layout <-
    column_layout %>%
    mutate(issue_fill = ifelse(is.na(.data$cmmnt), NA,
                               ifelse(grepl('missing', .data$cmmnt), NA,
                                      ifelse(grepl('<!>', .data$cmmnt), .data$df2, 'Else')))) %>%
    mutate(has_issue = ifelse(is.na(.data$issue_fill), 'No issue', 'Issue')) %>%
    mutate(issue_x = as.numeric(!is.na(.data$df1)))
  
  # create a color-scale based on types present
  type_order <- as.character(na.omit(unique(c(column_layout$df1, column_layout$df2))))
  col_types  <- c(user_colours(length(type_order), col_palette), 'gray90', 'gray90', 'white', 'tomato')
  type_order <- c(type_order, 'Missing', 'Type mismatch', 'No issue', 'Issue')
  names(col_types) <- type_order
  
  # LHS overlay information text
  lhs_types_layout <-
    column_layout %>%
    group_by(.data$df1) %>%
    count() %>%
    ungroup %>%
    arrange(desc(.data$n)) %>%
    arrange(is.na(.data$df1)) %>%
    mutate(
      tops       = cumsum(.data$n) / sum(.data$n),
      bottoms    = c(0, head(.data$tops, n = -1)),
      label_pos  = (.data$tops + .data$bottoms) / 2,
      type_label = paste0(ifelse(is.na(.data$df1), 'missing', .data$df1), ' (', .data$n, ')')
    )

  # RHS overlay information text (LHS missing)
  if(any(is.na(column_layout$df1))){
    rhs_types_layout <-
      column_layout %>%
      filter(is.na(.data$df1)) %>%
      arrange(-dplyr::row_number()) %>%
      mutate(n = row_number()) %>%
      select("n", df1 = "df2") %>%
      mutate(
        tops       = .data$n / sum(lhs_types_layout$n),
        bottoms    = c(0, head(.data$tops, n = -1)),
        label_pos  = (.data$tops + .data$bottoms) / 2,
        type_label = .data$df1)
  } else {
    rhs_types_layout <- tibble()
  }


  # Generate two-column comparison plot
  plt <- column_layout %>%
    mutate(
      df1 = factor(.data$df1, levels = type_order),
      df2 = factor(.data$df2, levels = type_order)) %>%
    ggplot(aes(ymax = .data$tops, ymin = .data$bottoms, xmax = 3, xmin = 2,
               fill = .data$df1)) +
    # show the filled cells for the first data frame
    geom_rect(color = 'white', linewidth = 0.06) +
    # show the filled cells for the second data frame
    geom_rect(aes(ymax = .data$tops, ymin = .data$bottoms, xmax = 4, xmin = 3, fill = .data$df2),
              alpha = 0.7, color = 'white', linewidth = 0.06) +
    # Add exclamation marks to indicate problems
    geom_fit_text(aes(ymax = .data$tops, ymin = .data$bottoms, xmax = 4,
                    xmin = 4.1, color = .data$has_issue), label = '!')
  
  # LHS text overlay
  if(nrow(lhs_types_layout) > 0){
    plt <- plt + geom_fit_text(
      aes(xmin = 2, xmax = 3, ymin = .data$bottoms, ymax = .data$tops,
          label = .data$type_label), colour = 'white',
      inherit.aes = FALSE, data = lhs_types_layout, angle = 0)
  }

  # RHS text overlay
  if(nrow(rhs_types_layout) > 0){
    plt <- plt + geom_fit_text(
      aes(xmin = 3, xmax = 4, ymin = 1 - .data$bottoms, ymax = 1 - .data$tops,
          label = .data$type_label), colour = 'white',
      inherit.aes = FALSE, data = rhs_types_layout, angle = 0)
  }

  plt <- plt +
    geom_fit_text(
      aes(xmin = 2 + .data$issue_x, xmax = 3 + .data$issue_x,
          ymin = .data$bottoms, ymax = .data$tops, label = .data$issue_fill),
      angle = 0, color = 'white', inherit.aes = FALSE,
      data = column_layout %>% filter(!is.na(.data$issue_fill))) +
    geom_text(
      aes(y = .data$label_pos, label = .data$col_name, colour = .data$df1,
          hjust = 'right', angle = 0), x = 1.8, size = 4) +
    # add the data frame names at the top
    geom_text(aes(x = .data$x, y = .data$y, label = .data$df), data = df_names_labels,
              hjust = 'center', vjust = 'center', angle = 0,
              size = 4, color = 'gray40', inherit.aes = FALSE) +
    scale_fill_manual(values  = col_types, na.value = 'gray60') +
    scale_color_manual(values = col_types, na.value = 'gray60') + 
    xlim(c(1, 5)) +
    ylim(c(0, 1.05)) +
    theme_void() +
    theme(legend.position = "none")
  plt
}
