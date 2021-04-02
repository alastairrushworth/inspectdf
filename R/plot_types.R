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
#' @importFrom stats chisq.test
#' @importFrom stats na.omit
#' @importFrom utils head

plot_types_1 <- function(
  df_plot, 
  df_names, 
  text_labels, 
  col_palette, 
  label_angle, 
  label_color, 
  label_size){
  
  # Get summary of the columns ready for radial plot
  column_layout <- df_plot %>%
    unnest(col_name) %>%
    select(-cnt, -pcnt) %>%
    mutate(ones       = 1, 
           tops       = cumsum(ones) / sum(ones), 
           bottoms    = c(0, head(tops, n = -1)), 
           label_pos  = (tops + bottoms) / 2, 
           text_just  = ifelse(label_pos > 0.5, 'right', 'left'), 
           text_rotn  = ifelse(label_pos > 0.5, -1, 1) * 90 - (label_pos * 360)) 
  # Get summary of the column types ready for radial plot
  types_layout <- 
    column_layout %>%
    group_by(type) %>%
    count() %>%
    ungroup %>%
    arrange(desc(n)) %>%
    mutate(
      tops       = cumsum(n) / sum(n), 
      bottoms    = c(0, head(tops, n = -1)), 
      label_pos  = (tops + bottoms) / 2, 
      text_just  = ifelse(label_pos > 0.5, 'center', 'center'), 
      text_rotn  = ifelse(label_pos > 0.5, -1, 1) * 90 - (label_pos * 360), 
      type_label = ifelse(label_pos > 0.5, paste0(type, ' (', n, ')'), paste0('(', n, ') ', type))
    )
  # Generate radial plot
  plt <- column_layout %>%
    ggplot(aes(ymax = tops, ymin = bottoms, xmax = 4, xmin = 3, fill = type)) +
    geom_rect() +
    geom_rect(aes(ymax = tops, ymin = bottoms, xmax = 3, xmin = -1, fill = type), alpha = 0.7) +
    geom_text(x = 5, 
              aes(y = label_pos, label = col_name, color = type, 
                  hjust = text_just, angle = text_rotn), size = 4) + 
    geom_text(x = 1.5, data = types_layout,
              aes(y = label_pos, label = type_label, 
                  hjust = text_just, angle = text_rotn),
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

plot_types_2 <- function(df_plot, df_names, text_labels, col_palette, 
                         label_angle, label_color, label_size, plot_type){
  
  # Get summary of the columns ready for radial plot
  column_list <- 
    df_plot %>%
    select(type, columns) %>%
    unnest(columns) %>%
    split(f = .$data_arg)
  # df_names for labelling
  df_names_labels <- tibble(df = df_names, y = 1.04, x = c(2.5, 3.5))
  
  
  # Join column names and types across the pair of inputs
  column_layout <- 
    full_join(x = column_list[[df_names[[1]]]], y = column_list[[df_names[[2]]]], by = c('col_name')) 
  
  # filter out non-issues if plottype != 1
  if(plot_type != 1) {
    column_layout <- 
      column_layout %>% 
      filter((type.x != type.y) | is.na(type.x) | is.na(type.y))
  }
  
  column_layout <- column_layout %>%
    select(-starts_with('data_arg'), df1 = type.x, df2 = type.y, col_name) %>%
    mutate(ones       = 1, 
           tops       = cumsum(ones) / sum(ones), 
           bottoms    = c(0, head(tops, n = -1)), 
           label_pos  = (tops + bottoms) / 2) 
  
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
    mutate(issue_fill = ifelse(is.na(cmmnt), NA, 
                               ifelse(grepl('missing', cmmnt), NA, 
                                      ifelse(grepl('<!>', cmmnt), df2, 'Else')))) %>%
    mutate(has_issue = ifelse(is.na(issue_fill), 'No issue', 'Issue')) %>%
    mutate(issue_x = as.numeric(!is.na(df1)))
  
  # create a color-scale based on types present
  type_order <- as.character(na.omit(unique(c(column_layout$df1, column_layout$df2))))
  col_types  <- c(user_colours(length(type_order), col_palette), 'gray90', 'gray90', 'white', 'tomato')
  type_order <- c(type_order, 'Missing', 'Type mismatch', 'No issue', 'Issue')
  names(col_types) <- type_order
  
  # LHS overlay information text 
  lhs_types_layout <-
    column_layout %>%
    group_by(df1) %>%
    count() %>%
    ungroup %>%
    arrange(desc(n)) %>%
    arrange(is.na(df1)) %>%
    mutate(
      tops       = cumsum(n) / sum(n),
      bottoms    = c(0, head(tops, n = -1)),
      label_pos  = (tops + bottoms) / 2,
      type_label = paste0(ifelse(is.na(df1), 'missing', df1), ' (', n, ')')
    )
  
  # RHS overlay information text (LHS missing)
  if(any(is.na(column_layout$df1))){
    rhs_types_layout <- 
      column_layout %>%
      filter(is.na(df1)) %>%
      arrange(-dplyr::row_number()) %>%
      mutate(n = 1:nrow(.)) %>%
      select(n, df1 = df2) %>%
      mutate(
        tops       = n / sum(lhs_types_layout$n),
        bottoms    = c(0, head(tops, n = -1)),
        label_pos  = (tops + bottoms) / 2,
        type_label = df1) 
  } else {
    rhs_types_layout <- tibble()
  }


  # Generate two-column comparison plot
  plt <- column_layout %>%
    mutate(
      df1 = factor(df1, levels = type_order), 
      df2 = factor(df2, levels = type_order)) %>%
    ggplot(aes(ymax = tops, ymin = bottoms, xmax = 3, xmin = 2, 
               fill = df1)) +
    # show the filled cells for the first data frame
    geom_rect(color = 'white', size = 0.06) +
    # show the filled cells for the second data frame
    geom_rect(aes(ymax = tops, ymin = bottoms, xmax = 4, xmin = 3, fill = df2), 
              alpha = 0.7, color = 'white', size = 0.06) +
    # Add exclamation marks to indicate problems
    geom_fit_text(aes(ymax = tops, ymin = bottoms, xmax = 4, 
                    xmin = 4.1, color = has_issue), label = '!')
  
  # LHS text overlay
  if(nrow(lhs_types_layout) > 0){
    plt <- plt + geom_fit_text(
      aes(xmin = 2, xmax = 3, ymin = bottoms, ymax = tops,
          label = type_label), colour = 'white',
      inherit.aes = FALSE, data = lhs_types_layout, angle = 0)
  }

  # RHS text overlay
  if(nrow(rhs_types_layout) > 0){
    plt <- plt + geom_fit_text(
      aes(xmin = 3, xmax = 4, ymin = 1 - bottoms, ymax = 1 - tops,
          label = type_label), colour = 'white',
      inherit.aes = FALSE, data = rhs_types_layout, angle = 0)
  }
  
  plt <- plt + 
    geom_fit_text(
      aes(xmin = 2 + issue_x, xmax = 3 + issue_x, 
          ymin = bottoms, ymax = tops, label = issue_fill), 
      angle = 0, color = 'white', inherit.aes = FALSE, 
      data = column_layout %>% filter(!is.na(issue_fill))) +
    geom_text(
      aes(y = label_pos, label = col_name, colour = df1,
          hjust = 'right', angle = 0), x = 1.8, size = 4) + 
    # add the data frame names at the top
    geom_text(aes(x = x, y = y, label = df), data = df_names_labels, 
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
