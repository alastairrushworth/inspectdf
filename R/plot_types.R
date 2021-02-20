#' @importFrom dplyr matches
#' @importFrom dplyr count
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

plot_types_1 <- function(df_plot, 
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
                         label_angle, label_color, label_size){
  
  # Get summary of the columns ready for radial plot
  column_list <- 
    df_plot %>%
    select(type, columns) %>%
    unnest(columns) %>%
    split(f = .$data_arg)
  # dfnames for labelling
  df_names <- names(column_list)
  df_names <- tibble(df = df_names, y = 1.04, x = c(2.5, 3.5))
  
  
  # Layout of columns, polar coordinates and text rotations
  column_layout <- 
    full_join(x = column_list[[1]], y = column_list[[2]], by = c('col_name')) %>%
    select(-starts_with('data_arg'), df1 = type.x, df2 = type.y, col_name) %>%
    mutate(ones       = 1, 
           tops       = cumsum(ones) / sum(ones), 
           bottoms    = c(0, head(tops, n = -1)), 
           label_pos  = (tops + bottoms) / 2) 
  type_order <- as.character(na.omit(unique(c(column_layout$df1, column_layout$df2))))
  col_types  <- c(user_colours(length(type_order), 0), 'gray90', 'gray90', 'white', 'tomato')
  type_order <- c(type_order, 'Missing', 'Type mismatch', 'No issue', 'Issue')
  names(col_types) <- type_order
  
  # extract tibble of issue comments
  issue_vec <- unlist(df_plot$issues)
  issue_df  <- tibble(
    col_name = names(issue_vec), 
    cmmnt = issue_vec) %>%
    distinct(.keep_all = TRUE)
  column_layout <- column_layout %>%
    left_join(issue_df, by = 'col_name') %>%
    mutate(issue_fill = ifelse(is.na(cmmnt), NA, 
                               ifelse(grepl('missing', cmmnt), 'Missing', 
                                      ifelse(grepl('<!>', cmmnt), 'Type mismatch', 'Else')))) %>%
    mutate(has_issue = ifelse(is.na(issue_fill), 'No issue', 'Issue')) %>%
    mutate(issue_x = as.numeric(!is.na(df1)))
  
  
  # Get summary of the column types ready for radial plot
  types_layout <-
    column_layout %>%
    group_by(df1) %>%
    count() %>%
    ungroup %>%
    arrange(desc(n)) %>%
    mutate(
      tops       = cumsum(n) / sum(n),
      bottoms    = c(0, head(tops, n = -1)),
      label_pos  = (tops + bottoms) / 2,
      type_label = paste0(df1, ' (', n, ')')
    ) %>%
    filter(!is.na(df1))
  
  # Generate radial plot
  plt <- column_layout %>%
    mutate(
      df1 = factor(df1, levels = type_order), 
      df2 = factor(df2, levels = type_order)) %>%
    ggplot(aes(ymax = tops, ymin = bottoms, xmax = 3, xmin = 2, fill = df1)) +
    # shaded rectangles containing comments
    geom_fit_text(aes(ymax = tops, ymin = bottoms, xmax = 4, xmin = 4.1, color = has_issue), label = '!') +
    # show the columns for df1
    geom_rect(color = 'white', size = 0.06) +
    # show the columns for df2
    geom_rect(aes(ymax = tops, ymin = bottoms, xmax = 4, xmin = 3, fill = df2), 
              alpha = 0.7, color = 'white', size = 0.06) +
    geom_fit_text(
      aes(xmin = 2, xmax = 3, ymin = bottoms, ymax = tops,
          label = type_label), colour = 'white',
      inherit.aes = FALSE, data = types_layout, angle = 0) +
    geom_fit_text(
      aes(xmin = 2 + issue_x, xmax = 3 + issue_x, 
          ymin = bottoms, ymax = tops, label = issue_fill), 
      angle = 0, color = 'white', inherit.aes = FALSE, 
      data = column_layout %>% filter(!is.na(issue_fill))) +
    geom_text(
      aes(y = label_pos, label = col_name, colour = df1,
          hjust = 'right', angle = 0), x = 1.8, size = 4) + 
    # add the data frame names at the top
    geom_text(aes(x = x, y = y, label = df), data = df_names, 
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
