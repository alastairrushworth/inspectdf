#' @importFrom dplyr if_any
#' @importFrom dplyr any_of
#' @importFrom dplyr pull
#' @importFrom dplyr select_if
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_colour_manual
#' 

plot_cat <- function(
    levels_df, text_labels = TRUE, 
    high_cardinality = 0, 
    cols = c("tomato3", "gray65", "darkmagenta"), 
    col_palette = 0, label_thresh = 0.1, label_color = NULL, label_angle = NULL, 
    label_size = NULL){

  df_names <- attr(levels_df, "df_names")
  # retain order of column names for plotting
  # axes are flipped by default so reverse colnames should appear in reverse
  column_name_order <- rev(levels_df$col_name)

  # select the list column containing frequency tables
  # either there are one or two columns depending on whether this is 
  # a summary of a comparison
  levels_df <- levels_df %>% filter(if_any(any_of("jsd"), ~!is.na(.x)))
  lvl_df   <- levels_df %>% select_if(is.list) 
  lstnms   <- colnames(lvl_df)
  is_onedf <- ncol(lvl_df) == 1
  
  # loop over columns to collapse out high cardinality categories
  new_lvls <- list()
  for(i in seq_along(lstnms)){
    nm         <- lstnms[i]
    lvl_df[[nm]] <- lapply(lvl_df[[nm]], merge_high_cardinality, card_thresh = high_cardinality)
    new_lvls[[i]] <- collapse_levels(lvl_df, i)
    new_lvls[[i]]$dfi <- df_names[[i]]
  }
  # combine into a single dataframe (if more than a single list)
  lvl_df <- bind_rows(new_lvls) %>% mutate(col_name2 = col_name)
  
  # some extra steps for comparisons
  if(!is_onedf){
    lvl_df <- lvl_df %>% mutate(col_name = paste0(col_name, ": ", dfi))
    # update the column name order 
    column_name_order <- apply(
      expand.grid(rev(unique(lvl_df$dfi)), column_name_order),
      1, function(v) paste0(v[2], ': ', v[1])
    )
  }

  # add new keys and arrange
  lvl_df2 <- lvl_df %>% 
    mutate(new_level_key = paste0(level_key, "-", dfi)) 
  lvl_df2 <- split(lvl_df2, f = factor(lvl_df2$col_name, levels = column_name_order))
  # ensure high cardinality categories appear at the end (if specified)
  lvl_df2 <- bind_rows(lapply(lvl_df2, move_card))
  
  # create keys for plotting
  lvl_df2 <- lvl_df2 %>%
    mutate(new_level_key = factor(new_level_key, 
                                  levels = unique(new_level_key))) %>%
    mutate(level_key = factor(level_key, 
                              levels = unique(level_key))) %>%
    mutate(col_name  = factor(col_name, levels = column_name_order)) %>%
    mutate(col_name2 = factor(col_name2, levels = rev(sort(unique(col_name2)))))

  # vector of colours for plotting
  ncolumns   <- length(unique(lvl_df2$col_name2))
  vcols      <- sapply(user_colours(ncolumns, col_palette), get_shade_ramp)
  col_inds   <- cbind(round(lvl_df2$colvalstretch * 1000, 0), as.integer(lvl_df2$col_name2))
  colour_vector <- vcols[col_inds]
  colour_vector[is.na(lvl_df2$value)] <- cols[2]
  colour_vector[lvl_df2$value == "High cardinality"] <- cols[3]
  # generate plot
  plt <- lvl_df2 %>%
    ggplot(aes(x = col_name, y = prop, fill = new_level_key)) +
    geom_bar(stat = "identity", position = "stack", colour = "black", size = 0.2) +
    scale_fill_manual(values = colour_vector) +
    theme(legend.position = 'none') + 
    coord_flip() +
    theme(
      axis.title.y     = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y     = element_blank(), 
      panel.border     = element_blank(), 
      panel.grid.major = element_blank(), 
      axis.title.x     = element_blank(), 
      axis.text.x      = element_blank(), 
      axis.ticks.x     = element_blank()) +
    labs(
      x = "", y = "", 
      subtitle = bquote("Gray segments are missing values")) 

  if(text_labels){
    lvl_df3 <- lvl_df2 
    annts <- lvl_df3 %>% 
      mutate(col_num = as.integer(col_name)) 
    lvl_df3$col_vec <- factor(as.integer(annts$colvalstretch < 0.7), levels = c(1, 0))
    lvl_df3$value[nchar(lvl_df3$value) == 0] <- '""'
    
    sum_small_cats <- function(lvldf, prop_thresh){
      # make levels df as a list
      lvldf_grp <- lvldf %>% group_by(col_name) %>% tidyr::nest()
      lvldf_lst <- lvldf_grp$data
      # loop over list elements, sum and reorder components
      for(i in 1:length(lvldf_lst)){
        lst_i <- lvldf_lst[[i]]
        b1    <- lst_i %>% filter(prop < prop_thresh)
        if(nrow(b1) > 0){
          b1top <- b1 %>% slice(1)
          b1top$prop[1]  <- sum(b1$prop, na.rm = T)
          b1top$value[1] <- NA
          # combine the summed part with the rest
           lst_i <- lst_i %>% 
            filter(prop >= prop_thresh) %>% 
            bind_rows(b1top) %>%
            arrange(value)
          # if high card is in there, reposition to top
          hc_i <- which(lst_i$col_vec == 0)
          if(length(hc_i) > 0) lst_i <- rbind(lst_i[hc_i, ], lst_i[-hc_i, ])
          lvldf_lst[[i]] <- lst_i
        } else {
          lvldf_lst[[i]] <- lst_i
        }
      }
      lvldf_grp$data <- lvldf_lst
      return(tidyr::unnest(lvldf_grp, cols = data))
    }
    
    lvl_df4     <- lvl_df3 %>% sum_small_cats(prop_thresh = label_thresh)
    label_color <- if(is.null(label_color)){
      c("white", "gray55")
    } else {
      if(length(label_color) == 1){
        c(label_color, label_color)
      } else {
        label_color
      }
    }
    # check for ggfittext install
    if(requireNamespace("ggfittext", quietly = TRUE)){
      plt <- plt + 
        suppressWarnings(
          ggfittext::geom_fit_text(
            data = lvl_df4,
            aes(x = col_name,
                y = prop,
                label = value, 
                fill = new_level_key,
                colour = col_vec,
                ymin = 0,
                ymax = prop),
            inherit.aes = FALSE,
            na.rm = TRUE,
            position = "stack",
            place = "middle",
            grow = FALSE,
            outside = FALSE,
            show.legend = FALSE
          ) 
        )
    } else {
      lvl_df4$value[lvl_df4$prop < 0.15] <- NA
      col_vec <- ifelse((lvl_df4$colvalstretch > 0.7), 2, 1)
      plt <- plt + 
        suppressWarnings(
          geom_text(
            data = lvl_df4,
            aes(x = col_name, 
                y = colval - prop / 2, 
                label = value), 
            color = c("white", "gray55")[col_vec], 
            inherit.aes = FALSE, 
            na.rm = TRUE, 
            hjust = 0.5)
        )
    }
    plt <- plt + scale_colour_manual(values = label_color)
  }
  
  # if this is a comparison, then add x-axis labels and descriptive title
  if(is_onedf){
    ttl <- paste0("Frequency of categorical levels in df::", 
                  df_names$df1)
  } else {
    split_labs <- strsplit(levels(lvl_df2$col_name), ": ")
    newlabs   <- sapply(split_labs, function(v) paste(rev(v), collapse = ": "))
    ttl <- paste0("Categorical levels in df::", 
                  df_names$df1, " vs. df::", df_names$df2)
    plt <- plt + 
      scale_x_discrete(labels = newlabs)
  }
  # add title
  plt <- plt + labs(title = ttl)
  plt
}

# function to merge high cardinality categories entries into a single level
merge_high_cardinality <- function(z, card_thresh){
  z %>% 
    filter(cnt <= card_thresh) %>%
    summarise(prop = sum(prop), cnt = sum(cnt)) %>%
    bind_cols(value = "High cardinality", .) %>%
    bind_rows(z %>% filter(cnt > card_thresh), .) %>% 
    select(-cnt)
}

# function
collapse_levels <- function(dfi, i){
  out <- dfi %>% 
    dplyr::pull(i) %>%
    bind_rows(., .id = 'col_name') %>% 
    group_by(col_name) %>%
    mutate(colval = cumsum(prop)) %>% 
    mutate(colvalstretch = (colval - min(colval) + 0.001)/
             (max(colval) - min(colval) + 0.001)) %>%
    mutate(colvalstretch = colvalstretch * (1 - 0.8 * (1/length(colval)))) %>%
    ungroup %>%
    arrange(col_name) %>%
    mutate(level_key = paste0(value, "-", col_name))
  return(out)
}

# # move high cardinality to the end of each column block
move_card <- function(M){
  which_card <- which(M$value == "High cardinality")
  if(which_card > 0 & nrow(M) > 1){
    wo_c <- M[-which_card[1], ]
    wo_c_r <- wo_c[nrow(wo_c):1, ]
    M <- rbind(M[which_card[1], ], wo_c_r)
  }
  return(M)
}
