#' Report column types of a data frame 
#'
#' @param df1 A data frame to report column types
#' @param df2 An optional second data frame for comparing column types with.  Defaults to \code{NULL}.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Prints the proportion of columns with each type.
#' @details When the second data frame \code{df2} is specified, column types are tabulated for both data frames to enable comparison of contents.
#' @examples
#' # get tibble of column types for the starwars data
#' report_types(starwars)
#' # get column types and show as barplot
#' report_types(starwars, show_plot = T)

report_types <- function(df1, df2 = NULL, show_plot = F){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame name
  df_name <- as.character(substitute(df1))
  ee <- find_chain_parts()$lhs
  if(!is.null(ee)) df_name <- deparse(ee)
  
  if(is.null(df2)){
    # number of columns
    ncl         <- ncol(df1)
    # possible types to look out for
    type_spine  <- tibble(col_type = c("logical", "integer", "numeric", "character", 
                                   "factor",  "list",    "matrix",  "data.frame", 
                                   "ordered factor"))
    classes     <- sapply(df1, class)
    classes     <- sapply(classes, paste, collapse = " ")
    types       <- table(classes)
    type_tibble <- tibble(col_type = names(types), count_type = as.integer(types))
    # summarise column types into df1
    out <- left_join(type_spine, type_tibble, by = "col_type") %>%
      replace_na(list(count_type = 0))     %>%
      mutate(percent = 100 * count_type / ncol(df1)) %>% 
      arrange(desc(percent))         %>% 
      filter(percent > 0) 
    
    # if plot requested then show barplot
    if(show_plot){
      ttl_plt <- paste0("Column type composition of df::", df_name)
      sttl_plt <- paste0("df::", df_name,  " contains ", ncol(df1), " columns.  Count of each type shown on bar.")
      plt <- out %>% 
        dplyr::mutate(col_type = factor(col_type, levels = as.character(col_type))) %>%
        ggplot2::ggplot(ggplot2::aes(x = col_type, y = percent, fill = col_type, label = count_type)) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::labs(x = "", y = "Percentage of columns (%)", title = ttl_plt, subtitle = sttl_plt) + 
        ggplot2::scale_fill_discrete(name = "Column types") +
        ggplot2::geom_text(nudge_y = -3, color = "white")
      print(plt)
    }
    return(out)
  } else {
    s1 <- report_types(df1, show_plot = F) %>% dplyr::rename(count_1 = count_type, percent_1 = percent)
    s2 <- report_types(df2, show_plot = F) %>% dplyr::rename(count_2 = count_type, percent_2 = percent)
    sjoin <- dplyr::full_join(s1, s2, by = "col_type") %>% 
      replace_na(list(count_1 = 0, count_2 = 0, percent_1 = 0, percent_2 = 0))
    
    if(show_plot){
      # gather df names
      df_name1 <- as.character(substitute(df1))
      df_name2 <- as.character(substitute(df2))
      if(length(df_name1) > 1) df_name1 <- df_name1[2]
      if(length(df_name2) > 1) df_name2 <- df_name2[2]
      if(df_name1 == df_name2) df_name2 <- paste0(df_name2, "_2")
      # convert to a tall df
      z1 <- sjoin %>% select(-contains("percent_")) %>% 
        gather(key = "df_input", value = "count", -col_type) %>% 
        mutate(df_input = gsub("count_", "", df_input))
      z2 <- sjoin %>% select(-contains("count_")) %>% 
        gather(key = "df_input", value = "percent", -col_type) %>% 
        mutate(df_input = gsub("percent_", "", df_input))
      z_tall <- z1 %>% left_join(z2, by = c("col_type", "df_input")) %>%
        mutate(df_input = case_when(df_input == "1" ~ df_name1, TRUE ~ df_name2))
    
      # make axis names
      ttl_plt <- paste0("Column type composition of df::", df_name1, " & ", "df::", df_name2)
      sttl_plt1 <- paste0("df::", df_name1,  " contains ", ncol(df1), " columns & ")
      sttl_plt2 <- paste0("df::", df_name2,  " contains ", ncol(df2), " columns.")
      # plot the result
      plt <- z_tall %>%
        dplyr::mutate(col_type = factor(col_type, levels = sjoin$col_type)) %>%
        ggplot2::ggplot(ggplot2::aes(x = col_type, y = percent, fill = as.factor(df_input), label = count)) + 
        ggplot2::geom_bar(stat = "identity", position = "dodge") + 
        ggplot2::labs(x = "", y = "Percentage of columns (%)", title = ttl_plt, subtitle = paste0(sttl_plt1, sttl_plt2)) + 
        ggplot2::scale_fill_discrete(name = "Data frame")# +
        # ggplot2::geom_text(nudge_y = -3, color = "white")
      print(plt)
    }
    
    return(sjoin)
  }
}








