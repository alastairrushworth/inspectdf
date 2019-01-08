#' Report column types of a data frame 
#'
#' @param df1 A data frame to report column types
#' @param df2 An optional second data frame for comparing column types with.  Defaults to \code{NULL}.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Prints the proportion of columns with each type.
#' @export
#' @details When the second data frame \code{df2} is specified, column types are tabulated for both data frames to enable comparison of contents.
#' @examples
#' data("starwars", package = "dplyr")
#' # get tibble of column types for the starwars data
#' report_types(starwars)
#' # get column types and show as barplot
#' report_types(starwars, show_plot = TRUE)
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom tidyr replace_na

report_types <- function(df1, df2 = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
    # number of columns
    ncl         <- ncol(df1)
    # possible types to look out for
    type_spine  <- tibble(col_type = c("logical", "integer", 
                                       "numeric", "character", 
                                       "factor",  "list",    
                                       "matrix",  "data.frame", 
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
      # convert column names to factor
      out_plot <- out %>% 
        mutate(col_type = factor(col_type, levels = as.character(col_type)))
      # construct bar plot of column types
      plt <- bar_plot(df_plot = out_plot, x = "col_type", y = "percent", 
                      fill = "col_type", label = "count_type", 
               ttl = paste0("Column type composition of df::", df_names$df1), 
               sttl = paste0("df::", df_names$df1,  " contains ", ncol(df1), 
                             " columns.  Count of each type shown on bar."), 
               ylb = "Percentage of columns (%)", lgnd = "Column types")
      # add text annotation to plot
      plt <- add_annotation_to_bars(x = out_plot$col_type, 
                                    y = out_plot$percent, 
                                    z = out_plot$count_type, 
                                    plt = plt, thresh = 0.1)
      # print plot
      print(plt)
    }
    return(out)
  } else {
    s1 <- report_types(df1, show_plot = F) %>% 
      rename(count_1 = count_type, percent_1 = percent)
    s2 <- report_types(df2, show_plot = F) %>% 
      rename(count_2 = count_type, percent_2 = percent)
    sjoin <- full_join(s1, s2, by = "col_type") %>% 
      replace_na(list(count_1 = 0, count_2 = 0, percent_1 = 0, percent_2 = 0))
    
    if(show_plot){
      # convert to a tall df
      z1 <- sjoin %>% select(-contains("percent_")) %>% 
        gather(key = "df_input", value = "count", -col_type) %>% 
        mutate(df_input = gsub("count_", "", df_input))
      z2 <- sjoin %>% select(-contains("count_")) %>% 
        gather(key = "df_input", value = "percent", -col_type) %>% 
        mutate(df_input = gsub("percent_", "", df_input))
      z_tall <- z1 %>% left_join(z2, by = c("col_type", "df_input")) %>%
        mutate(df_input = case_when(df_input == "1" ~ df_names$df1, TRUE ~ df_names$df2))
    
      # make axis names
      ttl_plt <- paste0("Column type composition of df::", 
                        df_names$df1, " & ", "df::", df_names$df2)
      sttl_plt1 <- paste0("df::", df_names$df1,  " contains ",
                          ncol(df1), " columns & ")
      sttl_plt2 <- paste0("df::", df_names$df2,  " contains ", 
                          ncol(df2), " columns.")
      # plot the result
      plt <- z_tall %>%
        mutate(col_type = factor(col_type, levels = sjoin$col_type)) %>%
        ggplot(aes(x = col_type, y = percent, 
                   fill = as.factor(df_input), label = count)) + 
        geom_bar(stat = "identity", position = "dodge") + 
        labs(x = "", y = "Percentage of columns (%)", title = ttl_plt, 
             subtitle = paste0(sttl_plt1, sttl_plt2)) + 
        scale_fill_discrete(name = "Data frame")
      print(plt)
    }
    
    return(sjoin)
  }
}








