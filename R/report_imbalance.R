#' Report the most commonly occuring value in each non-numeric column
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing feature imbalance with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{value_1}, \code{percent_1}, \code{value_2}, \code{percent_2}.  The \code{value} is the most frequently occuring category in each column and \code{percent_in_col} is the percentage frequency with which it occurs.
#' @examples
#' report_imbalance(starwars)
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice

report_imbalance <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
    # pick out categorical columns
    df_cat <- df1 %>% select_if(function(v) is.character(v) | is.factor(v))
    # calculate imbalance if any columns available
    if(ncol(df_cat) > 0){
      cnames      <- colnames(df_cat)
      # function to find the percentage of the most common value in a vector
      imb_cols       <- do.call("rbind", lapply(df_cat, fast_table))
      imb_cols$names <- colnames(df_cat)
      # get top ten most imbalance by common class and pass to histogrammer
      out <- imb_cols %>% 
        arrange(desc(prop)) %>% 
        slice(1:min(top, nrow(.))) %>% 
        mutate(prop = 100 * prop) %>% 
        select(col_name = names, value, percent = prop)
      # print plot if requested
      if(show_plot){
        # convert col_name to factor
        out_plot <- out %>% mutate(col_name = factor(col_name, levels = as.character(col_name)))
        # construct bar plot of missingess
        plt <- bar_plot(df_plot = out_plot, x = "col_name", y = "percent", fill = "col_name", label = "count_na", 
                        ttl = paste0("Prevalance of missing values in df::", df_names$df1), 
                        sttl = paste0("df::", df_names$df1,  " has ", ncol(df1), " columns, of which ", sum(out_plot$count_na > 0), " have missing values"), 
                        ylb = "% of column that is NA", rotate = TRUE)
        # add text annotation to plot
        plt <- add_annotation_to_bars(x = out_plot$col_name, y = out_plot$percent, z = out_plot$count_na, plt = plt)
        print(plt)
      }
      
      # return dataframe of values
      return(out)
    } else {
      # return empty dataframe of 
      return(tibble(col_name = character(), value = character(), percent = numeric()))
    }
  } else {
    s1 <- report_imbalance(df1,  top = top, show_plot = F) %>% rename(value_1 = value, percent_1 = percent)
    s2 <- report_imbalance(df2,  top = top, show_plot = F) %>% rename(value_2 = value, percent_2 = percent)
    imbal_tab <- full_join(s1, s2, by = c("col_name")) %>%
      mutate(p_value = prop_test_imbalance(., n_1 = nrow(df1), n_2 = nrow(df2)))
    return(imbal_tab)
  }
}