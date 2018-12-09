#' Report the most commonly occuring value in each non-numeric column
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing feature imbalance with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{value_1}, \code{percent_1}, \code{value_2}, \code{percent_2}.  The \code{value} is the most frequently occuring category in each column and \code{percent_in_col} is the percentage frequency with which it occurs.
#' @examples
#' report_imbalance(starwars)

report_imbalance <- function(df1, df2 = NULL, top = NULL, show_plot = F){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
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
        dplyr::arrange(desc(prop)) %>% 
        dplyr::slice(1:min(top, nrow(.))) %>% 
        mutate(prop = 100 * prop) %>% 
        select(col_name = names, value, percent = prop)
      # return dataframe of values
      return(out)
    } else {
      # return empty dataframe of 
      return(tibble(col_name = character(), value = character(), percent = numeric()))
    }
  } else {
    s1 <- report_imbalance(df1,  top = top, show_plot = F) %>% dplyr::rename(value_1 = value, percent_1 = percent_in_col)
    s2 <- report_imbalance(df2, top = top, show_plot = F) %>% dplyr::rename(value_2 = value, percent_2 = percent_in_col)
    imbal_tab <- dplyr::full_join(s1, s2, by = "col_name") %>%
      mutate(p_value = prop_test_imbalance(., n_1 = nrow(df1), n_2 = nrow(df2)))
    return(imbal_tab)
  }
}