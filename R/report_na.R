#' Report the proportion of each column containing missing values
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing missing values with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{count_na} and \code{percent_na}. 
#' @details When the second data frame \code{df2} is specified, the missingness is tabulated for both data frames, and where a pair of columns are common to both data frames a p-value is calculated for the equivalence of the proportion of missing values.
#' @examples
#' report_na(starwars)

report_na <- function(df1, df2 = NULL, top = NULL, show_plot = F){
  # perform basic column check on dataframe input
  check_df_cols(df1)

  if(is.null(df2)){
    # find the top 10 with most missingness
    df_summary <- vec_to_tibble(sapply(df1, sumna)) %>%
      dplyr::mutate(prop = 100 * n / nrow(df1)) %>%
      # dplyr::filter(prop > 0) %>%  
      dplyr::arrange(desc(prop)) %>%
      dplyr::slice(1:min(top, nrow(.))) 
    # if any missing values then print out
    if(nrow(df_summary) > 0){
      # return dataframe of values
      colnames(df_summary) <- c("col_name", "count_na", "percent")
      return(df_summary)
    } else {
      # return dataframe of values
      return(tibble(col_name = character(), count_na = integer(), percent = numeric()))
    }
    if(type == "console") invisible(df1)
  } else {
    s1 <- report_na(df1,  top = top, show_plot = F) %>% dplyr::rename(count_na_1 = count_na, percent_1 = percent)
    s2 <- report_na(df2, top = top, show_plot = F) %>% dplyr::rename(count_na_2 = count_na, percent_2 = percent)
    na_tab <- dplyr::full_join(s1, s2, by = "col_name")
    na_tab$p_value <- prop_test(na_1 = na_tab$count_na_1, na_2 = na_tab$count_na_2, n_1 = nrow(df1), n_2 = nrow(df2))
    return(na_tab)
  }
}



