#' Report the proportion of each column containing missing values
#'
#' @param df A data frame
#' @param top_n The number of rows to print for summaries. Default \code{top_n = NULL} prints everything.
#' @param type Character specificying report output type.  Default \code{type = "df"} causes report to be returned as a tibble.   \code{type = "console"} causes report to be returned directly to the console.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{count_na} and \code{percent_na}. 
#' @examples
#' report_na(starwars)

report_na <- function(df, top_n = NULL, type = "df"){
  # perform basic column check on dataframe input
  check_df_cols(df)

  # find the top 10 with most missingness
  df_summary <- vec_to_tibble(sapply(df, sumna)) %>%
    dplyr::mutate(prop = n / nrow(df)) %>%
    # dplyr::filter(prop > 0) %>%  
    dplyr::arrange(desc(prop)) %>%
    dplyr::slice(1:min(top_n, nrow(.))) 
  
  # if any missing values then print out
  if(nrow(df_summary) > 0){
    # print to console
    if(type == "console"){
      # print title text
      console_title("Columns sorted by % missing")
      # print console chart
      df_summary %>% dot_bars_na
    }
    # print dfs
    if(type == "df"){
      # return dataframe of values
      colnames(df_summary) <- c("col_name", "count_na", "percent_na")
      return(df_summary)
    }
  } else {
    if(type == "console"){
      # print title text
      console_title("Columns sorted by % missing")
      # print console chart
      cat(silver("    << Not applicable >>\n"))
    } 
    if(type == "df"){
      # return dataframe of values
      return(tibble(col_name = character(), count_na = integer(), percent_na = numeric()))
    }
  }
  if(type == "console") invisible(df)
}



