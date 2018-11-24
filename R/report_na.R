# find columns with missing values
report_na <- function(df, top_n = 10, type = "df"){
  # perform basic column check on dataframe input
  check_df_cols(df)

  # find the top 10 with most missingness
  df_summary <- vec_to_tibble(sapply(df, sumna)) %>%
    dplyr::mutate(prop = n / nrow(df)) %>%
    dplyr::filter(prop > 0) %>%  
    dplyr::arrange(desc(prop)) %>%
    dplyr::slice(1:top_n) 
  
  # if any missing values then print out
  if(nrow(df_summary) > 0){
    # print to console
    if(type == "console"){
      # print title text
      console_title("Columns sorted by % missing")
      # print console chart
      df_summary %>% dot_bars_na
      # invisibly return the df for further summaries
      invisible(df)
    }
    # print dfs
    if(type == "df"){
      # return dataframe of values
      return(df_summary)
    }
  } else {
    if(type == "console"){
      # print title text
      console_title("Columns sorted by % missing")
      # print console chart
      cat(silver("    << Not applicable >>\n"))
      # invisibly return the df for further summaries
      invisible(df)
    } 
    if(type == "df"){
      # return dataframe of values
      return(tibble(names = character(), n = integer(), value = numeric()))
    }
  }
}



