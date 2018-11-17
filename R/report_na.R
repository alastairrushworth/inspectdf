# find columns with missing values
report_na <- function(df, top_n = 10){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # print title text
  console_title("Columns sorted by % missing")
  
  # find the top 10 with most missingness
  df_summary <- vec_to_tibble(sapply(df, sumna)) %>%
    dplyr::mutate(prop = n / nrow(df)) %>%
    dplyr::filter(prop > 0) %>%  
    dplyr::arrange(desc(prop)) %>%
    dplyr::slice(1:top_n) 
  
  # if there is anything to print, else sensible message
  if(nrow(df_summary) > 0){
    df_summary %>% dot_bars_na
  } else {
    cat(silver("    << Not applicable >>\n"))
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}



