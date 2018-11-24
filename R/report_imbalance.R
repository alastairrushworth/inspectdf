report_imbalance <- function(df, top_n = 10, type = "df", include_numeric = F){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # exclude columns containing lists (or numeric if not specified)
  col_exclude <- ifelse(include_numeric, 
                        function(v) !is.list(v), 
                        function(v) !(is.list(v) | is.numeric(v))) 
  gdf         <- df %>% dplyr::select_if(col_exclude)
  
  # calculate imbalance if any columns available
  if(ncol(gdf) > 0){
    cnames      <- colnames(gdf)
    # function to find the percentage of the most common value in a vector
    imb_cols       <- do.call("rbind", lapply(gdf, fast_table))
    imb_cols$names <- colnames(gdf)
    # get top ten most imbalance by common class and pass to histogrammer
    out <- imb_cols %>% dplyr::arrange(desc(prop)) %>% dplyr::slice(1:top_n)

    if(type == "console"){
      # print console title text
      console_title("Top most imbalanced features (exlc. numeric")
      # print console chart
      out %>% dot_bars_imbalance
      # invisibly return the df for further summaries
      invisible(df)
    }
    if(type == "df"){
      # return dataframe of values
      return(out %>% select(names, value, prop))
    }
  } else {
      if(type == "console"){
        # print console title text
        console_title("Top most imbalanced features (exlc. numeric")
        # print console chart
        cat(silver("    << Not applicable >>\n"))
        # invisibly return the df for further summaries
        invisible(df)
      } 
    if(type == "df"){
      # return empty dataframe of 
      return(tibble(names = character(), value = character(), value = numeric()))
    }
  }
}