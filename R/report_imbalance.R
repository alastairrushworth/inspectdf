report_imbalance <- function(df, top_n = 10, include_numeric = F){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # print console title text
  console_title("Top most imbalanced features (exlc. numeric")
  
  # exclude columns containing lists (or numeric if not specified)
  col_exclude <- ifelse(include_numeric, 
                        function(v) !is.list(v), 
                        function(v) !(is.list(v) | is.numeric(v))) 
  gdf         <- df %>% 
    dplyr::select_if(col_exclude)
  if(ncol(gdf) > 0){
    cnames      <- colnames(gdf)
    # function to find the percentage of the most common value in a vector
    imb_cols       <- do.call("rbind", lapply(gdf, fast_table))
    imb_cols$names <- colnames(gdf)
    
    # get top ten most imbalance by common class and pass to histogrammer
    imb_cols %>% 
      dplyr::arrange(desc(prop)) %>% 
      dplyr::slice(1:top_n) %>% 
      dot_bars_imbalance
  } else {
    cat(silver("    << Not applicable >>\n"))
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}