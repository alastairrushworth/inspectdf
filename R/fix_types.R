fix_types <- function(df){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # function to check coercibility to character
  to_numeric <- function(v){
    diff_na <- try(sum(is.na(as.numeric(v))) - sum(is.na(v)), silent = T)
    if(diff_na > 4) v else as.numeric(v)
  }

  # convert character columns that are coercible to numeric
  df %<>% mutate_if(is.character, to_numeric)
  
  # invisibly return the df for further summaries
  invisible(df)
}