colnames_nice <- function(df, minlength = 10){
  # extract the column names
  cname <- colnames(df)
  nch   <- nchar(cname)
  
  # drop case, remove punctuation
  cname %<>% 
    tolower %>%
    gsub(pattern = "[.]| ", replacement = "_") %>%
    abbreviate(minlength = minlength)
  
  # replace old names with new ones
  colnames(df) <- cname 
  
  # invisibly return the data.frame
  invisible(df)
}