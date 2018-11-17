# types of the columns
report_types <- function(df, top_n = 10){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # number of columns
  ncl         <- ncol(df)
  # possible types to look out for
  type_spine  <- tibble(type = c("logical", "integer", "numeric", "character", 
                                 "factor",  "list",    "matrix",  "data.frame", 
                                 "ordered factor"))
  classes     <- sapply(df, class)
  classes     <- sapply(classes, paste, collapse = " ")
  types       <- table(classes)
  type_tibble <- tibble(type = names(types), n = as.integer(types))
  console_title(paste(ncl, " columns composed of types:", sep = ""))
  left_join(type_spine, type_tibble, by = "type") %>%
    replace_na(list(n = 0))     %>%
    mutate(prop = n / ncol(df)) %>% 
    arrange(desc(prop))         %>% 
    filter(prop > 0)            %>%
    dot_bars_composition 
  
  # invisibly return the df for further summaries
  invisible(df)
}






