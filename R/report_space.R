# show how much space the columns and data take up
report_space <- function(df, top_n = 10){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # input object name
  # data_name     <- deparse(substitute(df))
  
  # get column size
  col_space     <- sapply(df, pryr::object_size)
  col_max       <- which.max(col_space)
  col_max_size  <- col_space[col_max]
  col_max_names <- names(col_space)[col_max]
  
  # # get column sparsity of numeric features
  # val_numeric   <- df %>% select_if(is.numeric) %>% unlist %>% as.numeric
  # val_numeric[is.na(val_numeric)] <- 1
  # prop_sparse   <- round(mean(val_numeric == 0) * 100, 1)
  
  # get ncols, nrows, and storage size of the data
  ncl <- format(ncol(df), big.mark = ",")
  nrw <- format(nrow(df), big.mark = ",")
  sz  <- format(object.size(df), standard = "auto", unit = "auto", digits = 2L)
  
  # title text
  title_text <- c(paste("Data has ", ncl, " cols and ", nrw, " rows, occupying ", sz, sep = ""),
                  "Top columns in order of memory") %>% console_title

  # get top 10 largest columns by storage size, pass to the console histogrammer
  vec_to_tibble(col_space) %>% 
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1:top_n) %>%
    dot_bars_space
  
  # invisibly return the df for further summaries
  invisible(df)
}