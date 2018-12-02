#' Report the memory usage of a data frame
#'
#' @param df A data frame
#' @param top_n The number of rows to print for summaries. Default \code{top_n = NULL} prints everything.
#' @return Prints the proportion of overall memory used by each column and the total usage.
#' @examples
#' report_space(mtcars)
#' 
report_space <- function(df, top_n = NULL){
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # get column size
  col_space     <- sapply(df, pryr::object_size)
  col_max       <- which.max(col_space)
  col_max_size  <- col_space[col_max]
  col_max_names <- names(col_space)[col_max]
  
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
    dplyr::slice(1:min(top_n, nrow(.))) %>%
    dot_bars_space
  
  # invisibly return the df for further summaries
  invisible(df)
}