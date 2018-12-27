#' Report the memory usage of a data frame
#'
#' @param df1 A data frame
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @return Prints the proportion of overall memory used by each column and the total usage.
#' @examples
#' report_space(starwars)
#' 
report_space <- function(df1, df2 = NULL, top = NULL, show_plot = F){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  if(is.null(df2)){
    # sizing function
    size_up <- function(thing, form = F){x <- object.size(thing); if(form){return(format(x, standard = "auto", unit = "auto", digits = 2L))} else {return(x)}}

    # get column size
    col_space     <- sapply(df1, size_up, form = F)
    col_space_ch  <- sapply(df1, size_up, form = T)
    col_max       <- which.max(col_space)
    col_max_size  <- col_space[col_max]
    col_max_names <- names(col_space)[col_max]
    
    # get ncols, nrows, and storage size of the data
    ncl <- format(ncol(df1), big.mark = ",")
    nrw <- format(nrow(df1), big.mark = ",")
    sz  <- size_up(df1, form = T)
  
    # get top 10 largest columns by storage size, pass to the console histogrammer
    vec_to_tibble(col_space) %>% 
      dplyr::left_join(vec_to_tibble(col_space_ch), by = "names") %>%
      dplyr::mutate(percent_space = 100 * n.x / sum(n.x)) %>%
      dplyr::arrange(desc(percent_space)) %>%
      dplyr::slice(1:min(top, nrow(.))) %>%
      dplyr::rename(col_names = names, size = n.y) %>% 
      dplyr::select(-n.x)
  } else {
    df1 <- report_space(df1, top = top, show_plot = F)
    df2 <- report_space(df2, top = top, show_plot = F)
    full_join(df1, df2, by = "col_names") %>%
      select(col_names, contains("size"), contains("percent"))
  }
}
