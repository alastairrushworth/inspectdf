#' Report the memory usage of a data frame or compare usage in two data frames.
#'
#' @param df1 A data frame.
#' @param df2 An optional second data frame for comparison.
#' @param top Positive integer specifying the number of rows to print in the 
#' output tibble.  Useful when the input data frame has many columns.  
#' \code{top = NULL}, the default causes everything to be returned.
#' @param show_plot Logical argument determining whether plot is generated 
#' in addition to tibble output.  Default is \code{FALSE}.  
#' @return A tibble summarising the memory usage of each column
#'  in one or two data frames.
#' @details When \code{df2 = NULL}, a tibble is returned with the columns: \code{col_name}
#' the columns in \code{df1}, \code{size} and \code{pcnt} contain the memory usage 
#' of each column in \code{df1}.  The tibble is sorted in descending order of \code{size}.
#' 
#' When a second data frame \code{df2} is specified, column sizes are 
#' tabulated for both data frames to enable comparison.  
#' A full join is performed between size summary table for the two data frames: where
#' a column exists in one but not the other, \code{size} and \code{pcnt} some 
#' cells return \code{NA}.
#' @examples
#' data("starwars", package = "dplyr")
#' # get tibble of column memory usage for the starwars data
#' report_mem(starwars)
#' # get column memory usage and show as barplot
#' report_mem(starwars, show_plot = TRUE)
#' # compare memory usage 
#' report_mem(starwars, starwars[1:10, -3])
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @export

report_mem <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame names
  df_names <- get_df_names()
  
  # max size of both dfs
  sizes <- list(sz_1  = size_up(df1, form = T), 
                sz_2  = size_up(df2, form = T), 
                ncl_1 = ncol(df1), ncl_2 = ncol(df2), 
                nrw_1 = ncol(df2), ncl_2 = ncol(df2))
  
  if(is.null(df2)){
    # get column size
    col_space     <- sapply(df1, size_up, form = F)
    col_space_ch  <- sapply(df1, size_up, form = T)
    col_max       <- which.max(col_space)
    col_max_size  <- col_space[col_max]
    col_max_names <- names(col_space)[col_max]
    
    # get ncols, nrows, and storage size of the data
    ncl <- format(ncol(df1), big.mark = ",")
    nrw <- format(nrow(df1), big.mark = ",")
  
    # get top 10 largest columns by storage size, pass to the console histogrammer
    out <- vec_to_tibble(col_space) %>% 
      left_join(vec_to_tibble(col_space_ch), by = "names") %>%
      mutate(pcnt = 100 * n.x / sum(n.x)) %>%
      arrange(desc(pcnt)) %>%
      slice(1:min(top, nrow(.))) %>%
      rename(col_name = names, size = n.y) %>% 
      select(-n.x)
    # return plot if requested
    if(show_plot) plot_mem_1(out, df_names = df_names, sizes = sizes)
    # return tibble
    return(out)
  } else {
    # get the space report for both input dfs
    df1 <- report_mem(df1, top = top, show_plot = F)
    df2 <- report_mem(df2, top = top, show_plot = F)
    sjoin <- full_join(df1, df2, by = "col_name") %>%
      select(col_name, contains("size"), contains("pcnt"))
    colnames(sjoin)[2] <- paste0("size_",  df_names$df1)
    colnames(sjoin)[3] <- paste0("size_",  df_names$df2)
    colnames(sjoin)[4] <- paste0("pcnt_",  df_names$df1)
    colnames(sjoin)[5] <- paste0("pcnt_",  df_names$df2)
    # if plot is requested
    if(show_plot) plot_mem_2(sjoin, df_names = df_names, sizes = sizes)
    # return dataframe
    return(sjoin)
  }
}
