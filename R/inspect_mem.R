#' Summarise and compare the memory usage in one or two dataframes.
#'
#' @param df1 A data frame.
#' @param df2 An optional second data frame for comparing column sizes.  
#' Defaults to \code{NULL}.
#' @param show_plot Logical argument determining whether plot is returned
#' in addition to tibble output.  Default is \code{FALSE}.  
#' @return A tibble summarising and comparing the columnwise memory usage 
#' for one or a pair of data frames.
#' @details When a single data frame is specified, a tibble is returned which 
#' contains columnwise memory usage in descending order of size:
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1}.
#'   \item \code{size} character vector containing memory usage of each column.
#'   \item \code{pcnt} the percentage of total memory usage used by each column.
#' }
#' When both \code{df1} and \code{df2} are specified, column sizes are jointly 
#' tabulated for both data frames, by performing a full join by \code{col_name}.  
#' Rows are sorted in descending order of size as they appear in \code{df1}:
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1}
#'   and \code{df2}.
#'   \item \code{size_1}, \code{size_2} character vector containing memory usage of each column in
#'   each of \code{df1} and \code{df2}.
#'   \item \code{pcnt_1}, \code{pcnt_2} the percentage of total memory usage of each column within 
#'   each of \code{df1} and \code{df2}.
#' }
#' @examples
#' data("starwars", package = "dplyr")
#' # get tibble of column memory usage for the starwars data
#' inspect_mem(starwars)
#' # get column memory usage and show as barplot
#' inspect_mem(starwars, show_plot = TRUE)
#' # compare memory usage 
#' inspect_mem(starwars, starwars[1:10, -3])
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

inspect_mem <- function(df1, df2 = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame names
  df_names <- get_df_names()
  
  # max size of both dfs
  sizes <- list(sz_1  = size_up(df1, form = T), 
                sz_2  = size_up(df2, form = T), 
                ncl_1 = ncol(df1), ncl_2 = ncol(df2), 
                nrw_1 = nrow(df1), nrw_2 = nrow(df2))
  
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
  
    out <- vec_to_tibble(col_space) %>% 
      left_join(vec_to_tibble(col_space_ch), by = "names") %>%
      mutate(pcnt = 100 * n.x / sum(n.x)) %>%
      arrange(desc(pcnt)) %>%
      rename(col_name = names, size = n.y) %>% 
      select(-n.x)
    # return plot if requested
    if(show_plot) plot_mem_1(out, df_names = df_names, sizes = sizes)
    # return tibble
    return(out)
  } else {
    # get the space report for both input dfs
    df1 <- inspect_mem(df1, show_plot = F)
    df2 <- inspect_mem(df2, show_plot = F)
    sjoin <- full_join(df1, df2, by = "col_name") %>%
      select(col_name, contains("size"), contains("pcnt"))
    colnames(sjoin)[2:3] <- paste0("size_",  1:2)
    colnames(sjoin)[4:5] <- paste0("pcnt_",  1:2)
    # if plot is requested
    if(show_plot) plot_mem_2(sjoin, df_names = df_names, sizes = sizes)
    # return dataframe
    return(sjoin)
  }
}
