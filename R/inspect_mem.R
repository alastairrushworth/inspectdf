#' Summarise and compare the memory usage in one or two dataframes.
#'
#' @param df1 A data frame.
#' @param df2 An optional second data frame for comparing column sizes.  
#' Defaults to \code{NULL}.
#' @param show_plot (Deprecated) Logical flag indicating whether a plot should be shown.  
#' Superseded by the function \code{show_plot()} and will be dropped in a future version.
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
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @export

inspect_mem <- function(df1, df2 = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame names
  df_names <- get_df_names()
  
  # max size of both dfs
  sizes <- list(sz_1  = format_size(object.size(df1)), 
                sz_2  = format_size(object.size(df2)), 
                ncl_1 = ncol(df1), ncl_2 = ncol(df2), 
                nrw_1 = nrow(df1), nrw_2 = nrow(df2))
  
  if(is.null(df2)){
    # col_space vectors
    names_vec <- colnames(df1)
    col_space <- vector("list", length = sizes$ncl_1)
    col_space_ch <- vector("character", length = sizes$ncl_1)
    # initialise progress bar
    pb <- start_progress(prefix = " Column", total = sizes$ncl_1)
    # get column sizes in both character and numeric formats
    for(i in 1:sizes$ncl_1){
      update_progress(bar = pb, iter = i, total = sizes$ncl_1, what = names_vec[i])
      col_space[[i]] <- object.size(df1[[i]])
      col_space_ch[i] <- format_size(col_space[[i]])
    }
    col_space <- unlist(col_space)
    names(col_space) <- names(col_space_ch) <- colnames(df1)
    col_max       <- which.max(col_space)
    col_max_size  <- col_space[col_max]
    col_max_names <- names(col_space)[col_max]
    
    out <- vec_to_tibble(col_space) %>% 
      left_join(vec_to_tibble(col_space_ch), by = "names") %>%
      mutate(pcnt = 100 * n.x / sum(n.x)) %>%
      arrange(desc(pcnt)) %>%
      rename(col_name = names, size = n.y) %>% 
      select(-n.x)
    
    # attach attributes required for plotting
    attr(out, "type") <- list("mem", 1)
    attr(out, "df_names") <- df_names
    attr(out, "sizes") <- sizes
  } else {
    # get the space report for both input dfs
    df1 <- inspect_mem(df1)
    df2 <- inspect_mem(df2)
    out <- full_join(df1, df2, by = "col_name") %>%
      select(col_name, contains("size"), contains("pcnt"))
    colnames(out)[2:5] <- c("size_1", "size_2", "pcnt_1", "pcnt_2")
    
    # attach attributes required for plotting
    attr(out, "type") <- list("mem", 2)
    attr(out, "df_names") <- df_names
    attr(out, "sizes") <- sizes
  }
  if(show_plot) plot_deprecated(out)
  return(out)
}
