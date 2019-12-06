#' Summary and comparison of memory usage of dataframe columns 
#'
#' @description For a single dataframe, summarise the memory usage in each column. 
#' If two dataframes are supplied, compare memory usage for columns appearing 
#' in both dataframes.  For grouped dataframes, summarise the memory usage separately 
#' for each group.
#' 
#' @param df1 A data frame.
#' @param df2 An optional second data frame with which to comparing memory usage.  
#' Defaults to \code{NULL}.
#' @return A tibble summarising and comparing the columnwise memory usage 
#' for one or a pair of data frames.
#' @details 
#' For a \strong{single dataframe}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, a character vector containing column names of \code{df1}.
#'   \item \code{bytes}, integer vector containing the number of bytes in each column of \code{df1}.
#'   \item \code{size}, a character vector containing display-friendly memory usage of each column.
#'   \item \code{pcnt}, the percentage of the dataframe's total memory footprint 
#'   used by each column.
#' }
#' For a \strong{pair of dataframes}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, a character vector containing column names of \code{df1}
#'   and \code{df2}.
#'   \item \code{size_1}, \code{size_2}, a character vector containing memory usage of each column in
#'   each of \code{df1} and \code{df2}.
#'   \item \code{pcnt_1}, \code{pcnt_2}, the percentage of total memory usage of each column within 
#'   each of \code{df1} and \code{df2}.
#' }
#' For a \strong{grouped dataframe}, the tibble returned is as for a single dataframe, but where 
#' the first \code{k} columns are the grouping columns.  There will be as many rows in the result 
#' as there are unique combinations of the grouping variables.
#' 
#' @export
#' @author Alastair Rushworth
#' @seealso \code{\link{show_plot}}
#' 
#' @examples
#' # Load dplyr for starwars data & pipe
#' library(dplyr)
#' 
#' # Single dataframe summary
#' inspect_mem(starwars)
#' 
#' # Paired dataframe comparison
#' inspect_mem(starwars, starwars[1:20, ])
#' 
#' # Grouped dataframe summary
#' starwars %>% group_by(gender) %>% inspect_mem()
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

inspect_mem <- function(df1, df2 = NULL){
  
  # perform basic column check on dataframe input
  input_type <- check_df_cols(df1, df2)
  
  # capture the data frame names
  df_names <- get_df_names()
  
  # max size of both dfs
  sizes <- list(sz_1  = format_size(object.size(df1)), 
                sz_2  = format_size(object.size(df2)), 
                ncl_1 = ncol(df1), ncl_2 = ncol(df2), 
                nrw_1 = nrow(df1), nrw_2 = nrow(df2))
  
  # if only a single df input
  if(input_type == "single"){
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
      mutate(pcnt = 100 * n.x / sum(n.x), 
             bytes = as.integer(unlist(col_space))) %>%
      arrange(desc(pcnt)) %>%
      rename(col_name = names, size = n.y) %>% 
      select(col_name, bytes, size, pcnt)
  }
  if(input_type == "pair"){
    # get the space report for both input dfs
    df1 <- inspect_mem(df1)
    df2 <- inspect_mem(df2)
    out <- full_join(df1, df2, by = "col_name") %>%
      select(col_name, contains("size"), contains("pcnt"))
    colnames(out)[2:5] <- c("size_1", "size_2", "pcnt_1", "pcnt_2")
  }
  if(input_type == "grouped"){
    out <- apply_across_groups(df = df1, fn = inspect_mem)
  }
  attr(out, "type")     <- list(method = "mem", input_type = input_type)
  attr(out, "df_names") <- df_names
  attr(out, "sizes")    <- sizes
  return(out)
}
