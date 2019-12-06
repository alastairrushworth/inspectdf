#' Summary and comparison of the rate of missingness across dataframe columns
#'
#' @description For a single dataframe, summarise the rate of missingness in each 
#' column.  If two dataframes are supplied, compare missingness for columns appearing 
#' in both dataframes.  For grouped dataframes, summarise the rate of missingness
#' separately for each group.
#' 
#' @param df1 A data frame
#' @param df2 An optional second data frame for making columnwise comparison of missingness.  
#' Defaults to \code{NULL}.
#' @return A tibble summarising the count and percentage of columnwise missingness 
#' for one or a pair of data frames.
#' @details 
#' For a \strong{single dataframe}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, a character vector containing column names of \code{df1}.
#'   \item \code{cnt}, an integer vector containing the number of missing values by 
#'   column.
#'   \item \code{pcnt}, the percentage of records in each columns that is missing.
#' }
#' For a \strong{pair of dataframes}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, the name of the columns occurring in either \code{df1} or \code{df2}.
#'   \item \code{cnt_1}, \code{cnt_2}, a pair of integer vectors containing counts of missing entries
#'   for each column in \code{df1} and \code{df2}.
#'   \item \code{pcnt_1}, \code{pcnt_2}, a pair of columns containing percentage of missing entries
#'   for each column in \code{df1} and \code{df2}.
#'   \item \code{p_value}, the p-value associated with test of equivalence of rates of missingness.  Small 
#'   values indicate evidence that the rate of missingness differs for a column occurring 
#'   in both \code{df1} and \code{df2}.
#' }
#' For a \strong{grouped dataframe}, the tibble returned is as for a single dataframe, but where 
#' the first \code{k} columns are the grouping columns.  There will be as many rows in the result 
#' as there are unique combinations of the grouping variables.
#' 
#' @author Alastair Rushworth
#' @seealso \code{\link{show_plot}}
#' 
#' @examples
#' # Load dplyr for starwars data & pipe
#' library(dplyr)
#' 
#' # Single dataframe summary
#' inspect_na(starwars)
#' 
#' # Paired dataframe comparison
#' inspect_na(starwars, starwars[1:20, ])
#' 
#' # Grouped dataframe summary
#' starwars %>% group_by(gender) %>% inspect_na()
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr slice
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @export

inspect_na <- function(df1, df2 = NULL){
  
  # perform basic column check on dataframe input
  input_type <- check_df_cols(df1, df2)
  # capture the data frame names
  df_names <- get_df_names()
  # if ony one df input then inspect na content
  if(input_type == "single"){
      n_cols <- ncol(df1)
      names_vec <- colnames(df1)
      # calculate columnwise missingness
      na_vec <- vector("numeric", length = n_cols)
      pb <- start_progress(prefix = " Column", total = n_cols)
      for(i in 1:n_cols){
        update_progress(bar = pb, iter = i, total = n_cols, what = names_vec[i])
        na_vec[i] <- sumna(df1[[i]])
      }
      names(na_vec) <- names_vec
      out <- vec_to_tibble(na_vec) %>%
        mutate(pcnt = 100 * n / nrow(df1), n = as.integer(n)) %>%
        select(col_name = names, cnt = n, pcnt) %>%
        arrange(desc(pcnt))
  }
  if(input_type == "pair"){
    s1 <- inspect_na(df1) 
    s2 <- inspect_na(df2)
    out <- full_join(s1, s2, by = "col_name")
    out$p_value <- prop_test(na_1 = out$cnt.x, 
                             na_2 = out$cnt.y, 
                             n_1 = nrow(df1), 
                             n_2 = nrow(df2))
    colnames(out)[2:5] <- c("cnt_1", "pcnt_1", "cnt_2", "pcnt_2")
  }
  if(input_type == "grouped"){
    out <- apply_across_groups(df = df1, fn = inspect_na)
  }
  attr(out, "type")     <- list(method = "na", input_type = input_type)
  attr(out, "df_names") <- df_names
  return(out)
}
