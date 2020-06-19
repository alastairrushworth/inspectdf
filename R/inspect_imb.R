#' Summary and comparison of the most common levels in categorical columns 
#' 
#' @description For a single dataframe, summarise the most common level in each 
#' categorical column. If two dataframes are supplied, compare the most common 
#' levels of categorical features appearing in both dataframes.  For grouped 
#' dataframes, summarise the levels of categorical columns in the dataframe
#' split by group.
#'
#' @param df1 A dataframe.
#' @param df2 An optional second data frame for comparing columnwise imbalance.  
#' Defaults to \code{NULL}.  
#' @param include_na Logical flag, whether to include missing values as a unique level.  Default
#' is \code{FALSE} - to ignore \code{NA} values.
#' @return  A tibble summarising and comparing the imbalance for each categorical column 
#' in one or a pair of dataframes.
#' 
#' @details 
#' For a \strong{single dataframe}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, a character vector containing column names of \code{df1}.
#'   \item \code{value}, a character vector containing the most common categorical level 
#'   in each column of \code{df1}.
#'   \item \code{pcnt}, the relative frequency of each column's most common categorical level 
#'   expressed as a percentage.
#'   \item \code{cnt}, the number of occurrences of the most common categorical level in each
#'   column of \code{df1}.
#' }
#' For a \strong{pair of dataframes}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, a character vector containing names of the unique columns in \code{df1} 
#'   and \code{df2}.
#'   \item \code{value}, a character vector containing the most common categorical level 
#'   in each column of \code{df1}.  
#'   \item \code{pcnt_1}, \code{pcnt_2}, the percentage occurrence of \code{value} in 
#'   the column \code{col_name} for each of \code{df1} and \code{df2}, respectively.
#'   \item \code{cnt_1}, \code{cnt_2}, the number of occurrences of of \code{value} in 
#'   the column \code{col_name} for each of \code{df1} and \code{df2}, respectively.
#'   \item \code{p_value}, p-value associated with the null hypothesis that the true rate of 
#'   occurrence is the same for both dataframes.  Small values indicate stronger evidence of a difference
#'   in the rate of occurrence.
#' }
#' For a \strong{grouped dataframe}, the tibble returned is as for a single dataframe, but where 
#' the first \code{k} columns are the grouping columns.  There will be as many rows in the result 
#' as there are unique combinations of the grouping variables.
#' 
#' @author Alastair Rushworth
#' @seealso \code{\link{inspect_cat}}, \code{\link{show_plot}}
#' @export
#' @examples
#' # Load dplyr for starwars data & pipe
#' library(dplyr)
#' 
#' # Single dataframe summary
#' inspect_imb(starwars)
#' 
#' # Paired dataframe comparison
#' inspect_imb(starwars, starwars[1:20, ])
#' 
#' # Grouped dataframe summary
#' starwars %>% group_by(gender) %>% inspect_imb()
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom magrittr %>%

inspect_imb <- function(df1, df2 = NULL, include_na = FALSE){
  
  # perform basic column check on dataframe input
  input_type <- check_df_cols(df1, df2)
  # capture the data frame names
  df_names <- get_df_names()
  if(input_type == "single"){
    # pick out categorical columns
    df_cat <- df1 %>% 
      select_if(function(v) is.character(v) | is.factor(v) | is.logical(v))
    n_cols <- ncol(df_cat)
    # calculate imbalance if any columns available
    if(n_cols > 0){
      # check any factors for duplicate labels
      df_cat    <- check_factors(df_cat)
      names_cat <- colnames(df_cat)
      # function to find the percentage of the most common value in a vector
      levels_list <- vector("list", length = n_cols)
      pb <- start_progress(prefix = " Column", total = n_cols)
      for(i in 1:n_cols){
        update_progress(bar = pb, iter = i, total = n_cols, what = names_cat[i])
        full_tab  <- fast_table(df_cat[[i]], show_cnt = TRUE, show_na = include_na)
        first_row <- full_tab %>% slice(1)
        if(nrow(first_row) == 0){
          first_row <- tibble(value = "empty", prop = 0, cnt = as.integer(0))
        }
        levels_list[[i]] <- first_row
      }
      # collapse highest imbalance into single dataframe
      names(levels_list) <- names_cat
      imb_cols  <- suppressWarnings(bind_rows(levels_list, .id = "col_name"))
      
      # tidy up table output
      out <- imb_cols %>% 
        mutate(prop = 100 * prop) %>%
        arrange(desc(prop)) %>% 
        select(col_name, value, pcnt = prop, cnt)
    } else {
      # return empty dataframe if no categorical columns 
      out <- tibble(col_name = character(), 
                    value = character(), 
                    pcnt = numeric(), 
                    cnt = integer())
    }
  }
  if(input_type == "pair"){
    # summary of df1
    s1 <- inspect_imb(df1, include_na = include_na) %>% 
      rename(pcnt_1 = pcnt, cnt_1 = cnt)
    # summary of df2
    s2 <- inspect_imb(df2, include_na = include_na) %>% 
      rename(pcnt_2 = pcnt, cnt_2 = cnt)
    # left join summaries together
    out <- left_join(s1, s2, by = c("col_name", "value")) %>%
      mutate(p_value = prop_test_imb(., n_1 = nrow(df1), n_2 = nrow(df2)))
  } 
  if(input_type == "grouped"){
    out <- apply_across_groups(df = df1, fn = inspect_imb)
  }
  # attach attributes required for plotting
  attr(out, "type")     <- list(method = "imb", input_type = input_type)
  attr(out, "df_names") <- df_names
  return(out)
}