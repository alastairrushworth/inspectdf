#' Summarise and compare columnwise imbalance for non-numeric columns in one or two dataframes.
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing columnwise imbalance.  
#' Defaults to \code{NULL}.  
#' @param include_na Logical flag, whether to include missing values as a unique level.  Default
#' is \code{FALSE} - to ignore NAs.
#' @param show_plot (Deprecated) Logical flag indicating whether a plot should be shown.  
#' Superseded by the function \code{show_plot()} and will be dropped in a future version.
#' @return  A tibble summarising and comparing the imbalance for each non-numeric column 
#' in one or a pair of data frames.
#' @details When a single data frame is specified, a tibble is returned which 
#' contains columnwise imbalance, with columns
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1}.
#'   \item \code{value} character vector containing the most common categorical level 
#'   in each column of \code{df1}.
#'   \item \code{pcnt} the percentage of each column's entries occupied by the level in
#'   \code{value} column.
#'   \item \code{cnt} the number of occurrences of the most common categorical level in each
#'   column of \code{df1}.
#' }
#' When both \code{df1} and \code{df2} are specified, the most common levels in \code{df1} 
#' are compared to columns in \code{df2}.  If a categorical column appears in
#' both dataframes, a simple test is performed to test the null hypothesis that the rate of 
#' occurrence of the common level in \code{df1} is the same in both dataframes.  
#' The resulting tibble has columns
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1} and 
#'   \code{df2}.
#'   \item \code{value} character vector containing the most common categorical level 
#'   in each column of \code{df1}.  
#'   \item \code{pcnt_} the percentage of each column's entries occupied by the level in
#'   \code{value} column.
#'   \item \code{cnt_} the number of occurrences of the most common categorical level in each
#'   column of \code{df1} and \code{df2}.
#' }
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' # get tibble of most common levels
#' inspect_imb(starwars)
#' # compare imbalance 
#' inspect_imb(starwars, starwars[1:10, -3])
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

inspect_imb <- function(df1, df2 = NULL, show_plot = FALSE, include_na = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
    # pick out categorical columns
    df_cat <- df1 %>% 
      select_if(function(v) is.character(v) | is.factor(v) | is.logical(v))
    n_cols <- ncol(df_cat)
    # calculate imbalance if any columns available
    if(n_cols > 0){
      # check any factors for duplicate labels
      df_cat <- check_factors(df_cat)
      names_cat <- colnames(df_cat)
      # function to find the percentage of the most common value in a vector
      levels_list <- vector("list", length = n_cols)
      pb <- start_progress(prefix = " Column", total = n_cols)
      for(i in 1:n_cols){
        update_progress(bar = pb, iter = i, total = n_cols, what = names_cat[i])
        full_tab <- fast_table(df_cat[[i]], show_cnt = TRUE, show_na = include_na)
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

      # attach attributes required for plotting
      attr(out, "type")     <- list(method = "imb", 1)
      attr(out, "df_names") <- df_names
    } else {
      # return empty dataframe if no categorical columns 
      out <- tibble(col_name = character(), 
                    value = character(), 
                    pcnt = numeric(), 
                    cnt = integer())
    }
  } else {
    # summary of df1
    s1 <- inspect_imb(df1, include_na = include_na) %>% 
      rename(pcnt_1 = pcnt, cnt_1 = cnt)
    # summary of df2
    s2 <- inspect_imb(df2, include_na = include_na) %>% 
      rename(pcnt_2 = pcnt, cnt_2 = cnt)
    # left join summaries together
    out <- left_join(s1, s2, by = c("col_name", "value")) %>%
      mutate(p_value = prop_test_imb(., n_1 = nrow(df1), n_2 = nrow(df2)))
    # attach attributes required for plotting
    attr(out, "type")     <- list(method = "imb", 2)
    attr(out, "df_names") <- df_names
  }
  if(show_plot) plot_deprecated(out)
  return(out)
}