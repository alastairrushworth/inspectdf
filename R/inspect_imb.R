#' Summarise and compare columnwise imbalance for non-numeric columns in one or two dataframes.
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing columnwise imbalance.  
#' Defaults to \code{NULL}.  
#' @param show_plot Logical argument determining whether plot is returned
#' in addition to tibble output.  Default is \code{FALSE}.
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
#' # get most common levels and show as barplot
#' inspect_imb(starwars, show_plot = TRUE)
#' # compare memory usage 
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

inspect_imb <- function(df1, df2 = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
    # pick out categorical columns
    df_cat <- df1 %>% select_if(function(v) is.character(v) | is.factor(v))
    # calculate imbalance if any columns available
    if(ncol(df_cat) > 0){
      # function to find the percentage of the most common value in a vector
      freq_tabs      <- lapply(df_cat, fast_table, show_cnt = TRUE)
      imb_cols       <- suppressWarnings(bind_rows(freq_tabs, .id = "col_name"))

      out <- imb_cols %>% 
        group_by(col_name) %>%
        arrange(desc(prop)) %>% 
        slice(1) %>% ungroup %>%
        mutate(prop = 100 * prop) %>%
        arrange(desc(prop)) %>% 
        select(col_name, value, pcnt = prop, cnt)
      # print plot if requested
      if(show_plot) plot_imb_1(out, df_names = df_names)
      # return dataframe of values
      return(out)
    } else {
      # return empty dataframe if no categorical columns 
      return(tibble(col_name = character(), 
                    value = character(), 
                    pcnt = numeric(), 
                    cnt = integer()))
    }
  } else {
    # summary of df1
    s1 <- inspect_imb(df1, show_plot = F) %>% 
      rename(pcnt_1 = pcnt, cnt_1 = cnt)
    # summary of df2
    s2 <- inspect_imb(df2, show_plot = F) %>% 
      rename(pcnt_2 = pcnt, cnt_2 = cnt)
    # left join summaries together
    out <- left_join(s1, s2, by = c("col_name", "value")) %>%
      mutate(p_value = prop_test_imb(., n_1 = nrow(df1), n_2 = nrow(df2)))
    # print plot if requested
    if(show_plot) plot_imb_2(out, df_names = df_names, alpha = 0.05)
    # return combined data frame
    return(out)
  }
}