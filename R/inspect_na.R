#' Summarise and compare the rate of missingness in one or two dataframes.
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for making columnwise comparison of missingness.  
#' Defaults to \code{NULL}.
#' @param show_plot Logical determining whether to return a plot in addition to tibble.  
#' Default is \code{FALSE}.
#' @param alpha Alpha level for displaying whether test of difference between the data
#' frames reaches a particular threshold.  Only applies when (when \code{show_plot = T}).  
#' Defaults to 0.05.
#' @param text_labels Whether to show text annotation on plots (when \code{show_plot = T}). 
#' Default is \code{TRUE}.
#' @return A tibble summarising the count and percentage of columnwise missingness 
#' for one or a pair of data frames.
#' @details When a single data frame is specified, the a tibble is returned which 
#' contains the count and percentage of missing values, with column names
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1}.
#'   \item \code{cnt} integer vector containing the number of missing values by 
#'   column.
#'   \item \code{pcnt} the percentage of each column with missing values
#' }
#' 
#' When both \code{df1} and \code{df2} are specified, missingness is compared across 
#' columns occurring in both data frames.  A test of the null hypothesis that the rate 
#' of missingness is the same across the same column in either dataframe.
#' \itemize{
#'   \item \code{col_name} the name of the columns occurring in either \code{df1}
#'   \item \code{cnt_1}, \code{cnt_2} pair of integer vectors containing counts of missing entries
#'   for each column in \code{df1} and \code{df2}.
#'   \item \code{pcnt_1}, \code{pcnt_2} pair of columns containing percentage of missing entries
#'   for each column in \code{df1} and \code{df2}.
#'   \item \code{p_value} p-value associated with test of the rates of missingness.  Small 
#'   values indicate evidence that the rate of missingness differs for a column occurring 
#'   in both \code{df1} and \code{df2}.
#' }
#' 
#' 
#' 
#' @examples
#' data("starwars", package = "dplyr")
#' # inspect missingness in starwars data
#' inspect_na(starwars)
#' # show the result as a barplot
#' inspect_na(starwars, show_plot = TRUE)
#' # compare two dataframes
#' inspect_na(starwars, starwars[1:30, ])
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

inspect_na <- function(df1, df2 = NULL, show_plot = FALSE, alpha = 0.05, 
                       text_labels = TRUE){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  # if ony one df input then inspect na content
  if(is.null(df2)){
    # calculate columnwise missingness
    na_vec <- vector("numeric", length = ncol(df1))
    pb <- progress_bar$new(
      format = paste0(" ", df_names[[1]], " [:bar] :percent eta: :eta"),
      total = ncol(df1), clear = TRUE, width = 80)
    for(i in 1:ncol(df1)){
      pb$tick()
      na_vec[i] <- sumna(df1[[i]])
    }
    names(na_vec) <- colnames(df1)
    out <- vec_to_tibble(na_vec) %>%
      mutate(pcnt = 100 * n / nrow(df1)) %>%
      select(col_name = names, cnt = n, pcnt) %>%
      arrange(desc(pcnt))
    # if any missing values then print out
    if(nrow(out) > 0){
      # print plot if requested
      if(show_plot){
        plot_na_1(out, 
                  df_names = df_names, 
                  text_labels = text_labels)
      }
      # return summary tibble
      return(out)
    } else {
      # return dataframe of values
      return(tibble(col_name = character(), cnt = integer(), pcnt = numeric()))
    }
    if(type == "console") invisible(df1)
  } else {
    s1 <- inspect_na(df1, show_plot = F) 
    s2 <- inspect_na(df2, show_plot = F)
    na_tab <- full_join(s1, s2, by = "col_name")
    na_tab$p_value <- prop_test(na_1 = na_tab$cnt.x, 
                                na_2 = na_tab$cnt.y, 
                                n_1 = nrow(df1), 
                                n_2 = nrow(df2))
    colnames(na_tab)[c(3, 5)] <- paste0("pcnt_", 1:2)
    colnames(na_tab)[c(2, 4)] <- paste0("cnt_", 1:2)
    # print a plot if requested
    if(show_plot){
      plot_na_2(na_tab, 
                df_names = df_names, 
                alpha = alpha, 
                text_labels = text_labels)
    }
    # return dataframe
    return(na_tab)
  }
}



