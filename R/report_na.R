#' Report the rate of missingness in each column of a dataframe
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for making comparisons of missingness across columns.  
#' Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} 
#' prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  
#' Default is \code{FALSE}.
#' @param alpha Alpha level for performing significance tests.  Defaults to 0.05.
#' @return Return a \code{tibble} summarising columnwise missingness by presence of 
#' \code{NA}.  Output contains the columns \code{col_name}, \code{cnt_na} and \code{pcnt}. 
#' @details When a single data frame is specified, the tibble returned contains the count
#' and percentage of missing values within in each column.  The tibble has columns:
#' \itemize{
#'   \item \code{col_name} the name of the columns in \code{df1}
#'   \item \code{cnt} the number of missing values in each column of \code{df1}
#'   \item \code{pcnt} the percentage of each column with missing values
#' }
#' 
#' When a second data frame \code{df2} is specified, the tibble returned compares 
#' missingness for both data frames, and performs a statistical test for the null
#' hypothesis that the rate of missingness is the same for the same named column
#' in both data frames.
#' \itemize{
#'    \item \code{col_name} the name of the columns occurring in either \code{df1}
#'   \item \code{cnt_...} pair of columns containing number of missing entries
#'   for the same column in \code{df1} and \code{df2} - the data frame name are appended.
#'   \item \code{pcnt_...} pair of columns containing number of missing entries
#'   for the same column in \code{df1} and \code{df2} - the data frame name are appended.
#' }
#' 
#' 
#' 
#' @examples
#' data("starwars", package = "dplyr")
#' # missingness in starwars data
#' report_na(starwars)
#' report_na(starwars, show_plot = TRUE)
#' # compare missingness 
#' report_na(starwars, starwars[1:30, ])
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

report_na <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE, alpha = 0.05){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  # if ony one df input then report na content
  if(is.null(df2)){
    # find the top 10 with most missingness
    out <- vec_to_tibble(sapply(df1, sumna)) %>%
      mutate(pcnt = 100 * n / nrow(df1)) %>%
      select(col_name = names, cnt = n, pcnt) %>%
      arrange(desc(pcnt)) %>%
      slice(1:min(top, nrow(.)))
    # if any missing values then print out
    if(nrow(out) > 0){
      # print plot if requested
      if(show_plot) plot_na_1(out, df_names = df_names)
      # return summary tibble
      return(out)
    } else {
      # return dataframe of values
      return(tibble(col_name = character(), cnt = integer(), pcnt = numeric()))
    }
    if(type == "console") invisible(df1)
  } else {
    s1 <- report_na(df1, top = top, show_plot = F) 
    s2 <- report_na(df2, top = top, show_plot = F)
    na_tab <- full_join(s1, s2, by = "col_name")
    na_tab$p_value <- prop_test(na_1 = na_tab$cnt.x, 
                                na_2 = na_tab$cnt.y, 
                                n_1 = nrow(df1), 
                                n_2 = nrow(df2))
    colnames(na_tab)[c(3, 5)] <- paste0("pcnt_", df_names)
    colnames(na_tab)[c(2, 4)] <- paste0("cnt_", df_names)
    # print a plot if requested
    if(show_plot) plot_na_2(na_tab, df_names = df_names, alpha = alpha)
    # return dataframe
    return(na_tab)
  }
}



