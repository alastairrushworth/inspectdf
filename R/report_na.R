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
#' @details When a second data frame \code{df2} is specified, the tibble returned compares 
#' missing for both dataframes.  The \code{p_value} results from a a chi-square test 
#' of the null hypothesis that the rate of missingness is the same in in \code{df1} 
#' and \code{df2}.
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
      select(col_name = names, cnt_na = n, pcnt) %>%
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
      return(tibble(col_name = character(), cnt_na = integer(), pcnt = numeric()))
    }
    if(type == "console") invisible(df1)
  } else {
    s1 <- report_na(df1, top = top, show_plot = F) 
    s2 <- report_na(df2, top = top, show_plot = F)
    na_tab <- full_join(s1, s2, by = "col_name")
    na_tab$p_value <- prop_test(na_1 = na_tab$cnt_na.x, na_2 = na_tab$cnt_na.y, 
                                n_1 = nrow(df1), n_2 = nrow(df2))
    colnames(na_tab)[c(3, 5)] <- paste0("pcnt_", df_names)
    colnames(na_tab)[c(2, 4)] <- paste0("cnt_", df_names)
    # print a plot if requested
    if(show_plot) plot_na_2(na_tab, df_names = df_names, alpha = alpha)
    # return dataframe
    return(na_tab)
  }
}



