#' Report the proportion of each column containing missing values
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing missing values with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{count_na} and \code{percent_na}. 
#' @export
#' @details When the second data frame \code{df2} is specified, the missingness is tabulated for both data frames, and where a pair of columns are common to both data frames a p-value is calculated for the equivalence of the proportion of missing values.
#' @examples
#' data("starwars", package = "dplyr")
#' report_na(starwars)
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom magrittr %>%
#' @importFrom tibble tibble

report_na <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  # if ony one df input then report na content
  if(is.null(df2)){
    # find the top 10 with most missingness
    out <- vec_to_tibble(sapply(df1, sumna)) %>%
      mutate(percent = 100 * n / nrow(df1)) %>%
      select(col_name = names, count_na = n, percent) %>%
      arrange(desc(percent)) %>%
      slice(1:min(top, nrow(.)))
    # if any missing values then print out
    if(nrow(out) > 0){
      # print plot if requested
      if(show_plot){
        # convert col_name to factor
        out_plot <- out %>% mutate(col_name = factor(col_name, levels = as.character(col_name)))
        # construct bar plot of missingess
        plt <- bar_plot(df_plot = out_plot, x = "col_name", y = "percent", fill = "col_name", label = "count_na",
                        ttl = paste0("Prevalance of missing values in df::", df_names$df1),
                        sttl = paste0("df::", df_names$df1,  " has ", ncol(df1), " columns, of which ", sum(out_plot$count_na > 0), " have missing values"),
                        ylb = "% of column that is NA", rotate = TRUE)
        # add text annotation to plot
        plt <- add_annotation_to_bars(x = out_plot$col_name, y = out_plot$percent, z = out_plot$count_na, plt = plt)
        print(plt)
      }
      # return summary tibble
      return(out)
    } else {
      # return dataframe of values
      return(tibble(col_name = character(), count_na = integer(), percent = numeric()))
    }
    if(type == "console") invisible(df1)
  } else {
    s1 <- report_na(df1, top = top, show_plot = F) %>% rename(count_na_1 = count_na, percent_1 = percent)
    s2 <- report_na(df2, top = top, show_plot = F) %>% rename(count_na_2 = count_na, percent_2 = percent)
    na_tab <- full_join(s1, s2, by = "col_name")
    na_tab$p_value <- prop_test(na_1 = na_tab$count_na_1, na_2 = na_tab$count_na_2, n_1 = nrow(df1), n_2 = nrow(df2))
    return(na_tab)
  }
}



