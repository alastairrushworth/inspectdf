#' Report summary statistics for a data frame 
#'
#' @param df A data frame
#' @param top_n The number of rows to print for summaries \code{report_na}, \code{report_cor}, \code{report_imbalance}, \code{report_association}, Default \code{NULL} prints everything.
#' @return Prints statistics to the console.
#' @details \code{report} is a wrapper for the all of the individual reporting functions \code{report_space}, \code{report_types}, \code{report_na}, \code{report_cor}, \code{report_imbalance}, \code{report_association}.
#' @examples
#' report(mtcars)


report <- function(df, top_n = NULL){
  df %>%
    report_space(top_n = top_n) %>%
    report_types(type = "console") %>%
    report_na(top_n = top_n, type = "console") %>%
    report_cor(top_n = top_n, type = "console") %>% 
    report_imbalance(top_n = top_n, type = "console") %>%
    report_association(top_n = top_n, type = "console")
}