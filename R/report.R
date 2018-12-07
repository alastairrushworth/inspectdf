#' Report summary statistics for a data frame 
#'
#' @param df A data frame
#' @param top The number of rows to print for summaries \code{report_na}, \code{report_cor}, \code{report_imbalance}, \code{report_association}, Default \code{NULL} prints everything.
#' @return Prints statistics to the console.
#' @details \code{report} is a wrapper for the all of the individual reporting functions \code{report_space}, \code{report_types}, \code{report_na}, \code{report_cor}, \code{report_imbalance}, \code{report_association}.
#' @examples
#' report(starwars)


report <- function(df, top = NULL){
  df %>%
    report_space(top = top) %>%
    report_types(type = "console") %>%
    report_na(top = top, type = "console") %>%
    report_cor(top = top, type = "console") %>% 
    report_imbalance(top = top, type = "console") %>%
    report_association(top = top, type = "console")
}