#' Report summary statistics for a data frame 
#'
#' @param df A data frame
#' @param top The number of rows to print for summaries \code{report_na}, \code{report_cor}, \code{report_imbalance}, \code{report_association}, Default \code{NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Prints statistics to the console.
#' @details \code{report} is a wrapper for the all of the individual reporting functions \code{report_space}, \code{report_types}, \code{report_na}, \code{report_cor}, \code{report_imbalance}, \code{report_association}.
#' @examples
#' report(starwars)


report <- function(df1, df2 = NULL, top = NULL, show_plot = F){
  report_space(df1, df2, top = top)
  list(
    report_types(df1, df2, show_plot = F),
    report_na(df1, df2, top = top, show_plot = F),
    report_cor(df1, df2, top = top, show_plot = F),
    report_imbalance(df1, df2, top = top, show_plot = F),
    report_association(df1, df2, top = top, show_plot = F), 
    report_numeric(df1, df2, top = top, show_plot = F)
  ) %>% return
}