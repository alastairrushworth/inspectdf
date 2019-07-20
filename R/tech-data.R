#' Tech stocks closing prices
#'
#' Daily closing stock prices of the three tech companies Microsoft, 
#' Apple and IBM between 2007 and 2019.
#'
#' @docType data
#'
#' @usage data(tech)
#'
#' @format A \code{dataframe} with 3158 rows and 6 columns.
#'
#' @keywords datasets
#'
#' @source Data gethered using the \href{https://github.com/joshuaulrich/quantmod}{quantmod}
#' package.
#'
#' @examples
#' data(tech)
#' # change in correlation over time
#' tech %>%
#'   group_by(year) %>%
#'   inspect_cor() %>%
#'   show_plot()