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
#' @source Data gathered using the \href{https://github.com/joshuaulrich/quantmod}{quantmod}
#' package.
#'
#' @examples
#' data(tech)
#' head(tech)
#' # NOT RUN - change in correlation over time
#' # library(dplyr)
#' # tech_grp <- tech %>% 
#' #         group_by(year) %>%
#' #         inspect_cor()
#' # tech_grp %>% show_plot()    
"tech"