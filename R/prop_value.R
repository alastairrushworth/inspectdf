#' @importFrom dplyr lag
#' @importFrom tibble tibble
#' @importFrom utils tail

prop_value <- function(L){
  # ensure -Inf is the first break 
  if(!L$breaks[1] == -Inf){
    L$breaks <- c(-Inf, L$breaks)
    L$counts <- c(0, L$counts)
  }
  # ensure Inf is the last break
  if(!tail(L$breaks, 1) == Inf){
    L$breaks <- c(L$breaks, Inf)
    L$counts <- c(L$counts, 0)
  }
  props <- L$counts / sum(L$counts)
  labs <- paste0("[", dplyr::lag(L$breaks), ", ", L$breaks, ")")[-1]
  return(tibble(value = labs, prop = props))
}