#' @importFrom dplyr lag
#' @importFrom tibble tibble
#' @importFrom utils tail

prop_value <- function(L){
  props <- L$counts / sum(L$counts)
  labs <- paste0("[", dplyr::lag(L$breaks), ", ", L$breaks, ")")[-1]
  return(tibble(value = labs, prop = props))
}