#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr slice
#' @importFrom tibble tibble

fast_table <- function(v, show_all = F){
  vsort  <- sort(v, method = "quick")
  vals   <- unique(vsort)
  if(any(class(v) %in% c("integer", "numeric", "double", "factor"))){
    freq   <- count_levels_num(vsort)
  } else if(any(class(v) %in% c("character", "Date", "POSIXt"))){
    freq   <- count_levels_char(vsort)
  } else {
    freq <- NA
  }
  if(!show_all){
    tibble(value = vals, prop = freq / length(v)) %>% 
      dplyr::arrange(desc(prop)) %>% 
      dplyr::slice(1) %>%
      return
  } else {
    tibble(value = vals, prop = freq / length(v)) %>% 
      dplyr::arrange(desc(prop)) %>% 
      return
  }
}