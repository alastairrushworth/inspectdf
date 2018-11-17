fast_table <- function(v){
  vsort  <- sort(v, method = "quick")
  vals   <- unique(vsort)
  if(any(class(v) %in% c("integer", "numeric", "double", "factor"))){
    freq   <- count_levels_num(vsort)
  } else if(any(class(v) %in% c("character", "Date", "POSIXt"))){
    freq   <- count_levels_char(vsort)
  } else {
    freq <- NA
  }
  tibble(value = vals, prop = freq / length(v)) %>% 
    dplyr::arrange(desc(prop)) %>% 
    dplyr::slice(1)
}