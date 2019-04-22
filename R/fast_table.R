#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr desc
#' @importFrom dplyr slice
#' @importFrom tibble tibble

fast_table <- function(v, show_na = TRUE, show_cnt = FALSE){
  vsort  <- sort(v, method = "quick", na.last = TRUE)
  vals   <- unique(vsort)
  if(any(class(v) %in% c("integer", "numeric", "double", "factor"))){
    freq   <- count_levels_num(vsort)
  } else if(any(class(v) %in% c("logical", "character", "Date", "POSIXt"))){
    freq   <- count_levels_char(vsort)
  } else {
    freq <- NA
  }
  # recombine levels with frequencies
  tbl_freq <-  tibble(value = vals, prop = freq / length(v)) %>% 
    arrange(desc(prop))
  # if frequencies are required, add them here
  if(show_cnt) tbl_freq$cnt <- freq
  # if the values column is not numeric, then coerce to character
  if(!any("numeric" %in% class(tbl_freq$value))){
    tbl_freq$value <- as.character(tbl_freq$value)
  }
  # return frequency table
  return(tbl_freq)
}