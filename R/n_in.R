#' @importFrom magrittr %>%

n_in <- function(v1, v2){
  if(length(v1) > 0){
    out_vec <- vector("numeric", length = length(v1))
    for(i in 1:length(v1)){
      out_vec[i] <- sum(is.na(left_join(v1[[i]], v2[[i]], by = "value")$prop.y))
    }
  } else {
    out_vec <- numeric()
  }
  return(out_vec)
}

get_break <- function(L){
  out <- strsplit(gsub("\\[|,|\\)", "", L$value), " ") %>%
    unlist %>% as.numeric %>% unique %>% sort
  return(out)
}


