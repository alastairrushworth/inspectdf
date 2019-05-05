#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na

js_divergence_vec <- function(Mlist1, Mlist2){
  log_zero <- function(g){
    ifelse(g == 0, NA, log(g)/log(2.0))
  }
  js_divergence <- function(p, q){
    m <- 0.5 * (p + q)
    0.5 * (sum(p * log_zero(p / m), na.rm = T) + sum(q * log_zero(q / m), na.rm = T))
  }
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      if(is.null(Mlist1[[i]]) | is.null(Mlist2[[i]])){
        out_vec[i] <- NA
      } else {
        out_vec[i] <- full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
          replace_na(list(prop.x = 0, prop.y = 0)) %>%
          summarise(jsd = js_divergence(prop.x, prop.y)) %>%
          as.numeric
      }
    }
  }
  return(out_vec)
}
