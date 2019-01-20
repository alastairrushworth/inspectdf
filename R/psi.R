#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na

psi <- function(Mlist1, Mlist2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      if(is.null(Mlist1[[i]]) | is.null(Mlist2[[i]])){
        out_vec[i] <- NA
      } else {
        out_vec[i] <- full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
          replace_na(list(prop.x = 0, prop.y = 0)) %>%
          filter(!prop.x == 0, !prop.y == 0) %>%
          mutate(psi_vals = (prop.y - prop.x) * log(prop.y / prop.x)) %>% 
          summarise(psi = sum(psi_vals)) %>% 
          as.numeric
      }
    }
  }
  return(out_vec)
}
