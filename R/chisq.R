#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stats chisq.test
#' @importFrom tidyr replace_na

chisq <- function(Mlist1, Mlist2, n_1, n_2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      if(is.null(Mlist1[[i]]) | is.null(Mlist2[[i]])){
        out_vec[i] <- NA
      } else {
        out_vec[i] <- suppressWarnings(full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
                                         replace_na(list(prop.x = 0, prop.y = 0)) %>%
                                         mutate(prop.x = n_1 * prop.x, prop.y = n_2 * prop.y) %>%
                                         select(prop.y, prop.x) %>% 
                                         as.matrix %>% t %>% chisq.test %>% .$statistic)
      }
    }
  }
  return(out_vec)
}

chisq_p <- function(Mlist1, Mlist2, n_1, n_2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      if(is.null(Mlist1[[i]]) | is.null(Mlist2[[i]])){
        out_vec[i] <- NA
      } else {
        out_vec[i] <- suppressWarnings(full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
                                         replace_na(list(prop.x = 0, prop.y = 0)) %>%
                                         mutate(prop.x = n_1 * prop.x, prop.y = n_2 * prop.y) %>%
                                         select(prop.y, prop.x) %>% 
                                         as.matrix %>% t %>% chisq.test %>% .$p.value)
      }
    }
  }
  return(out_vec)
}