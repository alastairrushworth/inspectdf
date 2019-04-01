#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stats fisher.test
#' @importFrom tidyr replace_na

fisher <- function(Mlist1, Mlist2, n_1, n_2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      if(is.null(Mlist1[[i]]) | is.null(Mlist2[[i]])){
        out_vec[i] <- NA
      } else {
        out_vec[i] <- suppressWarnings(
          # join the histogrmas together
          full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
            # replace anything missing with 0
            replace_na(list(prop.x = 0, prop.y = 0)) %>%
            # proportion of counts in each histogram
            mutate(prop.x = n_1 * prop.x, prop.y = n_2 * prop.y) %>%
            # drop the values column
            select(-value) %>% 
            # convert to a matrix and transpose to a pair of row vectors
            as.matrix %>% t %>%
            # apply fisher's exact test and extract the statistic
            fisher.test(., simulate.p.value = TRUE, B = 10000) %>% 
            .$p.value
          )
      }
    }
  }
  return(out_vec)
}