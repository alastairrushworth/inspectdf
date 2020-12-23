#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stats fisher.test
#' @importFrom tidyr replace_na

chisq <- function(Mlist1, Mlist2, n_1, n_2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      if(is.null(Mlist1[[i]]) | is.null(Mlist2[[i]])){
        out_vec[i] <- NA
      } else {
        counts <- suppressWarnings(
          # join the histogrmas together
          full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
            # replace anything missing with 0
            replace_na(list(prop.x = 0, prop.y = 0)) %>%
            # proportion of counts in each histogram
            mutate(prop.x = as.integer(n_1 * prop.x), prop.y = as.integer(n_2 * prop.y)) %>%
            # drop the values column
            select(-value, -contains("cnt")) %>% 
            # drop rows where both are exactly 0
            filter(prop.x > 0 | prop.y > 0) %>%
            # convert to a matrix and transpose to a pair of row vectors
            as.matrix %>% t)
        # apply fisher's exact test and extract the statistic
        small_cats <- which(colSums(counts) <= 10)
        if(length(small_cats) > 1) counts <- cbind(counts[, -small_cats], rowSums(counts[, small_cats]))
        go_fish <- try(suppressWarnings(chisq.test(counts)$p.value), silent = TRUE)
        out_vec[i] <- ifelse("try-error" %in% class(go_fish), NA, go_fish)
      }
    }
  }
  return(out_vec)
}


