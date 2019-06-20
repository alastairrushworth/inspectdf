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
            filter(prop.x > 0 & prop.y > 0) %>%
            # pool groups if there are too many
            pool_groups(., 10) %>%
            # convert to a matrix and transpose to a pair of row vectors
            as.matrix %>% t)
        # apply fisher's exact test and extract the statistic
        go_fish <- try(fisher.test(counts, hybrid = FALSE) %>% .$p.value, silent = TRUE)
        out_vec[i] <- ifelse("try-error" %in% class(go_fish), NA, go_fish)
      }
    }
  }
  return(out_vec)
}

pool_groups <- function(df, nrw){
  df_sub <- df
  while(nrow(df_sub) > nrw){
    # combine the two smallest levels
    df_last_two <- tail(df_sub, 2) %>% colSums
    df_sub <- df_sub %>% 
      slice(1:(nrow(df_sub) - 2)) %>% 
      bind_rows(df_last_two) %>% 
      arrange(desc(prop.x))
  }
  return(df_sub)
}

