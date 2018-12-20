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

get_newlevel_tibble <- function(Mlist1, Mlist2){
  out_vec <- vector("list", length = length(Mlist1))
  if(length(Mlist2) > 0){
    for(i in 1:length(out_vec)){
      out_vec[[i]] <- anti_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>% 
        mutate(origin_df = "df1") %>%
        bind_rows(
          anti_join(Mlist2[[i]], Mlist1[[i]], by = "value") %>% 
            mutate(origin_df = "df2")) %>%
        select(-prop) 
    }
  }
  return(out_vec)
}

psi <- function(Mlist1, Mlist2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      out_vec[i] <- full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
        replace_na(list(prop.x = 0, prop.y = 0)) %>%
        filter(!prop.x == 0, !prop.y == 0) %>%
        mutate(psi_vals = (prop.y - prop.x) * log(prop.y / prop.x)) %>% 
        summarise(psi = sum(psi_vals)) %>% 
        as.numeric
    }
  }
  return(out_vec)
}

chisq <- function(Mlist1, Mlist2, n_1, n_2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      out_vec[i] <- suppressWarnings(full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
                                       replace_na(list(prop.x = 0, prop.y = 0)) %>%
                                       mutate(prop.x = n_1 * prop.x, prop.y = n_2 * prop.y) %>%
                                       select(prop.y, prop.x) %>% 
                                       as.matrix %>% t %>% chisq.test %>% .$statistic)
    }
  }
  return(out_vec)
}

chisq_p <- function(Mlist1, Mlist2, n_1, n_2){
  out_vec <- vector("numeric", length = length(Mlist1))
  if(length(Mlist1) > 0){
    for(i in 1:length(out_vec)){
      out_vec[i] <- suppressWarnings(full_join(Mlist1[[i]], Mlist2[[i]], by = "value") %>%
                                       replace_na(list(prop.x = 0, prop.y = 0)) %>%
                                       mutate(prop.x = n_1 * prop.x, prop.y = n_2 * prop.y) %>%
                                       select(prop.y, prop.x) %>% 
                                       as.matrix %>% t %>% chisq.test %>% .$p.value)
    }
  }
  return(out_vec)
}

