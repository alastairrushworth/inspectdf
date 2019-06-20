#' @importFrom stats prop.test

prop_test <- function(na_1, na_2, n_1, n_2, return_ci = FALSE){
  p_value <- vector("numeric", length = length(na_1))
  ci_list <- vector("list", length = length(na_1))
  if(length(p_value) > 0){
    for(i in 1:length(p_value)){
      if((is.na(na_1[i]) | is.na(na_2[i])) | (na_1[i] == 0 & na_2[i] == 0)){
        p_value[i] <- NA
        ci_list[[i]] <- NA
      } else {
        test_out <- suppressWarnings(prop.test(c(na_1[i], na_2[i]), c(n_1, n_2)))
        p_value[i] <- test_out$p.value
        ci_list[[i]] <- as.numeric(test_out$conf.int)
      }
    }
  }
  return_obj <- if(return_ci) list(p_value = p_value, ci_list = ci_list) else p_value
  return(return_obj)
}




prop_test_imb <- function(imbal_tab, n_1, n_2){
  p_value <- vector("numeric", length = nrow(imbal_tab))
  if(length(p_value) > 0){
    for(i in 1:nrow(imbal_tab)){
      cnt_1 <- imbal_tab$cnt_1[i]
      cnt_2 <- imbal_tab$cnt_2[i]
      if(is.na(cnt_1) | is.na(cnt_2)){
        p_value[i] <- NA
      } else {
        ptst  <- suppressWarnings(prop.test(c(cnt_1, cnt_2), c(n_1, n_2)))
        p_value[i] <- ptst$p.value
      }
    }
  }
  return(p_value)
}