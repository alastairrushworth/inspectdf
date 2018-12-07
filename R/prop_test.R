prop_test <- function(na_1, na_2, n_1, n_2){
  p_value <- vector("numeric", length = length(na_1))
  for(i in 1:length(p_value)){
    if((is.na(na_1[i]) | is.na(na_2[i])) | (na_1[i] == 0 & na_2[i] == 0)){
      p_value[i] <- NA
    } else {
      p_value[i] <- suppressWarnings(prop.test(c(na_1[i], na_2[i]), c(n_1, n_2))$p.value)
    }
  }
  return(p_value)
}




prop_test_imbalance <- function(imbal_tab, n_1, n_2){
  p_value <- vector("numeric", length = nrow(imbal_tab))
  for(i in 1:nrow(imbal_tab)){
    if((!(imbal_tab$value_1[i] == imbal_tab$value_2[i])) | (is.na(imbal_tab$percent_1[i])|is.na(imbal_tab$percent_2[i]))){
      p_value[i] <- NA
    } else {
      p_value[i] <- suppressWarnings(prop.test(c(imbal_tab$percent_1[i] * n_1, imbal_tab$percent_2[i] * n_2), c(n_1, n_2))$p.value)
    }
  }
  return(p_value)
}