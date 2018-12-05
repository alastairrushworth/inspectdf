cor_test <- function(cor_1, cor_2, n_1, n_2){
  fisher_trans_1 <- 0.5 * log((1 + cor_1)/(1 - cor_1))
  fisher_trans_2 <- 0.5 * log((1 + cor_2)/(1 - cor_2))
  var_diff       <- sqrt(1/(n_1 - 3) + 1/(n_2 - 3))
  zstat          <- (fisher_trans_1 - fisher_trans_2) / var_diff
  1 - (2 * pnorm(-abs(zstat)))
}