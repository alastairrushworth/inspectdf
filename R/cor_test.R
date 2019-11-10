#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select_
#' @importFrom magrittr %>%
#' @importFrom progress progress_bar
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats cor

cor_test <- function(cor_1, cor_2, n_1, n_2){
  var_diff       <- sqrt(1/(n_1 - 3) + 1/(n_2 - 3))
  zstat          <- (atanh(cor_1) - atanh(cor_2)) / var_diff
  (2 * pnorm(-abs(zstat)))
}

# univariate correlation tests
cor_test_2 <- function(df_input, df_name, with_col, alpha, method){
  # if with_col is specified, then only return a subset
  x <- if(is.null(with_col)) df_input else df_input[with_col]
  y <- if(is.null(with_col)) NULL else df_input[-which(names(df_input) %in% with_col)]
  # need some way to count how many pairwise complete obs there are
  if(is.null(with_col)){
    nna_mat <- crossprod(!is.na(x))
  } else {
    nna_mat <- t((!is.na(x))) %*% (!is.na(y))  
  }
  if(is.null(with_col)) nna_mat[upper.tri(nna_mat, diag = TRUE)] <- Inf
  # get the number of non-null elements
  nna_df <- nna_mat %>%
    as_tibble(rownames = 'col_1') %>% 
    gather(key = "col_2", value = "nna", -col_1) %>%
    filter(!nna == Inf)
  # get the correlation table
  cd <- cor(x = x, y = y, use = "pairwise.complete.obs", method = method)
  if(is.null(with_col)) cd[upper.tri(cd, diag = TRUE)] <- Inf
  cor_out <- cd %>%
    as_tibble(rownames = 'col_1') %>% 
    gather(key = "col_2", value = "corr", -col_1) %>%
    filter(!corr == Inf | is.na(corr)) %>%
    mutate(nna = nna_df$nna, se = (1 / sqrt(nna - 3)), pcnt_nna = 100 * nna / nrow(df_input)) %>%
    arrange(desc(abs(corr))) %>%
    mutate(p_value = 2 * pnorm(-abs(corr / se))) %>%
    mutate(lower = tanh(atanh(corr) - qnorm(1 - (alpha/2)) * se)) %>%
    mutate(upper = tanh(atanh(corr) + qnorm(1 - (alpha/2)) * se)) %>%
    mutate(pair = paste(col_1, col_2, sep = " & ")) %>%
    select(col_1, col_2, pair, corr, p_value, lower, upper, pcnt_nna)

  # return tibble of correlations
  return(cor_out)
}

