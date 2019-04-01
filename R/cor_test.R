#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select_
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom stats pnorm
#' @importFrom stats cor.test

cor_test <- function(cor_1, cor_2, n_1, n_2){
  fisher_trans_1 <- 0.5 * log((1 + cor_1)/(1 - cor_1))
  fisher_trans_2 <- 0.5 * log((1 + cor_2)/(1 - cor_2))
  var_diff       <- sqrt(1/(n_1 - 3) + 1/(n_2 - 3))
  zstat          <- (fisher_trans_1 - fisher_trans_2) / var_diff
  (2 * pnorm(-abs(zstat)))
}

# univariate correlation tests
cor_test_1 <- function(df_input, alpha = 0.05){
  # every combination of variables
  c_nms <- colnames(df_input)
  c_cmbs <- expand.grid(col_1 =  c_nms, col_2 =  c_nms) %>%
    filter(as.numeric(col_1) > as.numeric(col_2), !col_1 == col_2) %>%
    mutate_all(as.character) %>% 
    mutate(pair = paste(col_1, col_2, sep = " & "))
  # loop over rows and calculate correlation, p.value and cint
  out_cors <- vector("list", length = nrow(c_cmbs))
  for(i in 1:nrow(c_cmbs)){
    c_df   <- df_input %>% select_(c_cmbs$col_1[i], c_cmbs$col_2[i])
    c_test <- try(cor.test(c_df[, 1, drop = TRUE], 
                           c_df[, 2, drop = T], 
                           conf.level = 1 - alpha / 2), 
                  silent = TRUE)
    if(!class(c_test) == "try-error"){
      out_cors[[i]] <- tibble(corr = c_test$estimate, 
                              p_value = c_test$p.value,
                              lower = c_test$conf.int[1],
                              upper = c_test$conf.int[2]) 
    } else {
      out_cors[[i]] <- tibble(corr = NA, 
                              p_value = NA,
                              lower = NA,
                              upper = NA) 
    }

  }
  # combine into a single tibble
  cor_out <- bind_cols(c_cmbs, bind_rows(out_cors)) %>% 
    arrange(desc(abs(corr))) %>% as_tibble()
    
  # return tibble of correlations
  return(cor_out)
}