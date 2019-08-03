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
#' @importFrom stats pnorm
#' @importFrom stats cor.test

cor_test <- function(cor_1, cor_2, n_1, n_2){
  var_diff       <- sqrt(1/(n_1 - 3) + 1/(n_2 - 3))
  zstat          <- (atanh(cor_1) - atanh(cor_2)) / var_diff
  (2 * pnorm(-abs(zstat)))
}


# univariate correlation tests
cor_test_1 <- function(df_input, df_name, with_col, alpha, method){
  # every combination of variables
  c_nms   <- c_nms_1 <- c_nms_2 <- colnames(df_input)
  c_nms_1 <- if(is.null(with_col)) c_nms_1 else with_col
  c_cmbs  <- expand.grid(col_1 = factor(c_nms_1, levels = c_nms_2), col_2 =  c_nms_2) 
  c_cmbs  <- if(is.null(with_col)) c_cmbs %>% 
    filter(as.numeric(col_1) > as.numeric(col_2)) else c_cmbs
  c_cmbs  <- c_cmbs %>%
    filter(!col_1 == col_2) %>%
    mutate_all(as.character) %>% 
    mutate(pair = paste(col_1, col_2, sep = " & "))
  # loop over rows and calculate correlation, p.value and cint
  out_cors  <- vector("list", length = nrow(c_cmbs))
  total_its <- nrow(c_cmbs)
  pb <- start_progress(prefix = " Column pair", total = total_its)
  for(i in 1:total_its){
    update_progress(bar = pb, iter = i, total = total_its, what = c_cmbs$pair[i])
    c_df <- df_input[c(c_cmbs$col_1[i], c_cmbs$col_2[i])]
    # get correlation coefficients
    out_cors[[i]] <- get_corr(c_df[, 1, drop = TRUE], c_df[, 2, drop = TRUE], 
                              alpha = alpha, method = method)
  }
  # combine into a single tibble
  cor_out <- bind_cols(c_cmbs, bind_rows(out_cors)) %>% 
    arrange(desc(abs(corr))) %>% as_tibble()
    
  # return tibble of correlations
  return(cor_out)
}

# 
get_corr <- function(x, y, alpha, method){
  # get point estimates and pvalues
  ctest <- try(cor.test(x, y, conf.level = 1 - alpha, method = method), silent = TRUE)
  if(class(ctest) == "try-error"){
    cor_tab <- tibble(corr = NA, p_value = NA, lower = NA, upper = NA) 
  } else {
    cor_tab <- tibble(corr = ctest$estimate, p_value = ctest$p.value)
    if(grepl("pearson", ctest$method, ignore.case = TRUE)){
      if(!is.null(ctest$conf.int)){
        cor_tab$lower <- ctest$conf.int[1]
        cor_tab$upper <- ctest$conf.int[2]
      } else {
        cor_tab$lower <- cor_tab$upper <- NA
      }
    }
    if(grepl("kendall", ctest$method, ignore.case = TRUE)){
      # https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_Kendalls_Tau-b_Correlation.pdf
      cor_tab$lower <- tanh(atanh(ctest$estimate) - qnorm(1 - (alpha/2)) * (1 / sqrt(length(x) - 3)))
      cor_tab$upper <- tanh(atanh(ctest$estimate) + qnorm(1 - (alpha/2)) * (1 / sqrt(length(x) - 3)))
    }
    if(grepl("spearman", ctest$method, ignore.case = TRUE)){
      # this is an approximate test based on this
      # https://stats.stackexchange.com/questions/18887/how-to-calculate-a-confidence-interval-for-spearmans-rank-correlation
      cor_tab$lower <- tanh(atanh(ctest$estimate) - qnorm(1 - (alpha/2)) * (1 / sqrt(length(x) - 3)))
      cor_tab$upper <- tanh(atanh(ctest$estimate) + qnorm(1 - (alpha/2)) * (1 / sqrt(length(x) - 3)))
    }
  }
  return(cor_tab)
}












