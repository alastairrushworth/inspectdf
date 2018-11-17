

report_cor <- function(df, plots = F, top_n = 10){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # print title text
  console_title("Most correlated numeric pairs")
  
  # filter to only the numeric variables
  df_numeric <- df %>% select_if(is.numeric)
  
  # remove anything that is constant
  df_numeric <- df_numeric %>% select(-which(sapply(df_numeric, sd) == 0))
  
  # calculate correlation coefficients
  if(ncol(df_numeric) > 0){
    if(plots){
      df_numeric %>% 
        corrr::correlate(.) %>%
        corrr::network_plot(min_cor = .0, legend = T) %>%
        print
    }
    cor_num_mat <- suppressWarnings(cor(df_numeric, use = "pairwise.complete.obs"))
    cor_num_mat[lower.tri(cor_num_mat, diag = T)] <- NA
    cor_df <- tibble::as.tibble(cor_num_mat)
    cor_df$X1 <- colnames(cor_df)
    cor_df    <- tidyr::gather(cor_df, key = "X2", value = "cor", -X1)
    cor_df <- cor_df %>% dplyr::filter(!is.na(cor)) %>%
      dplyr::arrange(desc(abs(cor))) %>%
      dplyr::mutate(pair = paste(X1, X2, sep = " & ")) %>%
      dplyr::select(-X1, -X2)
    cor_df %>% dplyr::slice(1:top_n) %>% dot_bars_cor
  } else {
    cat(silver("    << Not applicable >>\n"))
  }
  
  # invisibly return the df for further summaries
  invisible(df)
}
