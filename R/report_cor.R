report_cor <- function(df, top_n = 10, type = "df"){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  # filter to only the numeric variables
  df_numeric <- df %>% select_if(is.numeric)
  # remove anything that is constant
  df_numeric <- df_numeric %>% select(-which(sapply(df_numeric, sd) == 0))
  
  # calculate correlation coefficients
  if(ncol(df_numeric) > 0){
    cor_num_mat <- suppressWarnings(cor(df_numeric, use = "pairwise.complete.obs"))
    cor_num_mat[lower.tri(cor_num_mat, diag = T)] <- NA
    cor_df <- tibble::as.tibble(cor_num_mat)
    cor_df$X1 <- colnames(cor_df)
    cor_df    <- tidyr::gather(cor_df, key = "X2", value = "cor", -X1)
    cor_df <- cor_df %>% dplyr::filter(!is.na(cor)) %>%
      dplyr::arrange(desc(abs(cor))) %>%
      dplyr::mutate(pair = paste(X1, X2, sep = " & ")) %>%
      dplyr::select(X1, X2, pair, cor) 
    out <- cor_df %>% dplyr::slice(1:top_n) 
    # if user doesn't request dataframe output
    if(type == "console"){
      # print title text
      console_title("Most correlated numeric pairs")
      # print console chart
      out %>% select(-X1, -X2, cor, pair) %>% dot_bars_cor 
      # invisibly return the dataframe input
      invisible(df)
    } 
    if(type == "df"){
      # return dataframe of 
      return(out)
    }
  } else {
    if(type == "console"){
    # print title text
    console_title("Most correlated numeric pairs")
    # print NULL message
    cat(silver("    << Not applicable >>\n"))
    # invisibly return the dataframe input
    invisible(df)
    } 
    if(type == "df"){
      # return empty dataframe of 
      return(tibble(X1 = character(), X2 = character(), 
                    pair = character(), cor = numeric()))
    }
  }
}
