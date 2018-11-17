report_association <- function(df, plots = F, top_n = 10){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # print title text
  console_title("Most associated categorical pairs")
  
  df_cat <- df %>% 
    select_if(function(v) is.character(v) | is.factor(v)) %>% 
    as.data.frame
  if(ncol(df_cat) > 1){
    GKMat <- GoodmanKruskal::GKtauDataframe(df_cat)
    if(plots) GKMat %>% plot
    ass_mat <- GKMat
    class(ass_mat) <- "matrix"
    diag(ass_mat) <- NA
    ass_df <- tibble::as.tibble(ass_mat)
    ass_df$X1 <- colnames(ass_df)
    ass_df  <- tidyr::gather(ass_df, key = "X2", value = "ass", -X1)
    ass_df <- ass_df %>% dplyr::filter(!is.na(ass)) %>%
      dplyr::arrange(desc(abs(ass))) %>%
      dplyr::mutate(pair = paste(X1, X2, sep = " -> ")) %>%
      dplyr::select(-X1, -X2)
    ass_df %>% dplyr::slice(1:top_n) %>% dot_bars_ass
  } else {
    cat(silver("    << Not applicable >>\n"))
  }
  # invisibly return the df for further summaries
  invisible(df)
}