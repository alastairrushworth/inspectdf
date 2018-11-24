report_association <- function(df, top_n = 10, type = "df"){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # pick out categorical columns
  df_cat <- df %>% 
    select_if(function(v) is.character(v) | is.factor(v)) %>% 
    as.data.frame
  
  # calculate association if categorical columns exist
  if(ncol(df_cat) > 1){
    GKMat <- GoodmanKruskal::GKtauDataframe(df_cat)
    ass_mat <- GKMat
    class(ass_mat) <- "matrix"
    diag(ass_mat) <- NA
    ass_df <- tibble::as.tibble(ass_mat)
    ass_df$X1 <- colnames(ass_df)
    ass_df  <- tidyr::gather(ass_df, key = "X2", value = "ass", -X1)
    ass_df <- ass_df %>% dplyr::filter(!is.na(ass)) %>%
      dplyr::arrange(desc(abs(ass))) %>%
      dplyr::mutate(pair = paste(X1, X2, sep = " -> ")) %>%
      dplyr::select(X1, X2, pair, ass) 
    out <- ass_df %>% dplyr::slice(1:top_n) 
    # if user doesn't request dataframe output
    if(type == "console"){
      # print title text
      console_title("Most associated categorical pairs")
      # print console chart
      out %>% select(-X1, -X2, ass, pair)  %>% dot_bars_ass
      # invisibly return the dataframe input
      invisible(df)
    } 
    if(type == "df"){
      # return dataframe of values
      return(out)
    }
  } else {
    if(type == "console"){
      # print title text
      console_title("Most associated categorical pairs")
      # print NULL message
      cat(silver("    << Not applicable >>\n"))
      # invisibly return the dataframe input
      invisible(df)
    } 
    if(type == "df"){
      # return empty dataframe of 
      return(tibble(X1 = character(), X2 = character(), 
                    pair = character(), ass = numeric()))
    }
  }
}