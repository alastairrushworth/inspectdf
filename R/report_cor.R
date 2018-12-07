#' Report the Pearson's correlation coefficient for each pair of numeric columns
#'
#' @param df1 A data frame containing numeric columns
#' @param df2 An optional second data frame for comparing correlation coefficients with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Return a \code{tibble} containing the columns \code{col_1}, \code{col_2} and \code{pair} and \code{correlation}.  The report contains only the upper triangle of the correlation matrix.  The tibble is sorted by descending absolute value in the \code{correlation} column.
#' @details When the second data frame \code{df2} is specified, correlations are tabulated for both data frames, and where a pair of numeric columns with the same names appear in both, a p-value is provided which test tests whether their correlations coefficients are equal.
#' @examples
#' report_cor(starwars)
#' report_cor(starwars, starwars[1:10, ])

report_cor <- function(df1, df2 = NULL, top = NULL, show_plot = F){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # filter to only the numeric variables
  df_numeric <- df1 %>% select_if(is.numeric)
  # remove anything that is constant
  df_numeric <- df_numeric %>% select(-which(sapply(df_numeric, sd) == 0))
  
  if(is.null(df2)){
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
        dplyr::select(col_1 = X1, col_2 = X2, pair, correlation = cor) 
      out <- cor_df %>% dplyr::slice(1:min(top, nrow(.))) 
      # return dataframe of correlations
      return(out)
    } else {
      # return empty dataframe of 
      return(tibble(col_1 = character(), col_2 = character(), 
                    pair = character(), correlation = numeric()))
    } 
  } else {
    s1 <- report_cor(df1, top = top, show_plot = F) %>% dplyr::rename(correlation_1 = correlation)
    s2 <- report_cor(df2, top = top, show_plot = F) %>% dplyr::select(pair, correlation_2 = correlation)
    cor_tab <- dplyr::full_join(s1, s2, by = "pair")
    cor_tab$p_value <- cor_test(cor_tab$correlation_1, cor_tab$correlation_2, n_1 = nrow(df1), n_2 = nrow(df2))
    return(cor_tab)
  }
}
