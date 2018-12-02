#' Report the Pearson's correlation coefficient for each pair of numeric columns
#'
#' @param df A data frame
#' @param top_n The number of rows to print for summaries. Default \code{top_n = NULL} prints everything.
#' @param type Character specificying report output type.  Default \code{type = "df"} causes report to be returned as a tibble.   \code{type = "console"} causes report to be returned directly to the console.
#' @return Return a \code{tibble} containing the columns \code{col_1}, \code{col_2} and \code{pair} and \code{correlation}.  The report contains only the upper triangle of the correlation matrix.  The tibble is sorted by descending absolute value in the \code{correlation} column.
#' @examples
#' report_cor(mtcars)

report_cor <- function(df, top_n = NULL, type = "df"){
  
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
      dplyr::select(col_1 = X1, col_2 = X2, pair, correlation = cor) 
    out <- cor_df %>% dplyr::slice(1:min(top_n, nrow(.))) 
    # if user doesn't request dataframe output
    if(type == "console"){
      # print title text
      console_title("Most correlated numeric pairs")
      # print console chart
      out %>% select(-col_1, -col_2, cor = correlation, pair) %>% dot_bars_cor 
    } 
    if(type == "df"){
      # return dataframe of correlations
      return(out)
    }
  } else {
    if(type == "console"){
      # print title text
      console_title("Most correlated numeric pairs")
      # print NULL message
      cat(silver("    << Not applicable >>\n"))
    } 
    if(type == "df"){
      # return empty dataframe of 
      return(tibble(col_1 = character(), col_2 = character(), 
                    pair = character(), correlation = numeric()))
    }
  }
  if(type == "console") invisible(df)
}
