#' Report Goodman and Kruskal's tau for association between categorical features
#'
#' @param df A data frame
#' @param top_n The number of rows to print for summaries. Default \code{top_n = NULL} prints everything.
#' @param type Character specificying report output type.  Default \code{type = "df"} causes report to be returned as a tibble.   \code{type = "console"} causes report to be returned directly to the console.
#' @return Return a \code{tibble} containing the columns \code{col_1}, \code{col_2} and \code{pair} and \code{association}.  The report contains only the upper triangle of the correlation matrix.  
#' @examples
#' report_association(starwars)

report_association <- function(df, top_n = NULL, type = "df"){
  
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
      dplyr::select(col_1 = X1, col_2 = X2, pair, association = ass) 
    out <- ass_df %>% dplyr::slice(1:min(top_n, nrow(.))) 
    # if user doesn't request dataframe output
    if(type == "console"){
      # print title text
      console_title("Most associated categorical pairs")
      # print console chart
      out %>% select(-col_1, -col_2, ass = association, pair)  %>% dot_bars_ass
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
    } 
    if(type == "df"){
      # return empty dataframe of 
      return(tibble(col_1 = character(), col_2 = character(), 
                    pair = character(), association = numeric()))
    }
  }
  if(type == "console") invisible(df)
}