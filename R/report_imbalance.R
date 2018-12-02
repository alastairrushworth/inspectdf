#' Report the most commonly occuring value in each non-numeric column
#'
#' @param df A data frame
#' @param top_n The number of rows to print for summaries. Default \code{top_n = NULL} prints everything.
#' @param type Character specificying report output type.  Default \code{type = "df"} causes report to be returned as a tibble.   \code{type = "console"} causes report to be returned directly to the console.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{value} and \code{percent_in_col}.  The \code{value} is the most frequently occuring category in each column and \code{percent_in_col} is the percentage frequency with which it occurs.
#' @examples
#' report_imbalance(starwars)

report_imbalance <- function(df, top_n = NULL, type = "df"){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # exclude columns containing lists (or numeric if not specified)
  col_exclude <- function(v) !(is.list(v) | is.numeric(v)) 
  gdf         <- df %>% dplyr::select_if(col_exclude)
  
  # calculate imbalance if any columns available
  if(ncol(gdf) > 0){
    cnames      <- colnames(gdf)
    # function to find the percentage of the most common value in a vector
    imb_cols       <- do.call("rbind", lapply(gdf, fast_table))
    imb_cols$names <- colnames(gdf)
    # get top ten most imbalance by common class and pass to histogrammer
    out <- imb_cols %>% dplyr::arrange(desc(prop)) %>% dplyr::slice(1:min(top_n, nrow(.)))

    if(type == "console"){
      # print console title text
      console_title("Top most imbalanced features (exlc. numeric")
      # print console chart
      out %>% dot_bars_imbalance
    }
    if(type == "df"){
      # return dataframe of values
      return(out %>% select(col_name = names, value, percent_in_col = prop))
    }
  } else {
      if(type == "console"){
        # print console title text
        console_title("Top most imbalanced features (exlc. numeric")
        # print console chart
        cat(silver("    << Not applicable >>\n"))
      } 
    if(type == "df"){
      # return empty dataframe of 
      return(tibble(col_name = character(), value = character(), percent_in_col = numeric()))
    }
  }
  if(type == "console") invisible(df)
}