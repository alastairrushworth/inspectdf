#' Report the most commonly occuring value in each non-numeric column
#'
#' @param df A data frame
#' @param df2 An optional second data frame for comparing feature imbalance with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param type Character specificying report output type.  Default \code{type = "df"} causes report to be returned as a tibble.   \code{type = "console"} causes report to be returned directly to the console.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{value_1}, \code{percent_1}, \code{value_2}, \code{percent_2}.  The \code{value} is the most frequently occuring category in each column and \code{percent_in_col} is the percentage frequency with which it occurs.
#' @examples
#' report_imbalance(starwars)

report_imbalance <- function(df, df2 = NULL, top = NULL, type = "df"){
  
  # perform basic column check on dataframe input
  check_df_cols(df)
  
  # exclude columns containing lists (or numeric if not specified)
  col_exclude <- function(v) !(is.list(v) | is.numeric(v)) 
  
  if(is.null(df2)){
    gdf         <- df %>% dplyr::select_if(col_exclude)
    # calculate imbalance if any columns available
    if(ncol(gdf) > 0){
      cnames      <- colnames(gdf)
      # function to find the percentage of the most common value in a vector
      imb_cols       <- do.call("rbind", lapply(gdf, fast_table))
      imb_cols$names <- colnames(gdf)
      # get top ten most imbalance by common class and pass to histogrammer
      out <- imb_cols %>% dplyr::arrange(desc(prop)) %>% dplyr::slice(1:min(top, nrow(.)))
      
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
  } else {
    s1 <- report_imbalance(df,  top = top, type = type) %>% dplyr::rename(value_1 = value, percent_1 = percent_in_col)
    s2 <- report_imbalance(df2, top = top, type = type) %>% dplyr::rename(value_2 = value, percent_2 = percent_in_col)
    imbal_tab <- dplyr::full_join(s1, s2, by = "col_name") %>%
      mutate(p_value = prop_test_imbalance(., n_1 = nrow(df), n_2 = nrow(df2)))
    return(imbal_tab)
  }
}