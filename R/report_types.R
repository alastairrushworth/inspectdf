#' Report column types of a data frame 
#'
#' @param df1 A data frame to report column types
#' @param df2 An optional second data frame for comparing column types with.  Defaults to \code{NULL}.
#' @param type Character specificying report output type.  Default \code{type = "df"} causes report to be returned as a tibble.   \code{type = "console"} causes report to be returned directly to the console.
#' @return Prints the proportion of columns with each type.
#' @details When the second data frame \code{df2} is specified, column types are tabulated for both data frames to enable comparison of contents.
#' @examples
#' report_types(starwars)

report_types <- function(df1, df2 = NULL, show_plot = F){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  if(is.null(df2)){
    # number of columns
    ncl         <- ncol(df1)
    # possible types to look out for
    type_spine  <- tibble(type = c("logical", "integer", "numeric", "character", 
                                   "factor",  "list",    "matrix",  "data.frame", 
                                   "ordered factor"))
    classes     <- sapply(df1, class)
    classes     <- sapply(classes, paste, collapse = " ")
    types       <- table(classes)
    type_tibble <- tibble(type = names(types), n = as.integer(types))
    
    # summarise column types into df1
    out <- left_join(type_spine, type_tibble, by = "type") %>%
      replace_na(list(n = 0))     %>%
      mutate(prop = n / ncol(df1)) %>% 
      arrange(desc(prop))         %>% 
      filter(prop > 0)            
    return(out)
  } else {
    s1 <- report_types(df1,  type = type) %>% dplyr::rename(n_1 = n, prop_1 = prop)
    s2 <- report_types(df2, type = type) %>% dplyr::rename(n_2 = n, prop_2 = prop)
    sjoin <- dplyr::full_join(s1, s2, by = "type") %>% 
      replace_na(list(n_1 = 0, n_2 = 0, prop_1 = 0, prop_2 = 0))
    return(sjoin)
  }
}






