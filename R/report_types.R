#' Report column types of a data frame 
#'
#' @param df A data frame
#' @param type Character specificying report output type.  Default \code{type = "df"} causes report to be returned as a tibble.   \code{type = "console"} causes report to be returned directly to the console.
#' @return Prints the proportion of columns with each type.
#' @examples
#' report_types(starwars)

report_types <- function(df, type = "df"){
  # perform basic column check on dataframe input
  check_df_cols(df)
  # number of columns
  ncl         <- ncol(df)
  # possible types to look out for
  type_spine  <- tibble(type = c("logical", "integer", "numeric", "character", 
                                 "factor",  "list",    "matrix",  "data.frame", 
                                 "ordered factor"))
  classes     <- sapply(df, class)
  classes     <- sapply(classes, paste, collapse = " ")
  types       <- table(classes)
  type_tibble <- tibble(type = names(types), n = as.integer(types))

  # summarise column types into df
  out <- left_join(type_spine, type_tibble, by = "type") %>%
    replace_na(list(n = 0))     %>%
    mutate(prop = n / ncol(df)) %>% 
    arrange(desc(prop))         %>% 
    filter(prop > 0)            
  if(type == "df"){
    return(out)
  } 
  if(type == "console"){
    # print title text
    console_title(paste(ncl, " columns composed of types:", sep = ""))
    # print console chart
    out %>% dot_bars_composition 
    # invisibly return the df for further summaries
    invisible(df)
  }
}






