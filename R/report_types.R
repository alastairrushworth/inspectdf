#' Report column types in a data frame or compare types in two data frames.
#'
#' @param df1 A data frame.
#' @param df2 An optional second data frame for comparison.  
#' @param show_plot Logical argument determining whether plot is generated 
#' in addition to tibble output.  Default is \code{FALSE}.  
#' @return A tibble summarising the count and percentage of different 
#' column types in one or two data frames.
#' @details When \code{df2 = NULL}, a tibble is returned with the columns: \code{col_type}
#' the types contained in \code{df1}, \code{count} the number of columns with each type
#' and \code{percent} the percentage of columns with each type.
#' 
#' When a second data frame \code{df2} is specified, column types are 
#' tabulated for both data frames to enable comparison of contents.  
#' @examples
#' data("starwars", package = "dplyr")
#' # get tibble of column types for the starwars data
#' report_types(starwars)
#' # get column types and show as barplot
#' report_types(starwars, show_plot = TRUE)
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom tidyr replace_na
#' @useDynLib reporter

report_types <- function(df1, df2 = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
    # number of columns
    ncl         <- ncol(df1)
    # summarise the types
    classes     <- sapply(df1, class)
    classes     <- sapply(classes, paste, collapse = " ")
    types       <- table(classes)
    type_tibble <- tibble(col_type = names(types), count_type = as.integer(types))
    # summarise column types into df1
    out <- type_tibble %>%
      replace_na(list(count_type = 0))  %>%
      mutate(percent = 100 * count_type / ncol(df1)) %>% 
      arrange(desc(percent))  %>% 
      filter(percent > 0) 
    
    # if plot requested then show barplot
    if(show_plot) plot_types_1(out, df_names = df_names)
    # return dataframe
    return(out)
  } else {
    s1 <- report_types(df1, show_plot = F)
    colnames(s1) <- c("col_type", paste0("cnt_", df_names[1]), 
                      paste0("pcnt_", df_names[1]))
    s2 <- report_types(df2, show_plot = F)
    colnames(s2) <- c("col_type", paste0("cnt_", df_names[2]), 
                      paste0("pcnt_", df_names[2]))
    sjoin <- full_join(s1, s2, by = "col_type") %>% 
      replace(is.na(.), 0)
    # show plot if requested
    if(show_plot) plot_types_2(sjoin, df_names = df_names)
    # return dataframe
    return(sjoin)
  }
}








