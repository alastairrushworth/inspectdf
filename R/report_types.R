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
    # more than one class - append them
    classes     <- unlist(lapply(classes, function(v) paste(v, collapse = " ")))
    # get column names by type
    nms_cls     <- tibble(nms = names(df1), cls = classes) %>% arrange(cls)
    nms_lst     <- split(nms_cls$nms, nms_cls$cls)
    nms_df      <- tibble(type = names(nms_lst), col_names = nms_lst) 
    # combine with type frequencies
    classes     <- sapply(classes, paste, collapse = " ")
    types       <- table(classes)
    type_tibble <- tibble(type = names(types), cnt = as.integer(types)) 
    # summarise column types into df1
    out <- type_tibble %>%
      replace_na(list(cnt = 0)) %>%
      mutate(pcnt = 100 * cnt / ncol(df1)) %>% 
      arrange(desc(pcnt))  %>% 
      left_join(nms_df, by = "type") %>%
      filter(pcnt > 0) 
    
    # if plot requested then show barplot
    if(show_plot) plot_types_1(out, df_names = df_names)
    # return dataframe
    return(out)
  } else {
    s1 <- report_types(df1, show_plot = F) %>% select(-col_names)
    colnames(s1)[2:3] <- c(paste0("cnt_",  df_names[1]), 
                           paste0("pcnt_", df_names[1]))
    s2 <- report_types(df2, show_plot = F) %>% select(-col_names)
    colnames(s2)[2:3] <- c(paste0("cnt_",  df_names[2]), 
                           paste0("pcnt_", df_names[2]))
    out <- full_join(s1, s2, by = "type") %>% 
      replace(is.na(.), 0)
    # show plot if requested
    if(show_plot) plot_types_2(out, df_names = df_names)
    # return dataframe
    return(out)
  }
}








