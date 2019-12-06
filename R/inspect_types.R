#' Summary and comparison of column types
#'
#' @description For a single dataframe, summarise the column types.  If two 
#' dataframes are supplied, compare column type composition of both dataframes.  
#'
#' @param df1 A dataframe.
#' @param df2 An optional second dataframe for comparison.  
#' @return A tibble summarising the count and percentage of different 
#' column types for one or a pair of data frames.
#' @details 
#' For a \strong{single dataframe}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{type}, a character vector containing the column types in \code{df1}.
#'   \item \code{cnt}, integer counts of each type.
#'   \item \code{pcnt}, the percentage of all columns with each type.
#'   \item \code{col_name}, the names of columns with each type. \cr
#' }
#' For a \strong{pair of dataframes}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{type}, a character vector containing the column types in 
#'   \code{df1} and \code{df2}.
#'   \item \code{cnt_1}, \code{cnt_2}, pair of integer columns containing counts of each type - 
#'   in each of \code{df1} and \code{df2}.
#'   \item \code{pcnt_1}, \code{pcnt_2}, pair of columns containing the percentage of 
#'   columns with each type - the data frame name are appended.
#' }
#' For a \strong{grouped dataframe}, the tibble returned is as for a single dataframe, but where 
#' the first \code{k} columns are the grouping columns.  There will be as many rows in the result 
#' as there are unique combinations of the grouping variables.
#' 
#' @author Alastair Rushworth
#' @seealso \code{\link{show_plot}}
#' 
#' @examples
#' # Load dplyr for starwars data & pipe
#' library(dplyr)
#' 
#' # Single dataframe summary
#' inspect_types(starwars)
#' 
#' # Paired dataframe comparison
#' inspect_types(starwars, starwars[1:20, ])
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
#' @useDynLib inspectdf

inspect_types <- function(df1, df2 = NULL){
  
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
    nms_df      <- tibble(type = names(nms_lst), col_name = nms_lst) 
    # combine with type frequencies
    classes     <- sapply(classes, paste, collapse = " ")
    types       <- table(classes)
    type_tibble <- tibble(type = names(types), cnt = as.integer(types)) 
    # summarise column types into df1
    out <- type_tibble %>%
      mutate(pcnt = 100 * cnt / ncol(df1)) %>% 
      arrange(desc(pcnt))  %>% 
      left_join(nms_df, by = "type") %>%
      filter(pcnt > 0) 
    # attach attributes required for plotting
    attr(out, "type") <- list(method = "types", 1)
    attr(out, "df_names") <- df_names
  } else {
    s1 <- inspect_types(df1) %>% select(-col_name)
    colnames(s1)[2:3] <- paste0(c("cnt_", "pcnt_"), 1)
    s2 <- inspect_types(df2) %>% select(-col_name)
    colnames(s2)[2:3] <- paste0(c("cnt_", "pcnt_"), 2)
    out <- full_join(s1, s2, by = "type") %>% 
      replace(is.na(.), 0)
    # attach attributes required for plotting
    attr(out, "type") <- list(method = "types", 2)
    attr(out, "df_names") <- df_names
  }
  return(out)
}








