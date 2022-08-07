#' Summary and comparison of column types
#'
#' @description For a single dataframe, summarise the column types.  If two 
#' dataframes are supplied, compare column type composition of both dataframes.  
#'
#' @param df1 A dataframe.
#' @param df2 An optional second dataframe for comparison.  
#' @param compare_index Whether to check column positions as well as types when comparing dataframes.  
#' Defaults to \code{FALSE}.
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
#'   in each of \code{df1} and \code{df2}
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

inspect_types <- function(df1, df2 = NULL, compare_index = FALSE){
  # perform basic column check on dataframe input
  input_type <- check_df_cols(df1, df2)
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
    # number of columns
    ncl         <- ncol(df1)
    # sweep out column types
    classes     <- sapply(df1, class)
    # if more than one class per column - paste together into single string
    classes     <- sapply(classes, paste0, collapse = " ")
    # get column names by type
    nms_cls     <- tibble(
      pos = 1:ncl, 
      nms = names(df1), 
      cls = classes
    ) %>% 
      arrange(cls)
    nms_lst     <- split(nms_cls, nms_cls$cls)
    nms_lst     <- lapply(nms_lst, function(v) {dd <- v$nms; names(dd) <- v$pos; dd})
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
  } else {
    # inspect types for first df
    s1  <- inspect_types(df1) %>% select(-pcnt)
    colnames(s1)[2] <- 'cnt_1'
    # inspect types for second df
    s2  <- inspect_types(df2) %>% select(-pcnt)
    colnames(s2)[2] <- 'cnt_2'
    # join together
    out <- full_join(s1, s2, by = "type") 
    column_tibble <- function(x, y){
      part_1 <-  if(is.null(x)) {
        tibble(col_name = character(), data_arg = character())
      } else {
        tibble(col_name = x, data_arg = df_names$df1)
      }
      part_2 <-  if(is.null(y)) {
        tibble(col_name = character(), data_arg = character())
      } else {
        tibble(col_name = y, data_arg = df_names$df2)
      }
      bind_rows(part_1, part_2)
    }
    out$columns <- mapply(column_tibble, x = out$col_name.x, y = out$col_name.y, SIMPLIFY = FALSE)
    # create an log column flagging issues 
    if(compare_index){
      log_logical <- as.integer(mapply(identical, x = out$col_name.x, y = out$col_name.y)) + 1
    } else{
      sort_check  <- function(x, y){
        ifelse(length(x) == 0 | length(y) == 0, FALSE, 
               ifelse(length(x) != length(y), FALSE, all(sort(x) == sort(y))))
      }
      log_logical <- as.integer(mapply(sort_check, x = out$col_name.x, y = out$col_name.y)) + 1
    }
    out$equal     <- c('\u2718', '\u2714')[log_logical]
    s1$col_name <- lapply(s1$col_name, function(v) tibble(position = names(v), col_name = v))
    s2$col_name <- lapply(s2$col_name, function(v) tibble(position = names(v), col_name = v))
    a           <- s1 %>% select(type, col_name) %>% unnest(col_name)
    b           <- s2 %>% select(type, col_name) %>% unnest(col_name)
    
    drudge <- full_join(a, b, by = 'col_name') %>%
      mutate(type_diff = case_when(
        type.x != type.y ~ paste0(
          df_names$df1, '::', col_name, ' ~ ',
          type.x, ' <!> ', df_names$df2, '::', 
          col_name, ' ~ ', type.y, ''
        )
      )) %>%
      mutate(missing_col = case_when(
        is.na(type.x) ~ paste0(
          df_names$df2, '::',
          col_name, ' ~ ', type.y, 
          '  missing from ', 
          df_names$df1
        ), 
        is.na(type.y) ~ paste0(
          df_names$df1, '::',
          col_name, ' ~ ', type.x, 
          '  missing from ', 
          df_names$df2
        )
      )) 
    names(drudge$missing_col) <- drudge$col_name
    names(drudge$type_diff)   <- drudge$col_name
    drudge_nest <- bind_rows(
      drudge %>% select(type = type.x, comment = type_diff), 
      drudge %>% select(type = type.y, comment = type_diff), 
      drudge %>% select(type = type.x, comment = missing_col), 
      drudge %>% select(type = type.y, comment = missing_col)
    ) %>%
      filter(!is.na(comment), !is.na(type)) %>%
      nest(issues = comment)
    
    out <- out %>%
      left_join(drudge_nest, by = 'type') %>% 
      select(-matches('col_name')) %>% 
      replace(is.na(.), 0) %>%
      mutate(issues = lapply(issues, function(v) v$comment)) %>%
      select(type, equal, cnt_1, cnt_2, columns, issues)
  }
  # attach attributes required for plotting
  attr(out, "type") <- list(method = "types", input_type = input_type)
  attr(out, "df_names") <- df_names
  return(out)
}






