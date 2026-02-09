#' @importFrom dplyr arrange
#' @importFrom dplyr last_col
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom tidyr unnest

apply_across_groups <- function(df, fn, ...){
  # split the grouped data into list
  out_nest <- df %>% nest()
  # group_nms nested
  grp_nms  <- out_nest %>% select(-last_col())
  # apply inspect_ over list elements
  grp_nms$out_list <- lapply(out_nest$data, fn, ...)
  # repeat each row of the grouping columns
  out <- unnest(grp_nms, cols = c('out_list'))
  # order result by group
  out <- out %>% arrange(1)
  # return output
  return(out)
}