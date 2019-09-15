apply_across_groups <- function(df, fn, ...){
  # grouping columns
  grp_nms  <- attr(df, "groups") %>% select(-ncol(.))
  # split the grouped data into list
  out_nest <- df %>% nest()
  # apply inspect_ over list elements
  grp_nms$out_list <- lapply(out_nest$data, fn, ...)
  # repeat each row of the grouping columns
  out <- unnest(grp_nms, cols = c('out_list'))
  # return output
  return(out)
}