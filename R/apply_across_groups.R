apply_across_groups <- function(df, fn, ...){
  # names of the groups
  grp_nms  <- attr(df, "groups") %>% select(1)
  cnm      <- colnames(grp_nms)[1]
  out_nest <- df %>% nest()
  out_list <- lapply(out_nest$data, fn, ...)
  grp_nms <- data.frame(rep(unlist(grp_nms), each = nrow(out_list[[1]])))
  colnames(grp_nms) <- cnm
  out <- bind_cols(grp_nms, bind_rows(out_list)) %>% as_tibble()
  return(out)
}