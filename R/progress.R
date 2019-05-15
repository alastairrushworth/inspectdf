#' @importFrom progress progress_bar

start_progress <- function(total, prefix){
  progress_bar$new(
    format = paste0(prefix, " (:iter/:total):  :what"),
    total = total, clear = TRUE, width = 80)
}
update_progress <- function(bar, iter, total, what){
  bar$tick(tokens = list(what = what, iter = iter, total = total))
}
