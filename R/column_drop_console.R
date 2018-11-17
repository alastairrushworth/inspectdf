column_drop_console <- function(type, names_to_drop = NULL){
  if(is.null(names_to_drop)) names_to_drop <- silver("<none>")
  cat(paste(type, " ", sep = ""))
  cat(paste("\U2022 ", names_to_drop, sep = ""))
  cat("\n")
}