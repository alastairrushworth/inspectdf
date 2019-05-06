#' @importFrom utils object.size
format_size <- function(size){
  x <- format(size, standard = "auto", unit = "auto", digits = 2L)
  return(x)
}