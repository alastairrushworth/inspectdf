#' @importFrom utils object.size
size_up <- function(thing, form = F){x <- object.size(thing); if(form){return(format(x, standard = "auto", unit = "auto", digits = 2L))} else {return(x)}}
