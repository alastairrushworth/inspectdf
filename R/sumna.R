sumna <- function(v){
  if(any(class(v) %in% c("numeric", "character", "integer", "logical", "factor"))){
    if(any(class(v) == "numeric"))   nasum <- na_numeric(v)
    if(any(class(v) == "character")) nasum <- na_character(v)
    if(any(class(v) == "integer"))   nasum <- na_integer(v)
    if(any(class(v) == "factor"))    nasum <- na_integer(v)
    if(any(class(v) == "logical"))   nasum <- na_logical(v)
  } else {
    nasum <- sum(is.na(v))
  }
  return(nasum)
}

