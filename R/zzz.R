.onLoad <- function(libname = find.package("inspectdf"), pkgname = "inspectdf"){

  # CRAN Note avoidance for magrittr dot pronoun
  # The dot (.) is a special magrittr symbol used in a few pipeline contexts
  # where it cannot be easily eliminated without significant code restructuring
  if(getRversion() >= "2.15.1")
    utils::globalVariables(".")
  invisible()
}
