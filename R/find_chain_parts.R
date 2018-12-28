find_chain_parts <- function() {
  i <- 1
  while(!("chain_parts" %in% ls(envir = parent.frame(i))) && i < sys.nframe()) {
    i <- i + 1
  }
  parent.frame(i)
}