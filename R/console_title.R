console_title <- function(txt){
  # find the width of the input text
  nch        <- max(nchar(txt))
  # pad to maximum length for printing
  txt        <- str_pad(txt, width = nch, side = "right", pad = " ")
  # number of dashes
  dash_nchar <- nch + 4
  dashes     <- paste(rep("-", dash_nchar), collapse = "")
  # dash top
  cat("\n"); cat(dashes); cat("\n")
  # loop over input text strings
  for(i in 1:length(txt)) cat(paste("* ", txt[i], " *\n", sep = ""))
  # dash bottom
  cat(dashes); cat("\n\n")
}
