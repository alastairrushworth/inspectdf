dot_bars_na <- function(sdf, text = ""){
  perc_vals  <- paste(round(sdf$prop * 100, 0), "% ", sep = "")
  perc       <- str_pad(perc_vals, width = max(nchar(perc_vals)), side = "left", pad = " ")
  total_bars <- 30 
  nbars      <- round(sdf$prop * total_bars, 0)
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  
  # table header
  bar_text   <- str_pad("% column missing", width = 30, pad = " ", side = "both")
  perc_text  <- str_pad("(%)", width = max(nchar(perc_vals)) - 1, pad = " ", side = "left")
  num_text   <- "Column"
  cat(paste("     ", bar_text, " "), perc_text, num_text, "\n   ", paste(rep("-", 56), collapse = ""), "\n")
  
  for(i in 1:length(sdf$prop)){
    cat("    \U2022 ")
    cat(red(bar_left[i]))
    cat(silver(bar_right[i]))
    cat(" \U2022")
    cat(green(paste(" ", perc[i], sdf$names[i], sep = "")))
    cat("\n")
  }
}

dot_bars_ass <- function(sdf, text = ""){
  total_bars <- 30 
  nbars      <- round(abs(sdf$ass) * total_bars, 0)
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  cor_vals   <- str_pad(as.character(format(round(sdf$ass, 3)), nsmall = 3), width = 5, side = "right", pad = " ")
  
  # table header
  bar_text   <- str_pad("Goodman and Kruskal's tau", width = 30, pad = " ", side = "both")
  perc_text  <- str_pad("(\U03C4)", width = 6, pad = " ", side = "right")
  num_text   <- str_pad("Column pair", width = 4, pad = " ", side = "right")
  cat(paste("     ", bar_text, " "), perc_text, num_text, "\n   ", paste(rep("-", 56), collapse = ""), "\n")
  
  
  
  for(i in 1:length(cor_vals)){
    cat("    \U2022 ")
    cat(red(bar_left[i]))
    cat(silver(bar_right[i]))
    cat(" \U2022")
    cat(green(paste(" ", cor_vals[i], " ", sdf$pair[i], sep = "")))
    cat("\n")
  }
}


dot_bars_cor <- function(sdf, text = ""){
  total_bars <- 30 
  nbars      <- round(abs(sdf$cor) * total_bars, 0)
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  cor_vals   <- str_pad(gsub("[+]-", "-", paste("+", as.character(round(sdf$cor, 3)), sep = "")), 
                        width = 6, side = "right", pad = "0")
  
  # table header
  bar_text   <- str_pad("Absolute coefficient (|\U03C1|)", width = 30, pad = " ", side = "both")
  perc_text  <- str_pad("(\U03C1)", width = 6, pad = " ", side = "right")
  num_text   <- str_pad("Column pair", width = 4, pad = " ", side = "right")
  cat(paste("     ", bar_text, " "), perc_text, num_text, "\n   ", paste(rep("-", 56), collapse = ""), "\n")
  
  
  
  for(i in 1:length(cor_vals)){
    cat("    \U2022 ")
    cat(red(bar_left[i]))
    cat(silver(bar_right[i]))
    cat(" \U2022")
    cat(green(paste(" ", cor_vals[i], " ", sdf$pair[i], sep = "")))
    cat("\n")
  }
}

dot_bars_space <- function(sdf, text = ""){
  total_bars <- 30 
  nbars      <- round(abs(sdf$prop) * total_bars, 0)
  perc       <- str_pad(paste(round(sdf$prop * 100, 0), "% ", sep = ""), width = 4, side = "right", pad = " ")
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- red(rep_bar_n(nbars, chr = "\U25A0"))
  bar_right  <- silver(rep_bar_n(nbars_c, chr = "\U00B7"))
  
  # table header
  bar_text   <- str_pad("% of total data size", width = 30, pad = " ", side = "both")
  perc_text  <- str_pad("(%)", width = 3, pad = " ", side = "right")
  num_text   <- str_pad("Column", width = 4, pad = " ", side = "right")
  cat(paste("     ", bar_text, " "), perc_text, num_text, "\n   ", paste(rep("-", 56), collapse = ""), "\n")
  
  # table contents
  to_print   <- cbind("    \U2022 ", bar_left, bar_right, " \U2022", green(paste(" ", perc, sdf$names, sep = "")), "\n")
  to_print   <- apply(to_print, 1, paste, collapse = "")
  for(i in 1:length(to_print)) cat(to_print[i])
}

dot_bars_imbalance <- function(sdf, text = ""){
  total_bars <- 30 
  nbars      <- round(abs(sdf$prop) * total_bars, 0)
  perc_vals  <- paste(round(sdf$prop * 100, 0), "% ", sep = "")
  perc       <- str_pad(perc_vals, width = max(nchar(perc_vals)), side = "left", pad = " ")
  nbars_c    <- 30 - nbars  
  rep_bar    <- function(n, chr)  paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(nbars_c, chr = "\U00B7")
  nms        <- str_pad(sdf$names, width = max(nchar(sdf$names)) + 1, side = "right", pad = " ")
  
  # table header
  bar_text   <- str_pad("% Single dominant value", width = 30, pad = " ", side = "both")
  perc_text  <- str_pad("(%)", width = 3, pad = " ", side = "right")
  num_text   <- str_pad("Column", width = max(nchar(sdf$names)), pad = " ", side = "right")
  val_text   <- str_pad("Value",  width = 6, pad = " ", side = "right")
  cat(paste("     ", bar_text, " "), perc_text, num_text, val_text, "\n   ", paste(rep("-", 56), collapse = ""), "\n")
  
  # table contents
  to_print   <- cbind("    \U2022 ", red(bar_left), silver(bar_right), 
                      " \U2022", green(paste(" ", perc, nms, sdf$value, sep = "")), "\n")
  to_print   <- apply(to_print, 1, paste, collapse = "")
  for(i in 1:length(to_print)) cat(to_print[i])
}

dot_bars_composition <- function(sdf, text = ""){
  total_bars <- 30
  nbars      <- round(sdf$prop * total_bars, 0)
  predash    <- cumsum(c(0, nbars[-length(nbars)]))
  postdash   <- 30 - nbars - predash
  rep_bar    <- function(n, chr) paste(rep(chr, n), collapse = "")
  rep_bar_n  <- function(ns, chr) sapply(ns, rep_bar, chr = chr)
  bar_left   <- rep_bar_n(predash, chr = "\U00B7")
  bar_mid    <- rep_bar_n(nbars, chr = "\U25A0")
  bar_right  <- rep_bar_n(postdash, chr = "\U00B7")
  perc       <- str_pad(paste(round(sdf$prop * 100, 0), "% ", sep = ""), width = 4, side = "left", pad = " ")
  bar_text   <- str_pad("% of columns", width = 30, pad = " ", side = "both")
  perc_text  <- str_pad("(%)", width = max(nchar(perc))  - 1, pad = " ", side = "right")
  num_text   <- str_pad("(#)", width = max(nchar(as.character(sdf$n))) + 2, pad = " ", side = "right")
  type_text  <- "Type"
  cat(paste("     ", bar_text, "  "), perc_text, 
      num_text,  type_text, "\n", "  ",
       paste(rep("-", 56), collapse = ""), "\n")
  for(i in 1:length(sdf$prop)){
    cat(paste("    \U2022 "))
    cat(silver(bar_left[i]))
    cat(red(bar_mid[i]))
    cat(silver(bar_right[i]))
    cat(paste(" \U2022"))
    cat(green(paste("  ", perc[i], "(", sdf$n[i], ") ",  sdf$type[i], sep = "")))
    cat("\n")
  }
}


