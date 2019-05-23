plot_deprecated <- function(out){
  message("The `show_plot = TRUE` is deprecated and will be removed in a future version.  The `show_plot()` function should be used instead.  For more info, check out the help file ?show_plot()")
  out %>% show_plot()
}

