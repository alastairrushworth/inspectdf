#' Report and compare the numeric variables within one or two data frames
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing categorical levels.  
#' Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. 
#' Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition 
#' to tibble output.  Default is \code{FALSE}.
#' @param breaks Optional argument determining how breaks are constructed for 
#' histograms to use when comparing numeric data frame features.  
#' This takes the same values as \code{hist(..., breaks)}.  See \code{?hist} 
#' for more details. 
#' @param breakseq For internal use.  Argument that accepts a pre-specified set of 
#' break points, default is \code{NULL}.
#' @return A \code{tibble} containing statistical summaries of the numeric 
#' columns of \code{df1}, or comparing the histograms of \code{df1} and \code{df2}.
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' # show summary statistics for starwars
#' report_num(starwars)
#' # show a visualisation too - limit number of bins
#' report_num(starwars, breaks = 10)
#' # compare two data frames
#' report_num(starwars, starwars[-c(1:10), ], breaks = 10, show_plot = T)
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 theme
#' @importFrom magrittr %>%
#' @importFrom graphics hist
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom utils tail

report_num <- function(df1, df2 = NULL, top = NULL, show_plot = F, 
                       breakseq = NULL, breaks = 20){

  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  if(is.null(df2)){
    # pick out numeric features
    df_num <- df1 %>% select_if(is.numeric)
    # calculate summary statistics for each
    if(ncol(df_num) > 0){
      # use the summary function to sweep out statistics
      df_num_sum <- df_num %>% gather(key = "col_name", value = "value") %>%
        group_by(col_name) %>% 
        summarise(min = min(value, na.rm = T), 
                  q1 = quantile(value, 0.25, na.rm = T),
                  median = median(value, na.rm = T), 
                  mean = mean(value, na.rm = T), 
                  q3 = quantile(value, 0.75, na.rm = T), 
                  max = max(value, na.rm = T), 
                  sd = sd(value, na.rm = T), 
                  percent_na = 100 * mean(is.na(value))) %>%
        ungroup
      # tibble determining breaks to use
      breaks_tbl <- tibble(col_name = colnames(df_num)) 
      # join to the breaks argument if supplied
      if(!is.null(breakseq)){
        breaks_tbl <- left_join(breaks_tbl, breakseq, by = "col_name")
      } else {
        breaks_tbl$breaks <- as.list(rep(NA, nrow(breaks_tbl)))
      }
      breaks_tbl$hist <- vector("list", length = ncol(df_num))
      # loop over the breaks_tbl and generate histograms, suppress plotting
      for(i in 1:nrow(breaks_tbl)){
        brks_na <- anyNA(breaks_tbl$breaks[[i]])
        breaks_tbl$hist[[i]] <- hist(unlist(df_num[breaks_tbl$col_name[i]]), plot = F, 
                                     breaks = if(brks_na) breaks else {breaks_tbl$breaks[[i]]}, 
                                     right = FALSE)
      }
      # extract basic info for constructing hist
      breaks_tbl$hist <- lapply(breaks_tbl$hist, prop_value)
      # ensure the histogram has a min and max breaks & join back to df_num_sum
      out <- left_join(df_num_sum, breaks_tbl, by = "col_name") %>% select(-breaks)
      # add feature names to the list
      names(out$hist) <-  as.character(out$col_name)
      # if plot is requested
      if(show_plot) plot_num_1(out, df_names = df_names)
      # return df
      return(out)
    } else {
      return(tibble(col_name = character(), min = numeric(), 
                    q1 = numeric(), median = numeric(), 
                    mean = numeric(), q3 = numeric(),
                    max = numeric(), sd = numeric(), 
                    percent_na = numeric(), hist = list()))
    }
  } else {
    # get histogram and summaries for first df
    s1 <- report_num(df1, top = top, show_plot = F, breaks = breaks) %>% 
      select(col_name, mean, sd, hist)
    # extract breaks from the above
    breaks_table <- tibble(col_name = s1$col_name, 
                           breaks = lapply(s1$hist, get_break))
    # get new histoggrams and summary stats using breaks from s1
    s2 <- report_num(df2, top = top, breakseq = breaks_table, show_plot = F) %>% 
      select(col_name, mean, sd, hist)
    s12 <- full_join(s1, s2, by = "col_name")
    # calculate psi and fisher p-value
    levels_tab <- s12 %>%
      mutate(psi = psi(hist.x, hist.y)) %>%
      mutate(fisher_p = fisher(hist.x, hist.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      rename(levels.x = hist.x, levels.y = hist.y) %>%
      select(-contains("mean"), -contains("sd"))
    # if plot is requested
    if(show_plot) plot_num_2(levels_tab, df_names = df_names)
    # return dataframe
    return(levels_tab)
  }
}









