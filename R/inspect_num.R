#' Summarise and compare the numeric variables within one or two dataframes
#'
#' @param df1 A dataframe.
#' @param df2 An optional second dataframe for comparing categorical levels.  
#' Defaults to \code{NULL}.
#' @param breaks Integer number of breaks used for histogram bins, passed to 
#' \code{graphics::hist()}.  Defaults to 20.
#' @param include_int Logical flag, whether to include integer columns in numeric summaries.  
#' Defaults to \code{TRUE}.
#' @param show_plot (Deprecated) Logical flag indicating whether a plot should be shown.  
#' Superseded by the function \code{show_plot()} and will be dropped in a future version.
#' \code{hist(..., breaks)}.  See \code{?hist} for more details. 
#' @return A \code{tibble} containing statistical summaries of the numeric 
#' columns of \code{df1}, or comparing the histograms of \code{df1} and \code{df2}.
#' @details 
#' If only \code{df1} is specified, \code{inspect_num()} returns a tibble with columns
#' \itemize{
#'   \item \code{col_name}, a character vector containing the column names in \code{df1}
#'   \item \code{min}, \code{q1}, \code{median}, \code{mean}, \code{q3}, \code{max} and 
#'   \code{sd}: the minimum, lower quartile, median, mean, upper quartile, maximum and 
#'   standard deviation for each numeric column.
#'   \item \code{pcnt_na}, the percentage of each numeric feature that is missing
#'   \item \code{hist}, a named list of tibbles containing the relative frequency of values in a 
#'   falling in bins determined by \code{breaks}.
#' }
#' If both \code{df1} and \code{df2} are specified, the tibble has columns
#' \itemize{
#'   \item \code{col_name} character vector containing the column names in \code{df1}
#'   and \code{df2}
#'   \item \code{hist_1}, \code{hist_2} list column for histograms of each of \code{df1} and \code{df2}.
#'   Where a column appears in both dataframe, the bins used for \code{df1} are reused to 
#'   calculate histograms for \code{df2}.
#'   \item{jsd} numeric column containing the Jensen-Shannon divergence.  This measures the 
#'   difference in distribution of a pair of binned numeric features.  Values near to 0 indicate
#'   agreement of the distributions, while 1 indicates disagreement.
#'   \item{fisher_p} p-value corresponding to Fisher's exact test.  A small p indicates 
#'   evidence that the two histograms are actually different.
#' }
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' # show summary statistics for starwars
#' inspect_num(starwars)
#' # with a visualisation too - try to limit number of bins
#' inspect_num(starwars, breaks = 10)
#' # compare two data frames
#' inspect_num(starwars, starwars[-c(1:10), ], breaks = 10)
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
#' @importFrom magrittr %>%
#' @importFrom graphics hist
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom utils tail

inspect_num <- function(df1, df2 = NULL, breaks = 20, include_int = TRUE, show_plot = FALSE){

  # fish out breaks_seq, if supplied
  breakseq <- attr(df1, "breakseq")
  # perform basic column check on dataframe input
  check_df_cols(df1, df2)
  # capture the data frame names
  df_names <- get_df_names()
  if(is.null(df2)){
    # pick out numeric features
    df_num <- df1 %>% select_if(is.numeric)
    if(!include_int) df_num <- df_num %>% select_if(is.double)
    n_cols <- ncol(df_num)
    # calculate summary statistics for each
    if(n_cols > 0){
      # columns names from df_num
      names_vec <- colnames(df_num)
      # tibble determining breaks to use
      breaks_tbl <- tibble(col_name = names_vec) 
      # join to the breaks argument if supplied
      if(!is.null(breakseq)){
        breaks_tbl <- left_join(breaks_tbl, breakseq, by = "col_name")
      } else {
        # if not supplied, create placeholder list of NULLs
        breaks_tbl$breaks <- lapply(as.list(1:nrow(breaks_tbl)), function(xc) NULL)
      }
      # initiate progress bar and loop
      pb <- start_progress(prefix = " Column", total = n_cols)
      # empty list for histograms and stats
      breaks_tbl$hist <- stats_list <- vector("list", length = nrow(breaks_tbl))
      for(i in 1:nrow(breaks_tbl)){
        # extract column
        col_nm <- breaks_tbl$col_name[i]
        col_i  <- df_num[[col_nm]]
        # loop over the breaks_tbl and generate histograms, suppressing plotting
        # if breaks already exist, then use them, otherwise create new breaks
        update_progress(bar = pb, iter = i, total = nrow(breaks_tbl), 
                        what = names_vec[i])
        # check first whether the column is completely missing
        if(any(!is.na(col_i))){
          # use the summary function to sweep out statistics
          get_stats <- function(vec, col_nm){
            tibble(col_name = col_nm,
                   min = min(vec, na.rm = T), 
                   q1 = quantile(vec, 0.25, na.rm = T),
                   median = median(vec, na.rm = T),
                   mean = mean(vec, na.rm = T),
                   q3 = quantile(vec, 0.75, na.rm = T),
                   max = max(vec, na.rm = T),
                   sd = sd(vec, na.rm = T),
                   pcnt_na = 100 * mean(is.na(vec)))
          }
          stats_list[[i]] <- get_stats(col_i, col_nm)
          # substract out mean to avoid numerical issues
          brks_null <- is.null(breaks_tbl$breaks[[i]])
          hist_i    <- suppressWarnings(
            hist(col_i, plot = FALSE, right = FALSE,
                 breaks = if(brks_null) breaks else {breaks_tbl$breaks[[i]]})
          )
          # extract basic info for constructing hist
          breaks_tbl$hist[[i]] <- prop_value(hist_i)
        } else {
          breaks_tbl$hist[[i]] <- tibble(value = NA, prop = 1)
          stats_list[[i]] <- tibble(col_name = col_nm, min = NA, q1 = NA, median = NA,
                                    mean = NA, q3 = NA, max = NA, sd = NA, pcnt_na = 100)
        }
      } 
      stats_df <- bind_rows(stats_list)
      # ensure the histogram has a min and max breaks & join back to df_num_sum
      out <- left_join(stats_df, breaks_tbl, by = "col_name") %>% 
        select(-breaks)
      # add feature names to the list
      names(out$hist) <-  as.character(out$col_name)
      # attach attributes required for plotting
      attr(out, "type") <- list(method = "num", 1)
      attr(out, "df_names") <- df_names
    } else {
      out <- tibble(col_name = character(), min = numeric(), 
                    q1 = numeric(), median = numeric(), 
                    mean = numeric(), q3 = numeric(),
                    max = numeric(), sd = numeric(), 
                    pcnt_na = numeric(), hist = list())
    }
  } else {
    # get histogram and summaries for first df
    s1 <- inspect_num(df1, breaks = breaks, include_int = include_int) %>% 
      select(col_name, hist)
    # extract breaks from the above and as an attribute
    attr(df2, "breakseq") <- tibble(col_name = s1$col_name, 
                                    breaks = lapply(s1$hist, get_break))
    # get new histograms and summary stats using breaks from s1
    s2 <- inspect_num(df2, include_int = include_int) %>% select(col_name, hist)
    out <- full_join(s1, s2, by = "col_name")
    # calculate js-divergence and fisher p-value
    out <- out %>%
      mutate(jsd = js_divergence_vec(hist.x, hist.y)) %>%
      mutate(fisher_p = fisher(hist.x, hist.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      select(col_name, hist_1 = hist.x, hist_2 = hist.y,  jsd, fisher_p)
    # attach attributes required for plotting
    attr(out, "type") <- list(method = "num", 2)
    attr(out, "df_names") <- df_names
  }
  if(show_plot) plot_deprecated(out)
  return(out)
}









