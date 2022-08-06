#' Summary and comparison of numeric columns
#' 
#' @description For a single dataframe, summarise the numeric columns.  If two 
#' dataframes are supplied, compare numeric columns appearing in both dataframes.  
#' For grouped dataframes, summarise numeric columns separately for each group.
#'
#' @param df1 A dataframe.
#' @param df2 An optional second dataframe for comparing categorical levels.  
#' Defaults to \code{NULL}.
#' @param breaks Integer number of breaks used for histogram bins, passed to 
#' \code{graphics::hist()}.  Defaults to 20.
#' @param include_int Logical flag, whether to include integer columns in numeric summaries.  
#' Defaults to \code{TRUE}.
#' \code{hist(..., breaks)}.  See \code{?hist} for more details. 
#' @return A \code{tibble} containing statistical summaries of the numeric 
#' columns of \code{df1}, or comparing the histograms of \code{df1} and \code{df2}.
#' @details 
#' For a \strong{single dataframe}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, a character vector containing the column names in \code{df1}
#'   \item \code{min}, \code{q1}, \code{median}, \code{mean}, \code{q3}, \code{max} and 
#'   \code{sd}, the minimum, lower quartile, median, mean, upper quartile, maximum and 
#'   standard deviation for each numeric column.
#'   \item \code{pcnt_na}, the percentage of each numeric feature that is missing
#'   \item \code{hist}, a named list of tibbles containing the relative frequency of values 
#'   falling in bins determined by \code{breaks}.
#' } 
#' For a \strong{pair of dataframes}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, a character vector containing the column names in \code{df1}
#'   and \code{df2}
#'   \item \code{hist_1}, \code{hist_2}, a list column for histograms of each of \code{df1} and \code{df2}.
#'   Where a column appears in both dataframe, the bins used for \code{df1} are reused to 
#'   calculate histograms for \code{df2}.
#'   \item{jsd}, a numeric column containing the Jensen-Shannon divergence.  This measures the 
#'   difference in distribution of a pair of binned numeric features.  Values near to 0 indicate
#'   agreement of the distributions, while 1 indicates disagreement.
#'   \item \code{pval}, the p-value corresponding to a NHT that the true frequencies of histogram bins are equal.
#'   A small p indicates evidence that the the two sets of relative frequencies are actually different.  The test
#'   is based on a modified Chi-squared statistic.
#' }
#' For a \strong{grouped dataframe}, the tibble returned is as for a single dataframe, but where 
#' the first \code{k} columns are the grouping columns.  There will be as many rows in the result 
#' as there are unique combinations of the grouping variables.
#' 
#' @export
#' @author Alastair Rushworth
#' @seealso \code{\link{show_plot}}
#' 
#' @examples
#' # Load dplyr for starwars data & pipe
#' library(dplyr)
#' 
#' # Single dataframe summary
#' inspect_num(starwars)
#' 
#' # Paired dataframe comparison
#' inspect_num(starwars, starwars[1:20, ])
#' 
#' # Grouped dataframe summary
#' starwars %>% group_by(gender) %>% inspect_num()
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
#' @importFrom tidyr replace_na
#' @importFrom utils tail

inspect_num <- function(df1, df2 = NULL, breaks = 20, include_int = TRUE){

  # perform basic column check on dataframe input
  input_type <- check_df_cols(df1, df2)
  # capture the data frame names
  df_names <- get_df_names()
  # list of all supplied colnames
  cnames <- if(is.null(df2)) colnames(df1) else c(colnames(df1), colnames(df2))
  
  # if only a single df input
  if(input_type == "single"){
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
      if(is.list(breaks)){
        breaks_tbl <- 
          tibble(col_name = names(breaks), breaks = breaks) %>%
          full_join(breaks_tbl, ., by = "col_name") %>%
          filter(col_name %in% cnames)
      } else {
        # if not supplied, create placeholder list of NULLs
        breaks_tbl$breaks <- vector('list', length = nrow(breaks_tbl))
      }
      
      # initiate progress bar and loop
      pb <- start_progress(prefix = " Column", total = nrow(breaks_tbl))
      # empty list for histograms and stats
      breaks_tbl$hist <- stats_list <- brks_list <- vector("list", length = nrow(breaks_tbl))
      for(i in 1:nrow(breaks_tbl)){
        # extract column
        col_nm <- breaks_tbl$col_name[i]
        col_i  <- df_num[[col_nm]]
        # loop over the breaks_tbl and generate histograms, suppressing plotting
        # if breaks already exist, then use them, otherwise create new breaks
        update_progress(bar = pb, iter = i, total = nrow(breaks_tbl), 
                        what = names_vec[i])
        # histogram breaks
        brks_null <- is.null(breaks_tbl$breaks[[i]])
        # decide what to pass to hist in terms of breaks
        hist_breaks <- 
          if(brks_null & (!is.list(breaks))){
            breaks[1] 
          } else if(brks_null & (is.list(breaks))){
            20
          } else {
            breaks_tbl$breaks[[i]]
          }
        if(any(!is.na(col_i))){
          # get summary statistics for this column
          stats_list[[i]] <- get_stats(col_i, col_nm)
          hist_i <- hist(col_i, plot = FALSE, right = TRUE, breaks = hist_breaks)
          # extract basic info for constructing hist
          breaks_tbl$hist[[i]] <- prop_value(hist_i)
          brks_list[[i]]       <- hist_i$breaks
        } else {
          brks_list[[i]]       <- list(NA)
          if(length(hist_breaks) > 1){
            hist_i <- hist(col_i, plot = FALSE, right = TRUE, breaks = hist_breaks)
            # extract basic info for constructing hist
            breaks_tbl$hist[[i]] <- prop_value(hist_i)
            brks_list[[i]]       <- hist_i$breaks
          } else{
            breaks_tbl$hist[[i]] <- tibble(value = NA, prop = 1)
          }
          stats_list[[i]]      <- tibble(
            col_name = col_nm, min = NA, q1 = NA, median = NA,
            mean = NA, q3 = NA, max = NA, sd = NA, pcnt_na = 100
          )
        }
      }
      names(brks_list) <- breaks_tbl$col_name
      stats_df <- bind_rows(stats_list)
      # ensure the histogram has a min and max breaks & join back to df_num_sum
      out <- left_join(stats_df, breaks_tbl, by = "col_name") %>% 
        select(-breaks)
      # add feature names to the list
      names(out$hist) <-  as.character(out$col_name)
    } else {
      out <- tibble(
        col_name = character(), min = numeric(), q1 = numeric(), 
        median = numeric(), mean = numeric(), q3 = numeric(),
        max = numeric(), sd = numeric(), pcnt_na = numeric(), hist = list()
      )
      brks_list <- list()
    }
  } 
  if(input_type == "pair"){
    # get histogram and summaries for first df
    s1 <- inspect_num(df1, breaks = breaks, include_int = include_int)
    brks_list <- attr(s1, 'brks_list')
    s1_sub <- s1 %>% select(col_name, hist)
    # get new histograms and summary stats using breaks from s1
    s2 <- inspect_num(df2, breaks = brks_list, include_int = include_int) 
    s2_sub <- s2 %>% select(col_name, hist)
    out <- full_join(s1_sub, s2_sub, by = "col_name")
    # calculate js-divergence and fisher p-value
    out <- out %>%
      mutate(jsd = js_divergence_vec(hist.x, hist.y)) %>%
      mutate(pval = chisq(hist.x, hist.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      select(col_name, hist_1 = hist.x, hist_2 = hist.y,  jsd, pval)
    # add summary stats as attributes
    attr(out, "inspected") <- 
      list(
        df1 = s1 %>% select(-hist), 
        df2 = s2 %>% select(-hist)
      )
    attr(out, "group_lengths") <- tibble(name = c('df1', 'df2'), rows = c(nrow(df1), nrow(df2)))
  }
  if(input_type == "grouped"){
    # get inspect_num on the ungrouped version
    s_ug      <- inspect_num(df1 %>% ungroup)
    brks_list <- attr(s_ug, 'brks_list')
    # create a nested version of df1 -reak into a list
    out_nest <- df1 %>% nest()
    if(is.numeric(out_nest[[1]])) out_nest <- out_nest %>% arrange(.[[1]])
    grp_nms  <- out_nest %>% select(-ncol(.)) %>% ungroup 
    out_list <- vector("list", length = nrow(out_nest))
    # loop over the subcomponents of out_nest
    for(i in 1:nrow(out_nest)){
      out_list[[i]] <- inspect_num(
        out_nest$data[[i]], 
        breaks = brks_list, 
        include_int = include_int
      )
    }
    grp_nms$out_list <- out_list
    out <- unnest(grp_nms, cols = c('out_list'))
    group_df   <- attr(df1, "groups") 
    group_vars <- colnames(group_df %>% select(-.rows)) 
    # get the average value by group - for plotting purposes
    rank_mean_by_group <- out %>%
      group_by(col_name) %>%
      mutate(rank_mean = rank(mean)) %>%
      ungroup %>% group_by(.data[[group_vars]]) %>%
      summarise(rank_mean = mean(rank_mean))
    # combine group lengths with group means - this is set as an attr &
    # used for graphics in show_plot
    group_lengths <- group_df %>%
      mutate(rows = lengths(.rows)) %>% 
      select(-.rows) %>%
      left_join(rank_mean_by_group, by = group_vars)
    attr(out, "group_lengths") <- group_lengths
  }
  attr(out, "type")      <- list(method = "num", input_type = input_type)
  attr(out, "df_names")  <- df_names
  attr(out, "brks_list") <- brks_list
  return(out)
}


# function to sweep out summary statistics from a column
get_stats <- function(vec, col_nm){
  tibble(
    col_name = col_nm, min = min(vec, na.rm = T), 
    q1 = quantile(vec, 0.25, na.rm = T),
    median = median(vec, na.rm = T),
    mean = mean(vec, na.rm = T),
    q3 = quantile(vec, 0.75, na.rm = T),
    max = max(vec, na.rm = T), sd = sd(vec, na.rm = T),
    pcnt_na = 100 * mean(is.na(vec))
  )
}






