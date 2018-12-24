#' Report and compare the numeric variables within one or two data frames
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing categorical levels  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @param breaks Optional argument specifying the breaks to use when comparing numeric data frame features.
#' @return If \code{df2 = NULL} then is a \code{tibble} containing the names of numericcolumns (\code{col_name}).
#' @examples
#' report_numeric(starwars)

report_numeric <- function(df1, df2 = NULL, top = NULL, show_plot = F, breaks = NULL){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  if(is.null(df2)){
    # pick out numeric features
    df_num <- df1 %>% select_if(is.numeric)
    # calculate summary statistics for each
    if(ncol(df_num) > 1){
      # use the summary function to sweep out statistics
      df_num_sum <- df_num %>% gather(key = "col_name", value = "value") %>%
        group_by(col_name) %>% 
        summarise(min = min(value, na.rm = T), q1 = quantile(value, 0.25, na.rm = T), median = median(value, na.rm = T), 
                  mean = mean(value, na.rm = T), q3 = quantile(value, 0.75, na.rm = T), max = max(value, na.rm = T), 
                  sd = sd(value, na.rm = T), percent_na = 100 * mean(is.na(value))) %>%
        ungroup
      # tibble determining breaks to use
      breaks_tbl <- tibble(col_name = colnames(df_num)) 
      # join to the breaks argument if supplied
      if(!is.null(breaks)){
        breaks_tbl <- left_join(breaks_tbl, breaks, by = "col_name")
      } else {
        breaks_tbl$breaks <- as.list(rep(NA, nrow(breaks_tbl)))
      }
      breaks_tbl$hist <- vector("list", length = ncol(df_num))
      # loop over the breaks_tbl and generate histograms, suppress plotting
      for(i in 1:nrow(breaks_tbl)){
        breaks_tbl$hist[[i]] <- hist(unlist(df_num[breaks_tbl$col_name[i]]), plot = F, 
                                     breaks = if(anyNA(breaks_tbl$breaks[[i]])) {"Sturges"} else {breaks_tbl$breaks[[i]]} )
      }
      # join back to df_num_sum
      df_num_sum <- left_join(df_num_sum, breaks_tbl, by = "col_name") %>% select(-breaks)
      # return df
      return(df_num_sum)
    } else {
      return(tibble::tibble(col_name = character(), min = numeric(), q1 = numeric(), 
                    median = numeric(), mean = numeric(), q3 = numeric(),
                    max = numeric(), sd = numeric(), hist = list()))
    }
  } else {
    s1 <- report_numeric(df1, top = top, show_plot = F) %>% select(col_name, mean, sd, hist)
    # extract breaks from the above
    breaks_table <- tibble::tibble(col_name = s1$col_name, breaks = lapply(s1$hist, function(L) L$breaks))
    # get new histoggrams and summary stats using breaks from s1
    s2 <- report_numeric(df2, top = top, breaks = breaks_table, show_plot = F) %>% select(col_name, mean, sd, hist)
    numeric_tab <- dplyr::full_join(s1, s2, by = "col_name")
    
    # calculate PSI
    numeric_tab$hist.x <- lapply(numeric_tab$hist.x, prop_value)
    numeric_tab$hist.y <- lapply(numeric_tab$hist.y, prop_value)
    levels_tab <- numeric_tab %>%
      mutate(psi = psi(hist.x, hist.y)) %>%
      mutate(chisq = chisq(hist.x, hist.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      mutate(p_value = chisq_p(hist.x, hist.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      rename(levels.x = hist.x, levels.y = hist.y) %>%
      select(-contains("mean"), -contains("sd"))
    return(levels_tab)
  }
}