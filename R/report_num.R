#' Report and compare the numeric variables within one or two data frames
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing categorical levels  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @param breaks Optional argument specifying the breaks to use when comparing numeric data frame features.
#' @return If \code{df2 = NULL} then is a \code{tibble} containing the names of numeric columns (\code{col_name}).
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' report_num(starwars)
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

report_num <- function(df1, df2 = NULL, top = NULL, show_plot = F, breaks = NULL, histfn = "FD"){

  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  if(is.null(df2)){
    # pick out numeric features
    df_num <- df1 %>% select_if(is.numeric)
    # calculate summary statistics for each
    if(ncol(df_num) > 1){
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
      if(!is.null(breaks)){
        breaks_tbl <- left_join(breaks_tbl, breaks, by = "col_name")
      } else {
        breaks_tbl$breaks <- as.list(rep(NA, nrow(breaks_tbl)))
      }
      breaks_tbl$hist <- vector("list", length = ncol(df_num))
      # loop over the breaks_tbl and generate histograms, suppress plotting
      for(i in 1:nrow(breaks_tbl)){
        breaks_tbl$hist[[i]] <- hist(unlist(df_num[breaks_tbl$col_name[i]]), plot = F, 
                                     breaks = if(anyNA(breaks_tbl$breaks[[i]])) {histfn} else {breaks_tbl$breaks[[i]]})
      }
      # extract basic info for constructing hist
      breaks_tbl$hist <- lapply(breaks_tbl$hist, prop_value)
      # ensure the histogram has a min and max breaks & join back to df_num_sum
      out <- left_join(df_num_sum, breaks_tbl, by = "col_name") %>% select(-breaks)
      # add feature names to the list
      names(out$hist) <-  as.character(out$col_name)
      # if plot is requested
      if(show_plot){
        out_plot <- out
        for(i in 1:length(out_plot$hist)){
          out_plot$hist[[i]]$col_name <- out_plot$col_name[i]
          diff_nums <- lapply(strsplit(gsub("\\[|,|\\)", "", out_plot$hist[[i]]$value), " "), function(v) diff(as.numeric(v))) %>% unlist %>% unique
          out_plot$hist[[i]]$mid <- lapply(strsplit(gsub("\\[|,|\\)", "", out_plot$hist[[i]]$value), " "), function(v) diff(as.numeric(v))/2 + as.numeric(v)[1]) %>% unlist
          if(is.nan(out_plot$hist[[i]]$mid[1]) | is.infinite(out_plot$hist[[i]]$mid[1])){
            out_plot$hist[[i]]$mid[1] <- out_plot$hist[[i]]$mid[2] - (diff_nums[is.finite(diff_nums)])[1]
          } 
          last_n <- length(out_plot$hist[[i]]$mid)
          if(is.nan(out_plot$hist[[i]]$mid[last_n]) | is.infinite(out_plot$hist[[i]]$mid[last_n])){
            out_plot$hist[[i]]$mid[last_n] <- out_plot$hist[[i]]$mid[last_n - 1] + (diff_nums[is.finite(diff_nums)])[1]
          }
        }
        out_plot <- bind_rows(out_plot$hist)
        plt <- out_plot %>%
          ggplot(aes(x = mid, y = prop)) + 
          geom_col(fill = "blue") + 
          facet_grid(. ~ col_name, scales = "free") + 
          labs(x = "", y = "Probability", 
                        title =  paste0("Histograms of numeric columns in df::", df_names$df1), 
                        subtitle = "")
        # print plot
        print(plt)
      }
      # return df
      return(out)
    } else {
      return(tibble(col_name = character(), min = numeric(), q1 = numeric(), 
                    median = numeric(), mean = numeric(), q3 = numeric(),
                    max = numeric(), sd = numeric(), percent_na = numeric(), hist = list()))
    }
  } else {
    s1 <- report_num(df1, top = top, show_plot = F) %>% select(col_name, mean, sd, hist)
    # extract breaks from the above
    breaks_table <- tibble(col_name = s1$col_name, breaks = lapply(s1$hist, get_break))
    # get new histoggrams and summary stats using breaks from s1
    s2 <- report_num(df2, top = top, breaks = breaks_table, show_plot = F) %>% select(col_name, mean, sd, hist)
    s12 <- full_join(s1, s2, by = "col_name")
    # calculate psi and fisher p-value
    levels_tab <- s12 %>%
      mutate(psi = psi(hist.x, hist.y)) %>%
      mutate(fisher_p = fisher(hist.x, hist.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      rename(levels.x = hist.x, levels.y = hist.y) %>%
      select(-contains("mean"), -contains("sd"))
    if(show_plot){
      out_plot <- levels_tab %>% 
        select(-psi, -fisher_p) 
      # add the variable name to the histograms as an extra column
      for(i in 1:nrow(out_plot)) out_plot$levels.x[[i]]$cname <- out_plot$col_name[i] 
      for(i in 1:nrow(out_plot)) out_plot$levels.y[[i]]$cname <- out_plot$col_name[i] 
      # combine the histograms
      trns_plot <- bind_rows(bind_rows(out_plot$levels.x), 
                            bind_rows(out_plot$levels.y))
      trns_plot$dfn <- rep(unlist(df_names), each = nrow(trns_plot) / 2)
      # drop rows where both dfs are zero
      zro_drop <- trns_plot %>%
        group_by(cname, value) %>%
        summarise(zs = as.integer(sum(prop == 0) == 2)) %>%
        ungroup
      trns_plot <- left_join(trns_plot, zro_drop, 
                             by = c("cname", "value")) %>%
        filter(zs == 0)
      # apply an ordering to the categories
      get_num <- function(st) as.integer(gsub("\\[|\\(", "", unlist(strsplit(st, ","))[1]))
      get_numV <- Vectorize(get_num)
      ord_vals <- trns_plot %>%
        select(value) %>%
        distinct() %>%
        mutate(first_num = get_numV(value)) %>%
        arrange(first_num)
      # generate a heatplot
      plt <- trns_plot %>%
        ggplot(aes(x = dfn, y = factor(value, levels = ord_vals$value), fill = prop)) + 
        geom_tile(colour = "white") + 
        geom_text(aes(label = round(prop * 100, 1)), col = "gray30") + 
        scale_fill_gradient(low = "white", high = "steelblue") + 
        theme(legend.position = "none") +
        labs(x = "", y = "") + 
        facet_wrap(~ cname, scales = "free", ncol = 4)
      print(plt)
    }
    return(levels_tab)
  }
}









