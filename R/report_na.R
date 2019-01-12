#' Report the proportion of each column containing missing values
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing missing values with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @param alpha Alpha level for performing significance tests.  Defaults to 0.05.
#' @return Return a \code{tibble} containing the columns \code{col_name}, \code{cnt_na} and \code{pcnt_na}. 
#' @export
#' @details When the second data frame \code{df2} is specified, the missingness is tabulated for both data frames, and where a pair of columns are common to both data frames a p-value is calculated for the equivalence of the proportion of missing values.
#' @examples
#' data("starwars", package = "dplyr")
#' report_na(starwars)
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr slice
#' @importFrom magrittr %>%
#' @importFrom tibble tibble

report_na <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE, alpha = 0.05){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  # if ony one df input then report na content
  if(is.null(df2)){
    # find the top 10 with most missingness
    out <- vec_to_tibble(sapply(df1, sumna)) %>%
      mutate(pcnt = 100 * n / nrow(df1)) %>%
      select(col_name = names, cnt_na = n, pcnt) %>%
      arrange(desc(pcnt)) %>%
      slice(1:min(top, nrow(.)))
    # if any missing values then print out
    if(nrow(out) > 0){
      # print plot if requested
      if(show_plot){
        # convert col_name to factor
        out_plot <- out %>% mutate(col_name = factor(col_name, levels = as.character(col_name)))
        # construct bar plot of missingess
        plt <- bar_plot(df_plot = out_plot, x = "col_name", y = "pcnt", fill = "col_name", label = "cnt_na",
                        ttl = paste0("Prevalance of missing values in df::", df_names$df1),
                        sttl = paste0("df::", df_names$df1,  " has ", ncol(df1), " columns, of which ", sum(out_plot$cnt_na > 0), " have missing values"),
                        ylb = "% of column that is NA", rotate = TRUE)
        # add text annotation to plot
        plt <- add_annotation_to_bars(x = out_plot$col_name, y = out_plot$pcnt, z = out_plot$cnt_na, plt = plt)
        print(plt)
      }
      # return summary tibble
      return(out)
    } else {
      # return dataframe of values
      return(tibble(col_name = character(), cnt_na = integer(), pcnt = numeric()))
    }
    if(type == "console") invisible(df1)
  } else {
    s1 <- report_na(df1, top = top, show_plot = F) 
    s2 <- report_na(df2, top = top, show_plot = F)
    na_tab <- full_join(s1, s2, by = "col_name")
    na_tab$p_value <- prop_test(na_1 = na_tab$cnt_na.x, na_2 = na_tab$cnt_na.y, n_1 = nrow(df1), n_2 = nrow(df2))
    colnames(na_tab)[c(3, 5)] <- paste0("pcnt_", df_names)
    colnames(na_tab)[c(2, 4)] <- paste0("cnt_", df_names)
    if(show_plot){
      na_tab_plot <- na_tab %>% 
        select(-starts_with("cnt")) %>% 
        gather(key = "data_frame", value = "pcnt", -col_name, -p_value) %>%
        mutate(data_frame = gsub("pcnt_", "", data_frame))
      na_tab_plot <- na_tab_plot[seq(dim(na_tab_plot)[1],1),]
      p_val_tab <- na_tab_plot %>% 
        mutate(is_sig = as.integer(p_value < 0.05) + 2, index = 1:nrow(na_tab_plot)) %>%
        replace_na(list(is_sig = 1)) %>%
        select(is_sig, index) 
    
      plt <- ggplot(na_tab_plot, aes(x = factor(col_name, levels = as.character(na_tab$col_name)), y = pcnt, colour = data_frame)) +
        geom_blank() + theme_bw() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
        geom_rect(
          fill = c("grey85", "darkorange2", "royalblue1")[p_val_tab$is_sig], alpha = 0.2,
          xmin = p_val_tab$index - 0.4, xmax = p_val_tab$index + 0.4,
          ymin = -100, ymax = 200, linetype = "blank") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
        geom_point(size = 3.7, color = "black") + 
        geom_point(size = 3) +
        coord_flip() + 
        labs(x = "", 
             title =  paste0("Comparison of % missingness between ",
                           df_names$df1, " and ", df_names$df2),
             subtitle = bquote("Coloured stripes represent evidence of inequality (blue) or equality (orange) of missingness at \u03B1 = 0.05")) + 
        guides(colour = guide_legend(title = bquote("Data frame"))) + 
        labs(y = "Percent missing", x = "")
      print(plt)
    }

    
    return(na_tab)
  }
}



