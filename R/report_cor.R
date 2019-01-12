#' Report the Pearson's correlation coefficient for each pair of numeric columns
#'
#' @param df1 A data frame containing numeric columns
#' @param df2 An optional second data frame for comparing correlation coefficients with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @param alpha Alpha level for performing significance tests.  Defaults to 0.05.
#' @param absolute Logical flag indicating whether to plot correlations on an absolute scale.  Note that this is just a display option and all tests and comparisons occur on the original scale regardless of this flag. 
#' @return Return a \code{tibble} containing the columns \code{col_1}, \code{col_2} and \code{pair} and \code{corr}.  The report contains only the upper triangle of the correlation matrix.  The tibble is sorted by descending absolute value in the \code{corr} column.
#' @export
#' @details When the second data frame \code{df2} is specified, correlations are tabulated for both data frames, and where a pair of numeric columns with the same names appear in both, a p-value is provided which test tests whether their correlations coefficients are equal.
#' @examples
#' data("starwars", package = "dplyr")
#' report_cor(starwars)
#' report_cor(starwars, starwars[1:10, ], show_plot = T)
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr distinct
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_blank
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom magrittr %>%
#' @importFrom tibble tibble

report_cor <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE, alpha = 0.05, absolute = T){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  # filter to only the numeric variables
  df_numeric <- df1 %>% select_if(is.numeric)
  # remove anything that is constant
  df_numeric <- df_numeric %>% select(-which(sapply(df_numeric, sd) == 0))
  
  if(is.null(df2)){
    # calculate correlation coefficients
    if(ncol(df_numeric) > 0){
      # get correlation coefficients for numeric pairs
      cor_df <- cor_test_1(df_numeric)
      # return top strongest if requested
      out <- cor_df %>% slice(1:min(top, nrow(.))) 
      # return plot if requested
      if(show_plot){
        # preprocess data a bit
        out_plot <- out %>% 
          mutate(pair = factor(pair, levels = as.character(pair)),
                 sign = as.factor(c("Negative", "Positive")[as.numeric(corr > 0) + 1]))
        if(absolute){
          out_plot$lower <- ifelse(out_plot$corr < 0, -out_plot$lower,
                                   out_plot$lower)
          out_plot$upper <- ifelse(out_plot$corr < 0, -out_plot$upper, out_plot$upper)
          out_plot$corr <- ifelse(out_plot$corr < 0, -out_plot$corr, 
                                         out_plot$corr)
        }
        
        # generate points and error bars for correlations
        plt <- ggplot(out_plot, aes(x = pair, y = corr, colour = sign)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
          geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black", width = .1) +
          geom_point(size = 3.7, color = "black") + 
          geom_point(size = 3) +
          coord_flip() + 
          labs(x = "", 
               title =  paste0("Pearson correlation of numeric columns in df::", df_names$df1), 
               subtitle = bquote("Error bars show 95% confidence regions for \u03C1"))
        if(absolute){
          plt <- plt + 
            guides(colour = guide_legend(title = bquote("\u03C1 Sign"))) + 
            labs(y = bquote("Absolute Pearson correlation (|\u03C1|)"), x = "")
        } else {
          plt <- plt + 
            guides(colour = FALSE) +
            labs(y = bquote("Pearson correlation (\u03C1)"), x = "")
        }

        # print plot
        print(plt)
      }
      # return dataframe of correlations
      return(out)
    } else {
      # return empty dataframe of 
      return(tibble(col_1 = character(), col_2 = character(), 
                    pair = character(), corr = numeric()))
    } 
  } else {
    # stats for df1
    s1 <- report_cor(df1, top = top, show_plot = F) %>% 
      select(col_1, col_2, corr) %>% rename(corr_1 = corr)
    # stats for df2
    s2 <- report_cor(df2, top = top, show_plot = F) %>% 
      select(col_1, col_2, corr) %>% rename(corr_2 = corr)
    # join the two
    cor_tab <- full_join(s1, s2, by = c("col_1", "col_2"))
    # add p_value for test of difference between correlation coefficients
    cor_tab$p_value <- cor_test(cor_tab$corr_1, cor_tab$corr_2, 
                                n_1 = nrow(df1), n_2 = nrow(df2))
    # generate plot if requested
    if(show_plot){
      cor_tab_plot <- cor_tab %>%
        mutate(pair = paste(col_1, col_2, sep = " & ")) %>%
        mutate(pair = factor(pair, levels = as.character(pair))) %>%
        select(-col_1, -col_2) %>% 
        gather(key = "data_frame", value = "corr", -pair, -p_value) %>%
        mutate(sign = as.factor(c("Negative", "Positive")[as.numeric(corr > 0) + 1]))
        # if only showing the absolute correlations
      if(absolute){
        # if showing absolute values, flip the sign of any negative values 
        cor_tab_plot$corr <- ifelse(cor_tab_plot$corr < 0, -cor_tab_plot$corr, 
                                    cor_tab_plot$corr)
      }
      p_val_tab <- cor_tab_plot %>% 
        mutate(is_sig = as.integer(p_value < 0.05) + 1, index = 1:nrow(cor_tab_plot)) %>%
        select(is_sig, index)
      # generate basic plot
      plt <- ggplot(cor_tab_plot, aes(x = as.factor(pair), y = corr, colour = sign, 
                               group = data_frame)) +
        geom_blank() + theme_bw() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank()) +
        geom_rect(
          fill = c("darkorange2", "royalblue1")[p_val_tab$is_sig], alpha = 0.2,
          xmin = p_val_tab$index - 0.4, xmax = p_val_tab$index + 0.4,
          ymin = -2, ymax = 2, linetype = "blank") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
        geom_point(size = 3.7, color = "black") + 
        geom_point(size = 3) +
        coord_flip() + 
        labs(x = "", 
             title =  paste0("Comparison of \u03C1 between df::", df_names$df1, 
                             " and ", df_names$df2),
             subtitle = bquote("Coloured stripes represent significance (blue) or not (orange) of test of equality at \u03B1 = 0.05"))
      # if absolute value requested then label accordingly
      if(absolute){
        plt <- plt + 
          guides(colour = guide_legend(title = bquote("\u03C1 sign"))) + 
          labs(y = bquote("Absolute Pearson correlation (|\u03C1|)"), x = "")
      } else {
        plt <- plt + 
          guides(colour = FALSE) +
          labs(y = bquote("Pearson correlation (\u03C1)"), x = "")
      }
      print(plt)
    }
    return(cor_tab)
  }
}
