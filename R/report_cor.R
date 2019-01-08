#' Report the Pearson's correlation coefficient for each pair of numeric columns
#'
#' @param df1 A data frame containing numeric columns
#' @param df2 An optional second data frame for comparing correlation coefficients with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Return a \code{tibble} containing the columns \code{col_1}, \code{col_2} and \code{pair} and \code{correlation}.  The report contains only the upper triangle of the correlation matrix.  The tibble is sorted by descending absolute value in the \code{correlation} column.
#' @export
#' @details When the second data frame \code{df2} is specified, correlations are tabulated for both data frames, and where a pair of numeric columns with the same names appear in both, a p-value is provided which test tests whether their correlations coefficients are equal.
#' @examples
#' data("starwars", package = "dplyr")
#' report_cor(starwars)
#' report_cor(starwars, starwars[1:10, ])
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
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom magrittr %>%
#' @importFrom tibble tibble

report_cor <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE){
  
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
                 sign = as.factor(c("Negative", "Positive")[as.numeric(correlation > 0) + 1]))
        
        # generate points and error bars for correlations
        plt <- ggplot(out_plot, aes(x = pair, y = correlation, colour = sign)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "lightsteelblue4") + 
          geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black", width = .1) +
          geom_point(size = 3.7, color = "black") + 
          geom_point(size = 3) +
          labs(x = "", y = bquote("Pearson correlation (\u03C1)"), 
                        title =  paste0("Pearson correlation of numeric columns in df::", df_names$df1), 
                        subtitle = bquote("Error bars show 95% confidence regions for \u03C1")) +
          guides(colour = FALSE) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        # print plot
        print(plt)
      }
      # return dataframe of correlations
      return(out)
    } else {
      # return empty dataframe of 
      return(tibble(col_1 = character(), col_2 = character(), 
                    pair = character(), correlation = numeric()))
    } 
  } else {
    s1 <- report_cor(df1, top = top, show_plot = F) %>% 
      rename(correlation_1 = correlation)
    s2 <- report_cor(df2, top = top, show_plot = F) %>% 
      select(pair, correlation_2 = correlation)
    cor_tab <- full_join(s1, s2, by = "pair")
    cor_tab$p_value <- cor_test(cor_tab$correlation_1, cor_tab$correlation_2, 
                                n_1 = nrow(df1), n_2 = nrow(df2))
    return(cor_tab)
  }
}
