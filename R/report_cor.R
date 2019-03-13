#' Report the Pearson's correlation coefficient for each pair of numeric columns
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for making comparisons of correlation coefficients.  
#' Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} 
#' prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  
#' Default is \code{FALSE}.
#' @param alpha Alpha level for performing significance tests.  Defaults to 0.05.
#' @param absolute Logical flag indicating whether to plot correlations on an absolute scale.  
#' Note that this is just a display option and all tests and comparisons occur on the original correlation 
#' scale regardless of the value of this flag. 
#' @return Return a \code{tibble} containing the columns \code{col_1}, \code{col_2} and 
#' \code{pair}, \code{corr}, \code{p_value}, \code{lower} and \code{upper}.  
#' @export
#' @details The tibble contains only the upper triangle of the correlation matrix.  The tibble is sorted by 
#' descending absolute value in the \code{corr} column.  The \code{p_value} corresponds 
#' to the null hypothesis that the correlation coefficient equals 0.  The \code{lower} 
#' and \code{upper} columns correspond to the lower and upper bounds of a confidence 
#' interval for the value in \code{corr} at the level specified by \code{alpha}.
#' 
#' When the second data frame \code{df2} is specified, correlations are tabulated for both data frames.  
#' Where a pair of numeric columns with the same names appear in both data frames, the entry in the 
#' \code{p_value} column corresponds to the null hypothesis that the two coefficients are equal.  
#' @examples
#' data("starwars", package = "dplyr")
#' # correlations in numeric columns
#' report_cor(starwars)
#' # compare correlations with a different data frame
#' report_cor(starwars, starwars[1:10, ], show_plot = TRUE)
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

report_cor <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE, 
                       alpha = 0.05, absolute = TRUE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  # filter to only the numeric variables
  df_numeric <- df1 %>% 
    select_if(is.numeric)
  # if only a single df input
  if(is.null(df2)){
    # calculate correlation coefficients
    if(ncol(df_numeric) > 1){
      # get correlation coefficients for numeric pairs
      suppressWarnings(cor_df <- cor_test_1(df_numeric, alpha = alpha))
      # return top strongest if requested
      out <- cor_df %>% 
        slice(1:min(top, nrow(.))) 
      # return plot if requested
      if(show_plot) plot_cor_1(out, df_names = df_names,
                               absolute = absolute)
      # return dataframe of correlations
      return(out %>% select(-pair))
    } else {
      # return empty dataframe 
      return(tibble(col_1 = character(), 
                    col_2 = character(), 
                    corr = numeric()))
    } 
  } else {
    # stats for df1
    s1 <- report_cor(df1, top = top, show_plot = F) %>% 
      select(col_1, col_2, corr) %>% 
      rename(corr_1 = corr)
    # stats for df2
    s2 <- report_cor(df2, top = top, show_plot = F) %>% 
      select(col_1, col_2, corr) %>% 
      rename(corr_2 = corr)
    # join the two
    cor_tab <- full_join(s1, s2, by = c("col_1", "col_2"))
    # add p_value for test of difference between correlation coefficients
    cor_tab$p_value <- cor_test(cor_tab$corr_1, cor_tab$corr_2, 
                                n_1 = nrow(df1), n_2 = nrow(df2))
    # generate plot if requested
    if(show_plot) plot_cor_2(cor_tab, absolute = absolute, 
                             alpha = alpha, df_names = df_names)
    # return the df
    return(cor_tab)
  }
}
