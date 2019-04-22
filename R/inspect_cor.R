#' Summarise and compare Pearson's correlation coefficients for numeric columns in one or two dataframes.
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing correlation 
#' coefficients.  Defaults to \code{NULL}.
#' @param show_plot Logical argument determining whether plot is returned
#' in addition to tibble output.  Default is \code{FALSE}.
#' @param alpha Alpha level for performing tests of correlation coefficient equality.  
#' Defaults to 0.05.
#' @param absolute Logical flag indicating whether to plot correlations on an absolute scale.  
#' Note that this is just a display option and all tests and comparisons occur on the original 
#' correlation scale regardless of this flag. 
#' @return A tibble summarising and comparing the correlations for each numeric column 
#' in one or a pair of data frames.
#' @details When only \code{df1} is specified, a tibble is returned which 
#' contains correlation coefficients 
#' \itemize{
#'   \item \code{col_1}, \code{co1_2} character vectors containing names of numeric columns in \code{df1}.
#'   \item \code{corr} numeric values of Pearson's correlation coefficient.
#'   \item \code{lower}, \code{upper} lower and upper values of the confidence interval for the correlations.
#'   \item \code{p_value} p-value associated with the null hypothesis of 0 correlation, small values 
#'   indicate evidence that the true correlation is not equal to 0.
#' }
#' When both \code{df1} and \code{df2} are specified, the tibble returned performs a comparison of the 
#' correlation coefficients across the dataframes.
#' \itemize{
#'   \item \code{col_1}, \code{co1_2} character vectors containing names of numeric columns in either 
#'   \code{df1} or \code{df2}.
#'   \item \code{corr_1}, \code{corr_2} numeric values of Pearson's correlation coefficient observed in 
#'   \code{df1} and \code{df2}, respectively.
#'   \item \code{p_value} p-value associated with the null hypothesis that the two correlation coefficients 
#'   are the same.  Small values indicate that the true correlation coefficients differ between the two 
#'   dataframes.
#' }
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' # correlations in numeric columns
#' inspect_cor(starwars)
#' # get visualisation with confidence bands
#' inspect_cor(starwars, show_plot = TRUE)
#' # compare correlations with a different data frame
#' inspect_cor(starwars, starwars[1:10, ], show_plot = TRUE)
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

inspect_cor <- function(df1, df2 = NULL, show_plot = FALSE, alpha = 0.05, 
                       absolute = TRUE){
  
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
      out <- cor_df 
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
    s1 <- inspect_cor(df1, show_plot = F) %>% 
      select(col_1, col_2, corr) %>% 
      rename(corr_1 = corr)
    # stats for df2
    s2 <- inspect_cor(df2, show_plot = F) %>% 
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
