#' Summarise and compare Pearson's correlation coefficients for numeric columns in one or two dataframes.
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing correlation 
#' coefficients.  Defaults to \code{NULL}.
#' @param alpha Alpha level for correlation confidence intervals.  Defaults to 0.05.
#' @param with_col Character vector of columns to calculate correlations with.  When set to 
#' the default, \code{NULL}, all pairs of correlations are returned.
#' @param show_plot (Deprecated) Logical flag indicating whether a plot should be shown.  
#' Superseded by the function \code{show_plot()} and will be dropped in a future version.
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
#' 
#' # only show correlations with 'mass' column
#' inspect_cor(starwars, with_col = "mass")
#' 
#' # compare correlations with a different data frame
#' inspect_cor(starwars, starwars[1:10, ])
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
#' @importFrom magrittr %>%
#' @importFrom tibble tibble

inspect_cor <- function(df1, df2 = NULL, with_col = NULL, alpha = 0.05, show_plot = FALSE){
  
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
      # perfom check for 0 variance features, return warning if found
      check_variance(df_numeric)
      # get correlation coefficients for numeric pairs
      suppressWarnings(cor_df <- cor_test_1(df_numeric,
                                            df_name = df_names[[1]], 
                                            with_col = with_col,
                                            alpha = alpha))
      # return top strongest if requested
      pair <- cor_df %>% select(pair) %>% unlist
      out <- cor_df %>% select(-pair)
      
      # attach attributes required for plotting
      attr(out, "type") <- list("cor", 1)
      attr(out, "df_names") <- df_names
      attr(out, "pair") <- pair
    } else {
      # return empty dataframe 
      out <- tibble(col_1 = character(), 
                    col_2 = character(), 
                    corr = numeric())
    } 
  } else {
    # stats for df1
    s1 <- inspect_cor(df1) %>% 
      select(col_1, col_2, corr) %>% 
      rename(corr_1 = corr)
    # stats for df2
    s2 <- inspect_cor(df2) %>% 
      select(col_1, col_2, corr) %>% 
      rename(corr_2 = corr)
    # join the two
    out <- full_join(s1, s2, by = c("col_1", "col_2"))
    # add p_value for test of difference between correlation coefficients
    out$p_value <- cor_test(out$corr_1, out$corr_2, 
                            n_1 = nrow(df1), n_2 = nrow(df2))

    # attach attributes required for plotting
    attr(out, "type")     <- list("cor", 2)
    attr(out, "df_names") <- df_names
  }
  if(show_plot) plot_deprecated(out)
  return(out)
}
