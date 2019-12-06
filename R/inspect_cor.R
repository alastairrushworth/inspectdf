#' Tidy correlation coefficients for numeric dataframe columns
#' 
#' @description Summarise and compare Pearson, Kendall and Spearman correlations for 
#' numeric columns in one, two or grouped dataframes.
#'
#' @param df1 A data frame. 
#' @param df2 An optional second data frame for comparing correlation 
#' coefficients.  Defaults to \code{NULL}.
#' @param method a character string indicating which type of correlation coefficient to use, one 
#' of \code{"pearson"}, \code{"kendall"}, or \code{"spearman"}, which can be abbreviated.
#' @param alpha Alpha level for correlation confidence intervals.  Defaults to 0.05.
#' @param with_col Character vector of column names to calculate correlations with all other numeric 
#' features.  The default \code{with_col = NULL} returns all pairs of correlations.
#' @return A tibble summarising and comparing the correlations for each numeric column 
#' in one or a pair of data frames.
#' @details When \code{df2 = NULL}, a tibble containing correlation coefficients for \code{df1} is 
#' returned:
#' \itemize{
#'   \item \code{col_1}, \code{co1_2} character vectors containing names of numeric 
#'   columns in \code{df1}.
#'   \item \code{corr} the calculated correlation coefficient.
#'   \item \code{p_value} p-value associated with a test where the null hypothesis is that 
#'   the numeric pair have 0 correlation. 
#'   \item \code{lower}, \code{upper} lower and upper values of the confidence interval 
#'   for the correlations.
#'   \item \code{pcnt_nna} the number of pairs of observations that were non missing for each 
#'   pair of columns.  The correlation calculation used by \code{inspect_cor()} uses only 
#'   pairwise complete observations.  
#' }
#' If \code{df1} has class \code{grouped_df}, then correlations will be calculated within the grouping levels 
#' and the tibble returned will have an additional column corresponding to the group labels.
#' 
#' When both \code{df1} and \code{df2} are specified, the tibble returned contains
#'  a comparison of the correlation coefficients across pairs of columns common to both 
#'  dataframes.
#' \itemize{
#'   \item \code{col_1}, \code{co1_2} character vectors containing names of numeric columns 
#'   in either \code{df1} or \code{df2}.
#'   \item \code{corr_1}, \code{corr_2} numeric columns containing correlation coefficients from
#'   \code{df1} and \code{df2}, respectively.
#'   \item \code{p_value} p-value associated with the null hypothesis that the two correlation 
#'   coefficients are the same.  Small values indicate that the true correlation coefficients 
#'   differ between the two dataframes.
#' }
#' 
#' Note that confidence intervals for \code{kendall} and \code{spearman} assume a normal sampling
#' distribution for the Fisher z-transform of the correlation.
#' @export
#' @examples
#' 
#' # Load dplyr for starwars data & pipe
#' library(dplyr)
#' 
#' # Single dataframe summary
#' inspect_cor(starwars)
#' # Only show correlations with 'mass' column
#' inspect_cor(starwars, with_col = "mass")
#' 
#' # Paired dataframe summary
#' inspect_cor(starwars, starwars[1:10, ])
#' 
#' # NOT RUN - change in correlation over time
#' # library(dplyr)
#' # tech_grp <- tech %>% 
#' #         group_by(year) %>%
#' #         inspect_cor()
#' # tech_grp %>% show_plot()     
#' 
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
#' @importFrom dplyr summarize
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom tidyr nest

inspect_cor <- function(df1, df2 = NULL, method = "pearson", with_col = NULL, 
                        alpha = 0.05){
  # perform basic column check on dataframe input
  input_type <- check_df_cols(df1, df2)
  # capture the data frame names
  df_names <- get_df_names()
  # filter to only the numeric variables
  df_numeric <- df1 %>% 
    select_if(is.numeric)
  # if only a single df input
  if(input_type == "single"){
    # check that with_col exists
    if(!is.null(with_col)){
      in_num <- with_col %in% colnames(df_numeric)
      in_all <- with_col %in% colnames(df1)
      if(!in_num & in_all)  stop(paste0("with_col = '", with_col, "' is not numeric"))
      if(!in_num & !in_all) stop(paste0("with_col = '", with_col, "' not found in ", df_names[[1]]))
    }
    # calculate correlation coefficients
    if(ncol(df_numeric) > 1){
      # perfom check for 0 variance features, return warning if found
      check_variance(df_numeric)
      # get correlation coefficients for numeric pairs
      suppressWarnings(cor_df <- cor_test_2(df_numeric,
                                            df_name = df_names[[1]], 
                                            with_col = with_col,
                                            alpha = alpha, 
                                            method = method))
      # return top strongest if requested
      pair <- cor_df %>% select(pair) %>% unlist
      out <- cor_df %>% select(-pair)
      
      # attach attributes required for plotting
      attr(out, "pair") <- pair
    } else {
      # return empty dataframe 
      out <- tibble(col_1 = character(), 
                    col_2 = character(), 
                    corr = numeric())
    } 
  } 
  if(input_type == "pair"){
    # stats for df1
    s1 <- inspect_cor(df1, method = method) %>% 
      select(col_1, col_2, corr) %>% 
      rename(corr_1 = corr)
    # stats for df2
    s2 <- inspect_cor(df2, method = method) %>% 
      select(col_1, col_2, corr) %>% 
      rename(corr_2 = corr)
    # join the two
    out <- full_join(s1, s2, by = c("col_1", "col_2"))
    # add p_value for test of difference between correlation coefficients
    out$p_value <- cor_test(out$corr_1, out$corr_2, 
                            n_1 = nrow(df1), n_2 = nrow(df2))
  }
  if(input_type == "grouped"){
    out <- apply_across_groups(df = df1, fn = inspect_cor, 
                               method = method, with_col = with_col, alpha = alpha)
  }
  attr(out, "type")     <- list(method = "cor", input_type = input_type)
  attr(out, "df_names") <- df_names
  attr(out, "method")   <- method
  return(out)
}
