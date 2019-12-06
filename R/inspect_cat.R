#' Summary and comparison of the levels in categorical columns 
#' 
#' @description For a single dataframe, summarise the levels of each categorical 
#' column.  If two dataframes are supplied, compare the levels of categorical features 
#' that appear in both dataframes.  For grouped dataframes, summarise the levels 
#' of categorical features separately for each group.
#' 
#' @param df1 A dataframe.
#' @param df2 An optional second data frame for comparing categorical levels.  
#' Defaults to \code{NULL}.
#' @return A tibble summarising or comparing the categorical features 
#' in one or a pair of dataframes.
#' 
#' @details 
#' For a \strong{single dataframe}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, character vector containing column names of \code{df1}.
#'   \item \code{cnt} integer column containing count of unique levels found in each column, 
#'   including \code{NA}.
#'   \item \code{common}, a character column containing the name of the most common level.
#'   \item \code{common_pcnt}, the percentage of each column occupied by the most common level shown in 
#'   \code{common}.
#'   \item \code{levels}, a named list containing relative frequency tibbles for each feature.
#' }
#' For a \strong{pair of dataframes}, the tibble returned contains the columns: \cr
#' \itemize{
#'   \item \code{col_name}, character vector containing names of columns appearing in both 
#'   \code{df1} and \code{df2}.
#'   \item \code{jsd}, a numeric column containing the Jensen-Shannon divergence.  This measures the 
#'   difference in relative frequencies of levels in a pair of categorical features.  Values near 
#'   to 0 indicate agreement of the distributions, while 1 indicates disagreement.
#'   \item \code{fisher_p}, the p-value corresponding to Fisher's exact test.  A small p indicates 
#'   evidence that the the two sets of relative frequencies are actually different.
#'   \item \code{lvls_1}, \code{lvls_2}, the relative frequency of levels in each of \code{df1} and \code{df2}.
#' }
#' For a \strong{grouped dataframe}, the tibble returned is as for a single dataframe, but where 
#' the first \code{k} columns are the grouping columns.  There will be as many rows in the result 
#' as there are unique combinations of the grouping variables.
#' @author Alastair Rushworth
#' @seealso \code{\link{inspect_imb}}, \code{\link{show_plot}}
#' @export
#' @examples
#' # Load dplyr for starwars data & pipe
#' library(dplyr)
#' 
#' # Single dataframe summary
#' inspect_cat(starwars)
#' 
#' # Paired dataframe comparison
#' inspect_cat(starwars, starwars[1:20, ])
#' 
#' # Grouped dataframe summary
#' starwars %>% group_by(gender) %>% inspect_cat()
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr do
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom progress progress_bar
#' @importFrom Rcpp compileAttributes

inspect_cat <- function(df1, df2 = NULL){
  
  # perform basic column check on dataframe input
  input_type <- check_df_cols(df1, df2)
  # capture the data frame names
  df_names <- get_df_names()
  
  # if only a single df input
  if(input_type == "single"){
    # pick out categorical columns
    df_cat <- df1 %>% 
      select_if(function(v) is.character(v) | is.factor(v) | 
                  is.logical(v) | any(c("Date", "datetime") %in% class(v))) %>%
      mutate_if(is.factor, as.character)
  
    # calculate association if categorical columns exist
    if(ncol(df_cat) > 0){
      # check any factors for duplicate labels
      df_cat <- check_factors(df_cat)
      # get the levels for each category
      names_cat <- colnames(df_cat)
      n_cols <- ncol(df_cat)
      levels_list <- vector("list", length = length(df_cat))
      # loop over columns and tabulte frequencies
      pb <- start_progress(prefix = " Column", total = n_cols)
      for(i in 1:n_cols){
        update_progress(bar = pb, iter = i, total = n_cols, what = names_cat[i])
        levels_list[[i]] <- fast_table(df_cat[[i]], show_na = TRUE, show_cnt = TRUE)
      }
      names(levels_list) <- names(df_cat)
      # get the most common level
      levels_top  <- lapply(levels_list, function(M) M[1, ]) %>% 
        do.call("rbind", .) %>% 
        mutate(col_name = names_cat) %>%
        select(-cnt)
      # get the unique levels
      levels_unique <- suppressWarnings(lapply(levels_list, nrow) %>% 
        do.call("rbind", .) %>% 
        as_tibble(rownames = "col_name"))
      # combine the above tables
      out <- levels_unique %>% 
        left_join(levels_top, by = "col_name") %>% 
        mutate(prop = prop * 100) %>%
        rename(cnt = V1, common = value, common_pcnt = prop)
      out$levels <- levels_list
      # sort by alphabetical order & filter to max number of rows
      out <- out %>% 
        arrange(col_name)
      # add names to the list
      names(out$levels) <- out$col_name
    } else {
      out <- tibble(col_name = character(), cnt = integer(), 
                    common = character(), common_pcnt = numeric(), 
                    levels = list())
    }
  }
  if(input_type == "pair"){
    # levels for df1
    s1 <- inspect_cat(df1) %>% 
      select(-contains("common"), -cnt)
    # levels for df2
    s2 <- inspect_cat(df2) %>% 
      select(-contains("common"), -cnt)
    # combine and clean up levels
    out <- full_join(s1, s2, by = "col_name") %>% 
      mutate(jsd = js_divergence_vec(levels.x, levels.y)) %>%
      mutate(fisher_p = fisher(levels.x, levels.y, n_1 = nrow(df1), 
                               n_2 = nrow(df2))) %>%
      select(col_name, jsd, fisher_p, lvls_1 = levels.x, lvls_2 = levels.y)
    
    # ensure the list names are retained
    names(out[[4]]) <- names(out[[5]]) <- as.character(out$col_name)
  }
  if(input_type == "grouped"){
    out <- apply_across_groups(df = df1, fn = inspect_cat)
  }
  attr(out, "type")     <- list(method = "cat", input_type = input_type)
  attr(out, "df_names") <- df_names
  return(out)
}
