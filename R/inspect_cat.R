#' Summarise and compare the levels for each categorical feature in one or two dataframes.
#'
#' @param df1 A dataframe
#' @param df2 An optional second data frame for comparing categorical levels.  
#' Defaults to \code{NULL}.
#' @param show_plot (Deprecated) Logical flag indicating whether a plot should be shown.  
#' Superseded by the function \code{show_plot()} and will be dropped in a future version.
#' @return A tibble summarising or comparing the categorical features 
#' in one or a pair of dataframes.
#' @details When \code{df2 = NULL}, a tibble containing summaries of the categorical features in 
#' \code{df1} is returned:
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1}.
#'   \item \code{cnt} integer column containing count of unique levels found in each column, 
#'   including \code{NA}.
#'   \item \code{common} character column containing the name of the most common level.
#'   \item \code{common_pcnt} percentage of each column occupied by the most common level shown in 
#'   \code{common}.
#'   \item \code{levels} names list containing relative frequency tibbles for each feature.
#' }
#' When \code{df1} and \code{df2} are specified, a comparison of the relative frequencies 
#' of levels in common columns is performed.  In particular, Jensen-Shannon divergence and 
#' Fisher's exact test are returned as part of the comparison.
#' \itemize{
#'   \item \code{col_name} character vector containing names of columns appearing in both 
#'   \code{df1} and \code{df2}.
#'   \item \code{jsd} numeric column containing the Jensen-Shannon divergence.  This measures the 
#'   difference in relative frequencies of levels in a pair of categorical features.  Values near 
#'   to 0 indicate agreement of the distributions, while 1 indicates disagreement.
#'   \item \code{fisher_p} p-value corresponding to Fisher's exact test.  A small p indicates 
#'   evidence that the the two sets of relative frequencies are actually different.
#'   \item \code{lvls_1}, \code{lvls_2} relative frequency of levels in each of \code{df1} and \code{df2}.
#' }
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' inspect_cat(starwars)
#' # compare the levels in two data frames
#' inspect_cat(starwars, starwars[1:20, ])
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

inspect_cat <- function(df1, df2 = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
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
      
      # attach attributes required for plotting
      attr(out, "type")     <- list(method = "cat", 1)
      attr(out, "df_names") <- df_names
    } else {
      out <- tibble(col_name = character(), cnt = integer(), 
                    common = character(), common_pcnt = numeric(), 
                    levels = list())
    }
  } else {
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
    
    # attach attributes required for plotting
    attr(out, "type")     <- list(method = "cat", 2)
    attr(out, "df_names") <- df_names
  }
  if(show_plot) plot_deprecated(out)
  return(out)
}
