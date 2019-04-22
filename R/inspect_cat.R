#' Summarise and compare the levels within each categorical feature in one or two dataframes.
#'
#' @param df1 A dataframe
#' @param df2 An optional second data frame for comparing categorical levels.  
#' Defaults to \code{NULL}.
#' @param show_plot Logical argument determining whether plot is returned
#' in addition to tibble output.  Default is \code{FALSE}.
#' @return A tibble summarising and comparing the categorical features 
#' in one or a pair of data frames.
#' @details When only \code{df1} is specified, a tibble is returned which 
#' contains summaries of the categorical levels in \code{df1}.
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1}.
#'   \item \code{cnt} integer column containing count of unique levels found in each column 
#'   (including \code{NA}).
#'   \item \code{common} character column containing the name of the most common level.
#'   \item \code{common_pcnt} percentage of each column occupied by the most common level shown in 
#'   \code{common}.
#'   \item \code{levels} relative frequency of levels in each column.
#' }
#' When both \code{df1} and \code{df2} are specified, the relative frequencies of levels across columns
#' common both dataframes are compared.  In particular, the population stability index, and Fisher's 
#' exact test are performed as part of the comparison.
#' \itemize{
#'   \item \code{col_name} character vector containing column names of \code{df1} and \code{df2}.
#'   \item{psi} numeric column containing the 
#'   \href{https://www.quora.com/What-is-population-stability-index}{population stability index}.  
#'   This measures the difference in the distribution of two categorical features.  Conventionally, 
#'   values exceeding 0.25 indicate strong evidence of a change, with values below 0.25 and 0.1 
#'   representing moderate and low evidence of a change.
#'   \item{fisher_p} p-value corresponding to Fisher's exact test.  A small p indicates 
#'   evidence that the the two sets of relative frequencies are actually different.
#'   \item \code{lvls_1}, \code{lvls_2} relative frequency of levels in each of \code{df1} and \code{df2}.
#' }
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' inspect_cat(starwars)
#' # return a visualisation too
#' inspect_cat(starwars, show_plot = TRUE)
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
      # get the levels for each category
      levels_list <- lapply(df_cat, fast_table, show_na = TRUE)
      # get the most common level
      levels_top  <- lapply(levels_list, function(M) M[1, ]) %>% 
        do.call("rbind", .) %>% 
        mutate(col_name = colnames(df_cat))
      # get the unique levels
      levels_unique <- suppressWarnings(lapply(levels_list, nrow) %>% 
        do.call("rbind", .) %>% 
        as_tibble(rownames = "col_name"))
      # combine the above tables
      levels_df <- levels_unique %>% 
        left_join(levels_top, by = "col_name") %>% 
        mutate(prop = prop * 100) %>%
        rename(cnt = V1, common = value, common_pcnt = prop)
      # add the list of levels as a final column
      levels_df$levels <- levels_list
      # sort by alphabetical order & filter to max number of rows
      levels_df <- levels_df %>% 
        arrange(col_name)
      # add names to the list
      names(levels_df$levels) <- levels_df$col_name
      # if plot is requested
      if(show_plot){
        # plot the categories using stacked bars
        plt <- plot_cat(levels_df, df_names)
        # return the plot
        print(plt) 
      }
      # return df
      return(levels_df)
    } else {
        return(tibble(col_name = character(), cnt = integer(), 
                      common = character(), common_pcnt = numeric(), 
                      levels = list()))
    }
  } else {
    # levels for df1
    s1 <- inspect_cat(df1, show_plot = FALSE)  %>% 
      select(-contains("common"), -cnt)
    # levels for df2
    s2 <- inspect_cat(df2, show_plot = FALSE) %>% 
      select(-contains("common"), -cnt)
    # combine and clean up levels
    levels_df <- full_join(s1, s2, by = "col_name") %>% 
      mutate(psi = psi(levels.x, levels.y)) %>%
      mutate(fisher_p = fisher(levels.x, levels.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
        select(col_name, psi, fisher_p, levels.x, levels.y)
    colnames(levels_df)[4:5] <- paste0("lvls_", 1:2)
    # ensure the list names are retained
    names(levels_df[[4]]) <- names(levels_df[[5]]) <- as.character(levels_df$col_name)
    # if plot is requested
    if(show_plot){
      # plot the categories using stacked bars
      plt <- plot_cat(levels_df, df_names)
      # return the plot
      print(plt)
    }
    # return the comparison table
    return(levels_df)
  }
}
