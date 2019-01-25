#' Report and compare the levels within each categorical feature
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing categorical features.  
#' Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. 
#' Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  
#' Default is \code{FALSE}.
#' @return A \code{tibble} containing the names of 
#' categorical columns (\code{col_name}), the number of levels within each (\code{n_lvl}), 
#' the most common level (\code{cmn_lvl}), the percentage occurrence of the most common 
#' feature (\code{cmn_pcnt}) and a list of tibbles containing the percentage appearance 
#' of each feature (\code{levels}).
#' @details If \code{df2} is specified, the tibble returned compares the common columns
#' in both data frames.  The population stability index (\code{psi}), chi-squared statistic
#' (\code{chisq}) are returned.  The \code{p_value} column provides evidence against the null 
#' hypothesis that level distribution in each pair of columns is the same.
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' report_cat(starwars)
#' # return a visualisation too
#' report_cat(starwars, show_plot = TRUE)
#' # compare the levels in two data frames
#' report_cat(starwars, starwars[1:20, ])
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr do
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom grDevices colorRampPalette
#' @importFrom magrittr %>%

report_cat <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  # capture the data frame names
  df_names <- get_df_names()
  
  if(is.null(df2)){
    # pick out categorical columns
    df_cat <- df1 %>% select_if(function(v) is.character(v) | is.factor(v))
    # calculate association if categorical columns exist
    if(ncol(df_cat) > 1){
      # get the levels for each category
      levels_list <- lapply(df_cat, fast_table, show_na = TRUE)
      # get the most common level
      levels_top  <- lapply(levels_list, function(M) M[1, ]) %>% 
        do.call("rbind", .) %>% 
        mutate(col_name = colnames(df_cat))
      # get the unique levels
      levels_unique <- lapply(levels_list, nrow) %>% 
        do.call("rbind", .) %>% 
        as_tibble(rownames = "col_name")
      # combine the above tables
      levels_df <- levels_unique %>% 
        left_join(levels_top, by = "col_name") %>% 
        mutate(prop = prop * 100) %>%
        rename(n_lvl = V1, cmn_lvl = value, cmn_pcnt = prop)
      # add the list of levels as a final column
      levels_df$levels <- levels_list
      # sort by alphabetical order & filter to max number of rows
      levels_df <- levels_df %>% 
        arrange(col_name) %>% 
        slice(1:min(top, nrow(.))) 
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
        return(tibble(col_name = character(), n_lvl = integer(), 
                      cmn_lvl = character(), cmn_pcnt = numeric(), 
                      levels = list()))
    }
  } else {
    # levels for df1
    s1 <- report_cat(df1, top = top, show_plot = FALSE)  %>% 
      select(-contains("dom"), -n_lvl)
    # levels for df2
    s2 <- report_cat(df2, top = top, show_plot = FALSE) %>% 
      select(-contains("dom"), -n_lvl)
    # combine and clean up levels
    levels_df <- full_join(s1, s2, by = "col_name") %>% 
      mutate(psi = psi(levels.x, levels.y)) %>%
      mutate(chisq = chisq(levels.x, levels.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      mutate(p_value = chisq_p(levels.x, levels.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      select(col_name, psi, chisq, p_value, levels.x, levels.y)
    colnames(levels_df)[5:6] <- paste0("lvls_", df_names)
    # ensure the list names are retained
    names(levels_df[[5]]) <- names(levels_df[[6]]) <- names(s2$levels)
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
