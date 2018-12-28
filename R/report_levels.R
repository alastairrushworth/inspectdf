#' Report and compare the levels within each categorical feature
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing categorical levels  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return If \code{df2 = NULL} then is a \code{tibble} containing the names of categorical columns (\code{col_name}), the number of levels within each (\code{n_levels}), the most common level (\code{dom_level}), the percentage occurence of the most common feature (\code{dom_percent}) and a list of tibbles containing the percentage appearance of each feature (\code{levels}).
#' @examples
#' report_levels(starwars)

report_levels <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  if(is.null(df2)){
    # pick out categorical columns
    df_cat <- df1 %>% select_if(function(v) is.character(v) | is.factor(v))
    # calculate association if categorical columns exist
    if(ncol(df_cat) > 1){
      # get the levels for each category
      levels_list <- lapply(df_cat, fast_table, show_all = T)
      # get the top levels
      levels_top  <- lapply(levels_list, function(M) M[1, ]) %>% do.call("rbind", .) %>% dplyr::mutate(col_name = colnames(df_cat))
      # get the unique levels
      levels_unique <- lapply(levels_list, nrow) %>% do.call("rbind", .) %>% tibble::as.tibble(rownames = "col_name")
      # combine the above tables
      levels_df <- levels_unique %>% dplyr::left_join(levels_top, by = "col_name") %>% dplyr::mutate(prop = prop * 100) %>%
        dplyr::rename(n_levels = V1, dom_level = value, dom_percent = prop)
      # add the list of levels as a final column
      levels_df$levels <- levels_list
      # sort by alphabetical order & filter to max number of rows
      levels_df %<>% dplyr::arrange(col_name) %>% dplyr::slice(1:min(top, nrow(.))) 
      # return df
      return(levels_df)
    } else {
        return(tibble(col_name = character(), n_levels = integer(), 
                      dom_level = character(), dom_percent = numeric(), levels = list()))
    }
  } else {
    s1 <- report_levels(df1, top = top, show_plot = F)  %>% select(-contains("dom"), -n_levels)
    s2 <- report_levels(df2, top = top, show_plot = F) %>% select(-contains("dom"), -n_levels)
    levels_tab <- dplyr::full_join(s1, s2, by = "col_name") %>% 
      mutate(diff_1_2 = n_in(levels.x, levels.y)) %>%
      mutate(diff_2_1 = n_in(levels.y, levels.x)) %>%
      mutate(diff_all = diff_1_2 + diff_2_1) %>%
      mutate(diff_df  = get_newlevel_tibble(levels.x, levels.y)) %>%
      mutate(psi = psi(levels.x, levels.y)) %>%
      mutate(chisq = chisq(levels.x, levels.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      mutate(p_value = chisq_p(levels.x, levels.y, n_1 = nrow(df1), n_2 = nrow(df2))) %>%
      select(-levels.x, -levels.y, col_name, psi, chisq, p_value, contains("diff"))
    
    return(levels_tab)
  }
}