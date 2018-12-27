#' Report the memory usage of a data frame
#'
#' @param df1 A data frame
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @return Prints the proportion of overall memory used by each column and the total usage.
#' @examples
#' # get tibble of column memory usage for the starwars data
#' report_space(starwars)
#' # get column memory usage and show as barplot
#' report_space(starwars, show_plot = T)
report_space <- function(df1, df2 = NULL, top = NULL, show_plot = F){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame name
  df_name <- as.character(substitute(df1))
  ee <- find_chain_parts()$lhs
  if(!is.null(ee)) df_name <- deparse(ee)
  
  if(is.null(df2)){
    # get column size
    col_space     <- sapply(df1, size_up, form = F)
    col_space_ch  <- sapply(df1, size_up, form = T)
    col_max       <- which.max(col_space)
    col_max_size  <- col_space[col_max]
    col_max_names <- names(col_space)[col_max]
    
    # get ncols, nrows, and storage size of the data
    ncl <- format(ncol(df1), big.mark = ",")
    nrw <- format(nrow(df1), big.mark = ",")
    sz  <- size_up(df1, form = T)
  
    # get top 10 largest columns by storage size, pass to the console histogrammer
    out <- vec_to_tibble(col_space) %>% 
      dplyr::left_join(vec_to_tibble(col_space_ch), by = "names") %>%
      dplyr::mutate(percent_space = 100 * n.x / sum(n.x)) %>%
      dplyr::arrange(desc(percent_space)) %>%
      dplyr::slice(1:min(top, nrow(.))) %>%
      dplyr::rename(col_names = names, size = n.y) %>% 
      dplyr::select(-n.x)
    # return plot if requested
    if(show_plot){
      ttl_plt <- paste0("Column sizes in df::", df_name)
      sttl_plt <- paste0("df::", df_name,  " has ", ncol(df1), " columns, ", nrow(df1), " rows and total memory usage of ", sz)
      plt <- out %>%
        dplyr::mutate(col_names = factor(col_names, levels = as.character(col_names))) %>%
        ggplot2::ggplot(ggplot2::aes(x = col_names, y = percent_space, fill = col_names, label = size)) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::labs(x = "", y = "Percentage of total space (%)", title = ttl_plt, subtitle = sttl_plt) + 
        ggplot2::guides(fill = FALSE) +
        ggplot2::geom_text(nudge_y = -3, color = "white", angle = 90) +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(plt)
    }
    # return tibble
    return(out)
    
  } else {
    # gather df names
    df_name1 <- as.character(substitute(df1))
    df_name2 <- as.character(substitute(df2))
    if(length(df_name1) > 1) df_name1 <- df_name1[2]
    if(length(df_name2) > 1) df_name2 <- df_name2[2]
    if(df_name1 == df_name2) df_name2 <- paste0(df_name2, "_2")
    # get the space report for both input dfs
    df1 <- report_space(df1, top = top, show_plot = F)
    df2 <- report_space(df2, top = top, show_plot = F)
    sjoin <- full_join(df1, df2, by = "col_names") %>%
      select(col_names, contains("size"), contains("percent"))
    colnames(sjoin)[2] <- paste0("size_",  df_name1)
    colnames(sjoin)[3] <- paste0("size_",  df_name2)
    colnames(sjoin)[4] <- paste0("space_", df_name1)
    colnames(sjoin)[5] <- paste0("space_", df_name2)
    # max size of both dfs
    sz1  <- size_up(df1, form = T)
    sz2  <- size_up(df2, form = T)
    
    if(show_plot){
      # convert to a tall df
      z1 <- sjoin %>% select(-contains("size")) %>% 
        gather(key = "df_input", value = "percent", -col_names) %>% 
        mutate(df_input = gsub("space_", "", df_input))
      z2 <- sjoin %>% select(-contains("space")) %>% 
        gather(key = "df_input", value = "size", -col_names) %>% 
        mutate(df_input = gsub("size_", "", df_input))
      z_tall <- z1 %>% left_join(z2, by = c("col_names", "df_input"))
      # make axis names
      ttl_plt <- paste0("Column sizes in df::", df_name1, " & df::", df_name2)
      sttl_plt1 <- paste0("df::", df_name1,  " has ", ncol(df1), " columns, ", nrow(df1), " rows and total memory usage of ", sz1)
      sttl_plt2 <- paste0("df::", df_name2,  " has ", ncol(df2), " columns, ", nrow(df2), " rows and total memory usage of ", sz2)
      # plot the result
      plt <- z_tall %>%
        dplyr::mutate(col_type = factor(col_names, levels = sjoin$col_names)) %>%
        ggplot2::ggplot(ggplot2::aes(x = col_names, y = percent, fill = as.factor(df_input))) + 
        ggplot2::geom_bar(stat = "identity", position = "dodge") + 
        ggplot2::labs(x = "", y = "Percentage of total space (%)", title = ttl_plt, subtitle = paste0(sttl_plt1, "\n", sttl_plt2)) + 
        ggplot2::scale_fill_discrete(name = "Data frame") +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(plt)
    }
    return(sjoin)
  }
}
