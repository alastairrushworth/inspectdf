#' Report the memory usage of a data frame
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing memory usage with.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Prints the proportion of overall memory used by each column and the total usage.
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' # get tibble of column memory usage for the starwars data
#' report_space(starwars)
#' # get column memory usage and show as barplot
#' report_space(starwars, show_plot = TRUE)
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 theme
#' @importFrom magrittr %>%
#' @importFrom tibble tibble

report_space <- function(df1, df2 = NULL, top = NULL, show_plot = FALSE){
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  # capture the data frame names
  df_names <- get_df_names()
  
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
      left_join(vec_to_tibble(col_space_ch), by = "names") %>%
      mutate(percent_space = 100 * n.x / sum(n.x)) %>%
      arrange(desc(percent_space)) %>%
      slice(1:min(top, nrow(.))) %>%
      rename(col_names = names, size = n.y) %>% 
      select(-n.x)
    # return plot if requested
    if(show_plot){
      # convert column names to factor
      out_plot <- out %>% mutate(col_names = factor(col_names, levels = as.character(col_names)))
      # construct bar plot of column memory usage
      plt <- bar_plot(df_plot = out_plot, x = "col_names", y = "percent_space", fill = "col_names", label = "size", 
                      ttl = paste0("Column sizes in df::", df_names$df1), 
                      sttl = paste0("df::", df_names$df1,  " has ", ncol(df1), " columns, ", nrow(df1), " rows and total memory usage of ", sz), 
                      ylb = "Percentage of total space (%)", rotate = TRUE)
      # add text annotation to plot
      plt <- add_annotation_to_bars(x = out_plot$col_names, y = out_plot$percent_space, z = out_plot$size, plt = plt, thresh = 0.2)
      # print plot
      print(plt)
    }
    # return tibble
    return(out)
    
  } else {
    # get the space report for both input dfs
    df1 <- report_space(df1, top = top, show_plot = F)
    df2 <- report_space(df2, top = top, show_plot = F)
    sjoin <- full_join(df1, df2, by = "col_names") %>%
      select(col_names, contains("size"), contains("percent"))
    colnames(sjoin)[2] <- paste0("size_",  df_names$df1)
    colnames(sjoin)[3] <- paste0("size_",  df_names$df2)
    colnames(sjoin)[4] <- paste0("space_", df_names$df1)
    colnames(sjoin)[5] <- paste0("space_", df_names$df2)
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
      ttl_plt <- paste0("Column sizes in df::", df_names$df1, " & df::", df_names$df2)
      sttl_plt1 <- paste0("df::", df_names$df1,  " has ", ncol(df1), " columns, ", nrow(df1), " rows and total memory usage of ", sz1)
      sttl_plt2 <- paste0("df::", df_names$df2,  " has ", ncol(df2), " columns, ", nrow(df2), " rows and total memory usage of ", sz2)
      # plot the result
      plt <- z_tall %>%
        mutate(col_type = factor(col_names, levels = sjoin$col_names)) %>%
        ggplot(aes(x = col_names, y = percent, fill = as.factor(df_input))) + 
        geom_bar(stat = "identity", position = "dodge") + 
        labs(x = "", y = "Percentage of total space (%)", title = ttl_plt, subtitle = paste0(sttl_plt1, "\n", sttl_plt2)) + 
        scale_fill_discrete(name = "Data frame") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(plt)
    }
    return(sjoin)
  }
}
