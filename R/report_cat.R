#' Report and compare the levels within each categorical feature
#'
#' @param df1 A data frame
#' @param df2 An optional second data frame for comparing categorical features.  
#' Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. 
#' Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  
#' Default is \code{FALSE}.
#' @return If \code{df2 = NULL} then is a \code{tibble} containing the names of 
#' categorical columns (\code{col_name}), the number of levels within each (\code{n_lvl}), 
#' the most common level (\code{cmn_lvl}), the percentage occurrence of the most common 
#' feature (\code{cmn_pcnt}) and a list of tibbles containing the percentage appearance 
#' of each feature (\code{levels}).
#' @export
#' @examples
#' data("starwars", package = "dplyr")
#' report_cat(starwars)
#' # return a visualisation too
#' report_cat(starwars, show_plot = T)
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
      # get the top levels
      levels_top  <- lapply(levels_list, function(M) M[1, ]) %>% do.call("rbind", .) %>% mutate(col_name = colnames(df_cat))
      # get the unique levels
      levels_unique <- lapply(levels_list, nrow) %>% do.call("rbind", .) %>% as_tibble(rownames = "col_name")
      # combine the above tables
      levels_df <- levels_unique %>% left_join(levels_top, by = "col_name") %>% mutate(prop = prop * 100) %>%
        rename(n_lvl = V1, cmn_lvl = value, cmn_pcnt = prop)
      # add the list of levels as a final column
      levels_df$levels <- levels_list
      # sort by alphabetical order & filter to max number of rows
      levels_df <- levels_df %>% arrange(col_name) %>% slice(1:min(top, nrow(.))) 
      # add names to the list
      names(levels_df$levels) <- levels_df$col_name
      if(show_plot){
        # plotting pallete
        b <- colorRampPalette(c("tomato3", "white"))
        zcols <- b(1001)
        put_na_top <- function(dfb){
          if(anyNA(dfb$value)){
            na_row <- which(is.na(dfb$value))[1]
            return(bind_rows(dfb[na_row, ], dfb[-na_row, ]))
          } else {
            return(dfb)
          }
          
        }
        lvl_df <- suppressWarnings(bind_rows(levels_df$levels, .id = 'col_name')) %>% 
          group_by(col_name) %>%
          arrange(col_name, desc(prop), desc(value)) %>%
          mutate(colval = cumsum(prop)) %>% ungroup %>%
          arrange(col_name, prop, value) %>%
          mutate(level_key = paste0(value, col_name))
        lvl_df <- lvl_df %>% 
          group_by(col_name) %>%
          do(put_na_top(.)) %>% ungroup %>% arrange(col_name, prop, value)
        plt <- lvl_df %>% 
          mutate(level_key = factor(level_key, levels = unique(level_key))) %>%
          ggplot(aes(x = col_name, y = prop, fill = level_key)) + 
          geom_bar(position = "stack", stat = "identity", colour = "black", 
                   size = 0.2) +
          scale_fill_manual(
            values = ifelse(is.na(lvl_df$value), "gray65", 
                            zcols[round(lvl_df$colval * 1000, 0)])) +
          coord_flip() +
          guides(fill = FALSE) + 
          theme(axis.title.y = element_blank(), panel.background = element_blank(),
                axis.ticks.y = element_blank(), panel.border = element_blank(), 
                panel.grid.major = element_blank()) +
          labs(x = "", y = "",
               title =  paste0("Frequency of levels in categorical columns of df::", 
                               df_names$df1), 
               subtitle = bquote("Gray segments correspond to missing values"))
        
        
        # two different label series
        annts <- lvl_df %>% 
          mutate(col_num = as.integer(factor(col_name, levels = levels_df$col_name)))
        annts$value[annts$prop < 0.15] <- NA
        col_vec <- ifelse((annts$colval > 0.7) & (annts$prop < 0.7), 2, 1)
        # add a white series to the bigger bars
        plt <- plt + geom_text(aes(x = annts$col_num, 
                            y = annts$colval - (annts$prop/2), 
                            label = annts$value), 
                        color = c("white", "gray70")[col_vec], inherit.aes = FALSE, na.rm = T)
        
      # return the plot
       print(plt) 
      }
      
      # return df
      return(levels_df)
    } else {
        return(tibble(col_name = character(), n_lvl = integer(), 
                      cmn_lvl = character(), cmn_pcnt = numeric(), levels = list()))
    }
  } else {
    s1 <- report_cat(df1, top = top, show_plot = FALSE)  %>% select(-contains("dom"), -n_lvl)
    s2 <- report_cat(df2, top = top, show_plot = FALSE) %>% select(-contains("dom"), -n_lvl)
    levels_tab <- full_join(s1, s2, by = "col_name") %>% 
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
