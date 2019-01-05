#' Report Goodman and Kruskal's tau for association between categorical features
#'
#' @param df1 A data frame.  
#' @param df2 An optional second data frame for comparing with \code{df2}.  Defaults to \code{NULL}.
#' @param top The number of rows to print for summaries. Default \code{top = NULL} prints everything.
#' @param show_plot Logical determining whether to show a plot in addition to tibble output.  Default is \code{FALSE}.
#' @return Return a \code{tibble} containing the columns \code{col_1}, \code{col_2}, \code{pair} and \code{k_tau}.  
#' @examples
#' report_association(starwars)
#' @importFrom GoodmanKruskal GKtauDataframe
#' @importFrom dplyr arrange
#' @importFrom dplyr contains
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather

report_association <- function(df1, df2 = NULL, top = NULL, show_plot = F){
  
  # perform basic column check on dataframe input
  check_df_cols(df1)
  
  if(is.null(df2)){
    # pick out categorical columns
    df_cat <- df1 %>% select_if(function(v) is.character(v) | is.factor(v)) %>% as.data.frame
    # calculate association if categorical columns exist
    if(ncol(df_cat) > 1){
      GKMat <- GKtauDataframe(df_cat)
      ass_mat <- GKMat
      class(ass_mat) <- "matrix"
      diag(ass_mat) <- NA
      ass_df <- tibble::as_tibble(ass_mat)
      ass_df$X1 <- colnames(ass_df)
      ass_df  <- gather(ass_df, key = "X2", value = "ass", -X1)
      ass_df <- ass_df %>% filter(!is.na(ass)) %>%
        arrange(desc(abs(ass))) %>%
        mutate(pair = paste(X1, X2, sep = " -> ")) %>%
        select(col_1 = X1, col_2 = X2, pair, association = ass) 
      out <- ass_df %>% slice(1:min(top, nrow(.))) 
      # return dataframe of values
      return(out %>% rename(k_tau = association))
    } else {
      # return empty dataframe of 
      return(tibble(col_1 = character(), col_2 = character(), pair = character(), k_tau = numeric()))
    }
  } else {
    s1 <- report_association(df1,  top = top,  show_plot = F) %>% rename(k_tau_1 = k_tau) %>% select(-col_1, -col_2)
    s2 <- report_association(df2,  top = top,  show_plot = F) %>% rename(k_tau_2 = k_tau) %>% select(-col_1, -col_2)
    ass_tab <- full_join(s1, s2, by = "pair") %>%
      mutate(tau_diff = k_tau_1 - k_tau_2) %>% 
      arrange(desc(abs(tau_diff)))
    return(ass_tab)
  }
}