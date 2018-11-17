report <- function(df, top_n = 10){
  df %>%
    report_space(top_n = top_n) %>%
    report_types(top_n = top_n) %>%
    report_na(top_n = top_n)    %>%
    report_cor(top_n = top_n)   %>% 
    report_imbalance(top_n = top_n) %>%
    report_association(top_n = top_n)
}