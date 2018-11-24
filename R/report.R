report <- function(df, top_n = NULL){
  df %>%
    report_space(top_n = top_n) %>%
    report_types(type = "console") %>%
    report_na(top_n = top_n, type = "console") %>%
    report_cor(top_n = top_n, type = "console") %>% 
    report_imbalance(top_n = top_n, type = "console") %>%
    report_association(top_n = top_n, type = "console")
}