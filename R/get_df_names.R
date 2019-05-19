get_df_names <- function(){
  # capture the data frame name from the function call
  df1_subs <- substitute(df1, env = parent.frame())
  df2_subs <- substitute(df2, env = parent.frame())
  # check for calls - usually if some subsetting going on
  df_name1 <- as.character(df1_subs)
  df_name2 <- as.character(df2_subs)
  # if class is call then get second arg
  if(class(df1_subs) == "call") df_name1 <- df_name1[2]
  if(class(df2_subs) == "call") df_name2 <- df_name2[2]
  # if either is a vector with pipe as first element then choose second
  df_name1 <- ifelse(df_name1 %>% length > 1 & df_name1[1] == "%>%", 
                     df_name1[2], df_name1[1])
  df_name2 <- ifelse(df_name2 %>% length > 1 & df_name2[1] == "%>%", 
                     df_name2[2], df_name2[1])
  # if passed using a chain function, look for argument name
  # only use if the first part of the chain isn't a fn
  ee <- find_chain_parts()$lhs
  if(!is.null(ee)){
    if(!grepl("[(]", deparse(ee))){
      df_name1 <- deparse(ee) 
    }
  }
  # print(df_name1)
  # if the length is still greater than 1, then use default names
  if(df_name1 %>% length > 1) df_name1 <- "df1"
  if(df_name2 %>% length > 1) df_name2 <- "df2"
  # if both data frames have the same name, then append a subscript to secodn
  if(!is.null(df_name2) & !is.na(df_name2) & (df_name1 == df_name2)){
    df_name2 <- paste0(df_name2, "_2")
  }
  return(list(df1 = df_name1, df2 = df_name2))
}

# for testing only
get_df_names_test <- function(df1, df2 = NULL){
  return(get_df_names())
}

