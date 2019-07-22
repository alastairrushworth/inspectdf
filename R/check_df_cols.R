check_df_cols <- function(df1, df2 = NULL){
  # if both df1 and df2 are grouped then stop
  if(("grouped_df" %in% class(df1)) & 
     ("grouped_df" %in% class(df2))){
    stop("Both dataframes seem to be grouped: when comparing dataframes,\
    only ungrouped are supported at present")
  }
  # performs some basic checks on df1
  if(!"data.frame" %in% class(df1)) stop("df1 is not a data.frame!")
  if(ncol(df1) == 0) stop("df1 seems to have no columns!")
  # perform basic checks on df2 if provided
  if(!is.null(df2)){
    if(!"data.frame" %in% class(df2)) stop("df2 is not a data.frame!")
    if(ncol(df2) == 0) stop("df2 seems to have no columns!")
  }
  # return the problem type
  if(is.null(df2)){
    if("grouped_df" %in% class(df1)){
      type <- "grouped"
    } else {
      type <- "single"
    }
  } else {
    type <- "pair"
  }
  return(type)
}




