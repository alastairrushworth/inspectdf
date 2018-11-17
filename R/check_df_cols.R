check_df_cols <- function(df){
  # performs some basic checks
  if(!"data.frame" %in% class(df)) stop("Your input is not a data.frame!")
  if(ncol(df) == 0) stop("Your input seems to have no columns!")
}




