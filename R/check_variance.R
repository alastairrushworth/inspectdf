#' @importFrom stats var

# this function checks for 0 variance numeric features
check_variance <- function(df_num){
  # get variances
  zvars <- sapply(df_num, var)
  # zap anything really small
  zvars <- zapsmall(zvars)
  # zero var indices
  zvar_inds <- which(zvars == 0)
  # return a warning if any column has zero variance
  if(length(zvar_inds) > 0){
    zvar_names <- colnames(df_num)[zvar_inds]
    wmesg <- paste(zvar_names, collapse = ", ")
    wmesg <- paste("Columns with 0 variance found:", wmesg)
    warning(wmesg, call. = FALSE)
  }
}