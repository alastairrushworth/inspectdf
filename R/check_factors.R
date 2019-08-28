check_factors <- function(df_cat){
  if("factor" %in% sapply(df_cat, class)){
    # is levels as long as unique levels
    are_lvls_unq <- function(fct){
      if(class(fct) == "factor"){
        return(!length(levels(fct)) == length(unique(levels(fct))))
      } else {
        return(FALSE)
      }
    }
    non_unq <- which(sapply(df_cat, are_lvls_unq))
    # any non-unique, replace levels
    if(length(non_unq) > 0){
      for(i in 1:length(non_unq)){
        levels(df_cat[, non_unq[i]]) <- unique(levels(df_cat[, non_unq[i]]))
        warning(paste0("Dropping duplicate levels found in ", names(df_cat[non_unq[i]])))
      }
    }
  }
  return(df_cat)
}