.onLoad <- function(libname = find.package("reporter"), pkgname = "reporter"){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".", "prop.x", "prop.y", "col_1", "col_2", 
                             "correlation", "prop", "psi_vals", "X1", "X2", 
                             "ass", "pair", "association", "k_tau", 
                             "k_tau_1", "k_tau_2", "tau_diff", "lower", 
                             "upper", "value", "col_name", "percent", "V1", 
                             "n_levels", "levels.x", "levels.y", "diff_1_2", 
                             "diff_2_1", "p_value", "n", "type", "count_na", 
                             "mid", "hist.x", "hist.y", "n.x", "percent_space", 
                             "n.y", "df_input", "count_type", "count", "col_type", 
                             "corr", "is_sig", "index", "data_frame", "pcnt", 
                             "n_lvl", "level_key"))
  invisible()
}