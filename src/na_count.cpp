#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int na_numeric(NumericVector x) {
  const int n = x.length();
  int na_count = 0;
  for(int i = 0; i < n; i++){
    // na_count = na_count + NumericVector::is_na(x[i]);
    na_count = na_count + NumericVector::is_na(x[i]);
  }
  return(na_count);
}

// [[Rcpp::export]]
int na_character(CharacterVector x) {
  const int n = x.length();
  int na_count = 0;
  for(int i = 0; i < n; i++){
    na_count = na_count + CharacterVector::is_na(x[i]);
  }
  return(na_count);
}

// [[Rcpp::export]]
int na_logical(LogicalVector x) {
  const int n = x.length();
  int na_count = 0;
  for(int i = 0; i < n; i++){
    na_count = na_count + LogicalVector::is_na(x[i]);
  }
  return(na_count);
}

// [[Rcpp::export]]
int na_integer(IntegerVector x) {
  const int n = x.length();
  int na_count = 0;
  for(int i = 0; i < n; i++){
    na_count = na_count + IntegerVector::is_na(x[i]);
  }
  return(na_count);
}