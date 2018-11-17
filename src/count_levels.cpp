#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector count_levels_num(NumericVector x){
  // this is the length of the input vector
  const int n = x.length();
  // this is whether the counts will be stored
  std::vector<int> count_levels;
  count_levels.push_back(0);
  int level_index = 0;
  for(int i = 1; i < n; i++){
    double diffi = !(x[i] == x[i - 1]);
    if(diffi){
      level_index++;
      count_levels.push_back(0);
    } else {
      count_levels[level_index]++;
    }
  }
  IntegerVector clevels = wrap(count_levels);
  clevels = clevels + 1;
  return(clevels);
}

// [[Rcpp::export]]
IntegerVector count_levels_char(CharacterVector x){
  // this is the length of the input vector
  const int n = x.length();
  // this is whether the counts will be stored
  std::vector<int> count_levels;
  count_levels.push_back(0);
  int level_index = 0;
  for(int i = 1; i < n; i++){
    double diffi = !(x[i] == x[i - 1]);
    if(diffi){
      level_index++;
      count_levels.push_back(0);
    } else {
      count_levels[level_index]++;
    }
  }
  IntegerVector clevels = wrap(count_levels);
  clevels = clevels + 1;
  return(clevels);
}