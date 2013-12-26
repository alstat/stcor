#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
double stacovf2(NumericMatrix x, int timelag, NumericMatrix w1){
  int i, j, t, nrow = x.nrow(), ncol = x.ncol();
  double sum = 0;
  
  for(i = 0; i < nrow; ++i){
    for(j = 0; j < nrow; ++j){
      for(t = 0; t < (ncol - timelag); ++t){
        sum = sum + w1(i, j) * x(j, t) * (x(i, (t + timelag)));
      }
    }
  }
  
  return sum / (nrow * (ncol - timelag));
}
