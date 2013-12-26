#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)
  
// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
double stacf1(NumericMatrix x, int timelag){
  int i, t, nrow = x.nrow(), ncol = x.ncol();
  double sum = 0.0;
  
  for (i = 0; i < nrow; ++i){
    for(t = 0; t < (ncol - timelag); ++t){
      sum = sum + x(i, t) * x(i, (t + timelag));
    }
  }
  return sum;
}
