// [[Rcpp::export]]
double stacovf1(NumericMatrix x, int timelag){
  int i, t, nrow = x.nrow(), ncol = x.ncol();
  double sum = 0;
  
  for(i = 0; i < nrow; ++i){
    for(t = 0; t < (ncol - timelag); ++t){
      sum = sum + x(i, t) * (x(i, (t + timelag)));
    }
  }
  
  return sum / (nrow * (ncol - timelag));
}

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

// [[Rcpp::export]]
double stacovf3(NumericMatrix x, int timelag, NumericMatrix w2){
  int i, j, t, nrow = x.nrow(), ncol = x.ncol();
  double sum = 0;
  
  for(i = 0; i < nrow; ++i){
    for(j = 0; j < nrow; ++j){
      for(t = 0; t < (ncol - timelag); ++t){
        sum = sum + x(i, t) * w2(i, j) * (x(j, (t + timelag)));
      }
    }
  }
  
  return sum / (nrow * (ncol - timelag));
}

// [[Rcpp::export]]
double stacovf4(NumericMatrix x, int timelag, NumericMatrix w1, NumericMatrix w2){
  int i, j, t, nrow = x.nrow(), ncol = x.ncol();
  double sum = 0;
  
  for(i = 0; i < nrow; ++i){
    for(j = 0; j < nrow; ++j){
      for(t = 0; t < (ncol - timelag); ++t){
        sum = sum + w1(i, j) * x(j, t) * w2(i, j) * (x(j, (t + timelag)));
      }
    }
  }
  
  return sum / (nrow * (ncol - timelag));
}
