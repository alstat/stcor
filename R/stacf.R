stacf <- function(data, max.timelag, wmatrices = list(...)){
  stacf.tab <- matrix(NA, nrow = max.timelag, ncol = length(wmatrices))
  
  for(j in 1:length(wmatrices)){
    for(i in 1:max.timelag){
      stacf.tab[i, j] <- stacf.eq(data = data, wmatrix = wmatrices[[j]], timelag = i)
    }
  }
  return(stacf.tab)
}
