stpacf <- function(data, max.timelag, wmatrices = list(...)){
  a.mat <- matrix(NA, nrow = max.timelag * (length(wmatrices)), ncol = 1)
  b.mat <- matrix(NA, nrow = max.timelag * (length(wmatrices)), 
                  ncol = max.timelag * (length(wmatrices)))
  c.mat <- matrix(NA, nrow = max.timelag * (length(wmatrices)), ncol = 1)
  d.mat <- matrix(NA, nrow = max.timelag, ncol = length(wmatrices))
  i.max = 0
  for(j in 1:max.timelag){
    for(i in 1:length(wmatrices)){
      a.mat[i + i.max, ] <- stacovfCPP(data, wmatrix1 = wmatrices[[i]], timelag = j)
    }
    i.max = i * j
  }
  i.max = 0
  k.max = 0
  for(l in 1:max.timelag){
    for(k in 1:length(wmatrices)){
      for(j in 1:max.timelag){
        for(i in 1:length(wmatrices)){
          b.mat[i + i.max, k + k.max] <- stacovfCPP(data, wmatrix1 = wmatrices[[i]], wmatrix2 = wmatrices[[k]], timelag = j - l)
        }
        i.max = i * j
      }
      i.max = 0
    }
    k.max = k * l 
  }
  b.matinv <- solve(b.mat)
  c.mat <- b.matinv %*% a.mat
  d.mat <- matrix(c.mat, nrow = max.timelag, ncol = length(wmatrices), byrow = TRUE)
  return(d.mat)
}
