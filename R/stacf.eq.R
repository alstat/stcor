stacf.eq <- function(datamat, 
                  wmatrix = NULL,
                  timelag = 1){
  a = dim(datamat)[1]
  b = dim(datamat)[2]
  sumn <- 0
  if(is.null(wmatrix)){
    for(i in 1:a){
      for(t in 1:(b - (timelag))){
        sumn <- sumn + (datamat[i, t])*(datamat[i, (t + (timelag))])
      }
    }
    return(sumn / sqrt((sum((datamat)^2)) * (sum(datamat^2))))
  }
  if(!is.null(wmatrix)){
    for(i in 1:a){
      for(j in 1:a){
        for(t in 1:(b - (timelag))){
          sumn <- sumn + (wmatrix[i, j]) *
            (datamat[j, t]) * (datamat[i, t + (timelag)])
        }
      }
    }
    return(sumn / sqrt((sum((wmatrix %*% datamat)^2)) * (sum(datamat^2))))
  }
}
