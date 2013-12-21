stacovf <- function(datamat, 
                    wmatrix1 = NULL, 
                    wmatrix2 = NULL, 
                    timelag = 1){
  a = dim(datamat)[1]
  b = dim(datamat)[2]
  sumn <- 0
  if(timelag < 0){
    timelag = -timelag
  }
  if(is.null(wmatrix1) && is.null(wmatrix2)) {
    for(i in 1:a){
      for(t in 1:(b-(timelag))){
        sumn <- sumn +  (datamat[i,t])*(datamat[i,(t+(timelag))])
      }
    }
    return(sumn / (a * (b - (timelag))))
  }
  if(!is.null(wmatrix1) && is.null(wmatrix2)){
    for(i in 1:a){
      for(j in 1:a){
        for(t in 1:(b-(timelag))){
          sumn <- sumn +   (wmatrix1[i,j])*
            (datamat[j,t])*(datamat[i,t+(timelag)])
        }
      }
    }
    return(sumn/(a*(b-(timelag))))
  }
  if(is.null(wmatrix1) && !is.null(wmatrix2)) {
    for(i in 1:a){
      for(j in 1:a){
        for(t in 1:(b-(timelag))){
          sumn <- sumn + (datamat[i,t])*(wmatrix2[i,j])*(datamat[j,t+(timelag)])
        }
      }
    }
    return(sumn/(a*(b-(timelag))))
  }
  if(!is.null(wmatrix1) && !is.null(wmatrix2)){
    for(i in 1:a){
      for(j in 1:a){
        for(t in 1:(b - (timelag))){
          sumn <- sumn + (wmatrix1[i, j]) *
            (datamat[j, t]) * (wmatrix2[i, j]) * (datamat[j, t + (timelag)])
        }
      }
    }
    return(sumn / (a * (b - (timelag))))
  }
}
