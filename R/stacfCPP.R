stacfCPP <- function(data, 
                     wmatrix = NULL,
                     timelag = 1){
  if(is.null(wmatrix)){
    out <- stacf1(x = data, timelag)
    return(out / sqrt((sum((data)^2)) * (sum(data^2))))
  }
  if(!is.null(wmatrix)){
    out <- stacf2(x = data, y = wmatrix, timelag)
    return(out / sqrt((sum((wmatrix %*% data)^2)) * (sum(data^2))))
  }
}
