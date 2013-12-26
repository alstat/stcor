stacovfCPP <- function(data, 
                       wmatrix1 = NULL, 
                       wmatrix2 = NULL, 
                       timelag = 1){
  if(!is.data.frame(data))
    stop("data should be data.frame class")
  if(is.data.frame(data))
    data = t(as.matrix(data))
  a = dim(data)[1]
  b = dim(data)[2]
  sumn <- 0
  if(timelag < 0){
    timelag = -timelag
  }
  if(is.null(wmatrix1) && is.null(wmatrix2)) {
    out <- stacovf1(data, timelag)
  }
  if(!is.null(wmatrix1) && is.null(wmatrix2)){
    out <- stacovf2(data, timelag, wmatrix1)
  }
  if(is.null(wmatrix1) && !is.null(wmatrix2)) {
    out <- stacovf3(data, timelag, wmatrix2)
  }
  if(!is.null(wmatrix1) && !is.null(wmatrix2)){
    out <- stacovf4(data, timelag, wmatrix1, wmatrix2)
  }
  return(out)
}
