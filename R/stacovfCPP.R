stacovfCPP <- function(data, 
                       wmatrix1 = NULL, 
                       wmatrix2 = NULL, 
                       timelag = 1){
  a = dim(data)[1]
  b = dim(data)[2]
  sumn <- 0
  if(timelag < 0)
    timelag = -timelag
  if(is.null(wmatrix1) && is.null(wmatrix2)) 
    out <- stacovf1(data, timelag)
  else if(!is.null(wmatrix1) && is.null(wmatrix2))
    out <- stacovf2(data, timelag, wmatrix1)
  else if(is.null(wmatrix1) && !is.null(wmatrix2)) 
    out <- stacovf3(data, timelag, wmatrix2)
  else if(!is.null(wmatrix1) && !is.null(wmatrix2))
    out <- stacovf4(data, timelag, wmatrix1, wmatrix2)
  return(out)
}
