centralize <- function(data){
  if(!is.data.frame(data))
    stop("data should be data.frame class")
  if(is.data.frame(data)){
    row <- dim(data)[1]
    col <- dim(data)[2]
    mat <- matrix(NA, nrow = row, ncol = col)
    for(j in 1:col){
      mat[, j] <- data[, j] - mean(data[, j])      
    }  
    return(as.data.frame(mat))
  }
}
