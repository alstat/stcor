#' Centralize Space-Time Series Data
#'
#' This function centralizes the space-time series data to its mean.
#'
#' @param data space-time series data in \code{\link{data.frame}} 
#'   or \code{\link{matrix}} object.
#' 
#' @details
#' The \code{\link{data}} which is either in \code{\link{data.frame}}
#' or \code{\link{matrix}} object should have at least 2 columns to meet 
#' the requirements for modeling STARIMA. The columns will be the spatial
#' locations of the data, and the rows would be the series of observations
#' from each location or space.
#' 
#' @export
#' @examples
#' # Remove the Year column in palay dataset, since we dont 
#' # need to centralize this.
#' palay1 <- subset(palay, select = -c(Year))
#' 
#' # Centralize the data
#' stcenter(palay1)
stcenter <- function(data){
  if(!is.data.frame(data))
    stop("data should be data.frame class")
  else if(is.data.frame(data)){
    if(nrow(data) == 0L)
      stop("data must have one or more observations")
    if(ncol(data) == 1L)
      stop("data must have at least 2 locations")
    row <- dim(data)[1L]
    col <- dim(data)[2L]
    mat <- matrix(NA, nrow = row, ncol = col)
    for(j in 1L:col){
      mat[, j] <- data[, j] - mean(data[, j])      
    }  
    return(as.data.frame(mat))
  }
}
