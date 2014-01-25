#' @rdname stacf
stpacf <- function(data, max.timelag = 15, wmatrices = list(NULL, ...), conf.level = 0.95){
  a.mat <- matrix(NA, nrow = max.timelag * (length(wmatrices)), ncol = 1L)
  b.mat <- matrix(NA, nrow = max.timelag * (length(wmatrices)), 
                  ncol = max.timelag * (length(wmatrices)))
  c.mat <- matrix(NA, nrow = max.timelag * (length(wmatrices)), ncol = 1L)
  d.mat <- matrix(NA, nrow = max.timelag, ncol = length(wmatrices))
 
  if(!is.data.frame(data) && !is.matrix(data))
    stop("data should be data.frame or matrix object")
  else if(is.data.frame(data) || is.matrix(data))
    data = t(as.matrix(data))
  if(nrow(data) == 0L)
    stop("data must have one or more observations")
  if(ncol(data) == 1L)
    stop("data must have at least 2 locations")
  
  i.max = 0L
  for(j in 1L:max.timelag){
    for(i in 1L:length(wmatrices)){
      a.mat[i + i.max, ] <- stacovfCPP(data, wmatrix1 = wmatrices[[i]], timelag = j)
    }
    i.max = i * j
  }
  i.max = 0L
  k.max = 0L
  for(l in 1L:max.timelag){
    for(k in 1L:length(wmatrices)){
      for(j in 1L:max.timelag){
        for(i in 1L:length(wmatrices)){
          b.mat[i + i.max, k + k.max] <- stacovfCPP(data, wmatrix1 = wmatrices[[i]], wmatrix2 = wmatrices[[k]], timelag = j - l)
        }
        i.max = i * j
      }
      i.max = 0L
    }
    k.max = k * l 
  }
  b.matinv <- solve(b.mat)
  c.mat <- b.matinv %*% a.mat
  d.mat <- matrix(c.mat, nrow = max.timelag, ncol = length(wmatrices), byrow = TRUE)
  
  melt.df <- melt(d.mat)
  sp.lag <- max(melt.df[, 2L]) - 1L
  lab <- rep(paste("Spatial Lag", 0L:sp.lag), each = max.timelag)
  melt.df[, 2L] <- lab
  names(melt.df) <- c("X1", "X2", "Values")
  colnames(d.mat) <- paste("Spatial Lag", 0L:sp.lag)
  
  ciline <- qnorm((1L - conf.level)/2L)/sqrt(ncol(data))
  
  p <- qplot(x = X1, 
             y = Values, 
             data = melt.df, 
             fill = X2 , 
             stat= "identity",
             position = "identity", 
             geom = "bar") + 
    xlab(expression(bold("Time Lag"))) +
    ylab(expression(bold("Space-Time Partial Autocorrelation"))) +
    geom_hline(
      yintercept = -ciline,
      color = "blue", 
      size = 0.2) + 
    geom_hline(
      yintercept = ciline,
      color = "blue", 
      size = 0.2) +
    theme(
      legend.position= "FALSE") + 
    facet_grid(X2 ~ .)
  
  print(p)
  
  return(d.mat)
}
