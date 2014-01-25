#' Space-Time Correlograms
#'
#' The stcor package provides functions for correlograms of the 
#' advanced time series model, the Space-Time Autoregressive Integrated 
#' Moving Average (STARIMA). These functions are the Space-Time 
#' Autocorrelation Function (\code{\link{stacf}}) and Space-Time Partial 
#' Autocorrelation Function (\code{\link{stpacf}}). Including function for 
#' centralizing (\code{\link{stcenter}}) the data to its mean, and a dataset
#' \code{\link{palay}}.
#' 
#' @author Al-Ahmadgaid Bahauddin Asaad <alstated@@gmail.com>
#' @seealso \href{http://alstat.github.io/stcor/}{stcor website}
#' @import reshape2 ggplot2
#' @export stcenter stacf stpacf stacovfCPP
#' @useDynLib stcor
#' @docType package
#' @name stcor-package
NULL
