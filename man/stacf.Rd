\name{stacf}
\alias{stacf}
\alias{stpacf}
\title{Space-Time Autocorrelation Function (STACF) and Partial
Autocorrelation Function (STPACF)}
\usage{
stacf(data, max.timelag = 15, wmatrices = list(NULL, ...),
  conf.level = 0.95)

stpacf(data, max.timelag = 15, wmatrices = list(NULL, ...),
  conf.level = 0.95)
}
\arguments{
  \item{data}{space-time series data in
  \code{\link{data.frame}} or \code{\link{matrix}} object.}

  \item{max.timelag}{the maximum time lag at which to
  calculate the STACF, default is set to 15.}

  \item{wmatrices}{the weights matrices of the spatial
  order.}

  \item{conf.level}{the confidence level for significant
  lags.}
}
\description{
The \code{stacf} and \code{stpacf} estimate the
autocorrelation function and partial autocorrelation
function of the space-time series, respectively.
}
\details{
The \code{\link{data}} which is either in
\code{\link{data.frame}} or \code{\link{matrix}} object
should have at least 2 columns to meet the requirements of
STARIMA model. The columns will be the spatial locations of
the data, and the rows will be the series of observations
from each location or space. Analogous to the univariate
time series, the STACF determines the lag order of the
space-time moving average (STMA) model both for time and
space; and STPACF identifies the time and spatial lag order
of the space-time autoregressive (STAR) model.

The STPACF is computed using the space-time analogue of the
Yule-Walker equation for univariate time series (Pfeifer
and Deutsch, 1980 pg. 39).
\deqn{\gamma_{h0}(s)=\sum_{j=1}^k\sum_{l=0}^{\lambda}\phi_{jl}\gamma_{hl}(s-j)}
for \eqn{s = 1, 2, ..., k} and \eqn{h = 0, 1, 2, ...,
\lambda}. The last coefficient \eqn{\hat{\phi}_{kl}}
obtained from solving the system of equations as \eqn{l =
0, 1, 2, ..., \lambda} for \eqn{k = 1, 2, ...} is called
the space-time partial correlation function of spatial
order \eqn{\lambda}.
}
\examples{
## The package contains dataset named palay. Run ?palay
## to get help about the data. The data has 6 regions that
## can be assigned into the following weights matrices
## using the equal weighting of spatial order.

# First spatial order weights matrix
W1 <- rbind(c(0, 1/2, 1/2, 0, 0, 0),
            c(1/2, 0, 1/2, 0, 0, 0),
            c(1/3, 1/3, 0, 1/3, 0, 0),
            c(0, 0, 1/2, 0, 1/2, 0),
            c(0, 0, 0, 1/2, 0, 1/2),
            c(0, 0, 0, 1/2, 1/2, 0))

# Second spatial order weights matrix
W2 <- rbind(c(0, 0, 0, 1, 0, 0),
            c(0, 0, 0, 1, 0, 0),
            c(0, 0, 0, 0, 1, 0),
            c(0, 0, 0, 0, 0, 1),
            c(0, 0, 1, 0, 0, 0),
            c(0, 0, 1, 0, 0, 0))

# Third spatial order weights matrix
W3 <- rbind(c(0, 0, 0, 0, 1, 0),
            c(0, 0, 0, 0, 1/2, 1/2),
            c(0, 0, 0, 0, 0, 1),
            c(1, 0, 0, 0, 0, 0),
            c(1/2, 1/2, 0, 0, 0, 0),
            c(0, 1, 0, 0, 0, 0))

## It is required to centralize the data to its mean for
## STARIMA modeling, but before doing so, the Year column
## of the data must be removed since we do not have to
## centralize this,
palay1 <- subset(palay, select = -c(Year))

## Now, centralize the data
palay.cen <- stcenter(palay1)

## Finally, Compute the STACF and STPACF
stacf(data = palay.cen, wmatrices = list(NULL, W1, W2, W3))
stpacf(data = palay.cen, wmatrices = list(NULL, W1, W2, W3))
}
\references{
Box, G. E.P. et al. (1994). \emph{Time Series Analysis:
Forecasting and Control}. New Jersey: Prentice-Hall.

Pfeifer, P. E. and Deutsch, S. J. (1980a). A three-stage
iterative procedure for space-time modelling.
\emph{Technometrics}, volume 22, pg. 35-47.

Pfeifer, P. E. and Deutsch, S. J. (1980b). Identification
and interpretation of first order space-time ARMA models.
\emph{Technometrics}, volume 22, pg. 397-408.

Pfeifer P. E. and Deutsch, S. J. (1981). Seasonal
space-time ARMA modelling. \emph{Geographical Analysis},
volume 13, pg. 117-133.
}
\seealso{
\href{http://alstat.github.io/stcor/}{stcor website}
}
