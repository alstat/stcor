\name{stcenter}
\alias{stcenter}
\title{Centralize Space-Time Series Data}
\usage{
stcenter(data)
}
\arguments{
  \item{data}{space-time series data in
  \code{\link{data.frame}} or \code{\link{matrix}} object.}
}
\description{
This function centralizes the space-time series data to its
mean.
}
\details{
The \code{\link{data}} which is either in
\code{\link{data.frame}} or \code{\link{matrix}} object
should have at least 2 columns to meet the requirements for
modeling STARIMA. The columns will be the spatial locations
of the data, and the rows would be the series of
observations from each location or space.
}
\examples{
# Remove the Year column in palay dataset, since we dont
# need to centralize this.
palay1 <- subset(palay, select = -c(Year))

# Centralize the data
stcenter(palay1)
}
