\name{CHILI}
\alias{CHILI}
\docType{data}
\title{
Swiss Surveillance Data on Influenza Like Illness, 2000-2016
}
\description{
  The \code{CHILI} dataset is a time series of the weekly number of
  ILI cases in Switzerland from 2000 to 2016,
  estimated from the Swiss Sentinella Reporting System.
}
\usage{data("CHILI")}
\format{
  a univariate time series of class \code{\link[zoo]{zoo}},
  where the time index is of class \code{\link{Date}}
  and always refers to the Tuesday of the notification week
}
\source{
The Swiss ILI data has been received on 19 January 2017 by courtesy of:

    Swiss Federal Office of Public Health\cr
    Public Health Directorate\cr
    Communicable Diseases Division\cr
    3003 Bern\cr
    SWITZERLAND
}
\examples{
summary(CHILI)
plot(CHILI)
}
\keyword{datasets}
