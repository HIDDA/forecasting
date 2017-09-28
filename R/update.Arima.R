################################################################################
### Refit an ARIMA Model on a Subset of the Time Series
###
### Copyright (C) 2017 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Refit an ARIMA Model on a Subset of the Time Series
##'
##' There seems to be no function in package \CRANpkg{forecast} (as of version
##' 8.2) to re-estimate an ARIMA model on a subset of the original time series.
##' This `update` method does exactly that.
##' @param object an object of class `"Arima"`, e.g., from
##'     [forecast::auto.arima()].
##' @param subset an integer vector selecting part of the original time series
##'     (and external regressors).
##' @param ... further arguments to be passed to [arima()].
##' @return the updated model.
##' @author Sebastian Meyer
##' @importFrom stats update
##' @export

update.Arima <- function (object, subset, ...)
{
    x <- object$x
    xreg <- object$xreg  # arima() does not store this, Arima() does
    if (!missing(subset)) {
        x <- x[subset]
        xreg <- xreg[subset,,drop=FALSE]
    }
    ordseas <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]  # see 'arimaorder'
    res <- forecast::Arima(x,
        order = ordseas[1:3],
        seasonal = list(order = ordseas[4:6], period = ordseas[7]),
        xreg = xreg,
        include.mean = "intercept" %in% names(object$coef),
        include.drift = "drift" %in% names(object$coef),
        lambda = object$lambda, ...)
    res$call$xreg <- xreg  # otherwise stats:::predict.Arima would not work
    res
}
