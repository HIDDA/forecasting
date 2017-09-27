################################################################################
### Proper Scoring Rules for Log-Normal Forecasts
###
### Copyright (C) 2017 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Proper Scoring Rules for Log-Normal Forecasts
##'
##' Predictions are assumed to have a [LN][plnorm](`meanlog`, `sdlog`)
##' distribution. The functions are vectorized and preserve the dimensions of
##' the input.
##'
##' @param x the observed counts.
##' @param meanlog,sdlog parameters of the log-normal distribution, i.e., mean
##'     and standard deviation of the distribution on the log scale.
##' @return scores for the predictions of the observations in `x` (maintaining
##'     their dimensions).
##' @author Sebastian Meyer
##' @seealso The R package \CRANpkg{scoringRules} implements the logarithmic
##'     score and the continuous ranked probability scores (CRPS) for many
##'     continuous distributions.
##' @name scores_lnorm
NULL

##' @describeIn scores_lnorm Computes the Dawid-Sebastiani score (DSS) for the
##'     log-normal distribution similar to [surveillance::dss()] for the Poisson
##'     and negative binomial distribution.
##' @export
dss_lnorm <- function (x, meanlog, sdlog)
{
    varlog <- sdlog^2
    surveillance:::.dss(meanP = exp(meanlog + varlog/2),
                        varP = exp(2*meanlog + varlog) * expm1(varlog),
                        x = x)
}

##' @describeIn scores_lnorm Computes the logarithmic score (LogS) for the
##'     log-normal distribution similar to [surveillance::logs()] for the Poisson
##'     and negative binomial distribution.
##' @importFrom stats dlnorm
##' @export
logs_lnorm <- function (x, meanlog, sdlog)
{
    -dlnorm(x = x, meanlog = meanlog, sdlog = sdlog, log = TRUE)
}
