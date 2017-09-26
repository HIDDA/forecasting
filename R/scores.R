################################################################################
### Proper scoring rules
###
### Copyright (C) 2017 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Dawid-Sebastiani Score (DSS) for Log-Normal Predictions
##'
##' Computes the DSS for the log-normal distribution similar to
##' [surveillance::dss()] for the Poisson and negative binomial distribution.
##' Predictions are assumed to have a [LN][plnorm](`meanlog`, `sdlog`)
##' distribution. The function is vectorized and preserves the dimensions of the
##' input.
##'
##' @param x the observed counts.
##' @param meanlog,sdlog parameters of the log-normal distribution, i.e., mean
##'     and standard deviation of the distribution on the log scale.
##' @return scores for the predictions of the observations in `x` (maintaining
##'     their dimensions).
##' @author Sebastian Meyer
##' @export

dss_lnorm <- function (x, meanlog, sdlog)
{
    varlog <- sdlog^2
    surveillance:::.dss(meanP = exp(meanlog + varlog/2),
                        varP = exp(2*meanlog + varlog) * expm1(varlog),
                        x = x)
}
