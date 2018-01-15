################################################################################
### Proper Scoring Rules for Log-Normal Forecasts
###
### Copyright (C) 2017-2018 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Proper Scoring Rules for Log-Normal Forecasts
##'
##' This is a simple wrapper around functions from the \CRANpkg{scoringRules}
##' package for predictions with a [LN][plnorm](`meanlog`, `sdlog`)
##' distribution. The function is vectorized and preserves the dimension of
##' the input.
##'
##' @param x the observed counts.
##' @param meanlog,sdlog parameters of the log-normal distribution, i.e., mean
##'     and standard deviation of the distribution on the log scale.
##' @param which a character vector specifying which scoring rules to apply. The
##'     Dawid-Sebastiani score (`"dss"`) and the logarithmic score (`"logs"`)
##'     are available and both computed by default.
##' @return scores for the predictions of the observations in `x` (maintaining
##'     their dimensions).
##' @importFrom stats setNames
##' @importFrom scoringRules dss_lnorm logs_lnorm
##' @export

scores_lnorm <- function (x, meanlog, sdlog, which = c("dss", "logs"))
{
    scorelist <- lapply(X = setNames(paste0(which, "_lnorm"), nm = which),
                        FUN = do.call,
                        args = alist(y = x, meanlog = meanlog, sdlog = sdlog),
                        envir = environment())  # to resolve x, meanlog, sdlog
    simplify2array(scorelist, higher = TRUE)
}
