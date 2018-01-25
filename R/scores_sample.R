################################################################################
### Proper Scoring Rules based on Simulations
###
### Copyright (C) 2018 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Proper Scoring Rules based on Simulations
##'
##' This is a simple wrapper around functions from the \CRANpkg{scoringRules}
##' package to calculate scoring rules from simulation-based forecasts.
##' Calculation of the logarithmic score involves kernel density estimation,
##' see [scoringRules::logs_sample()].
##' The function is vectorized and preserves the dimension of the input.
##'
##' @param x a vector of observed counts.
##' @param sims a matrix of simulated counts with as many rows as `length(x)`.
##' @param which a character vector specifying which scoring rules to apply. The
##'     Dawid-Sebastiani score (`"dss"`) and the logarithmic score (`"logs"`)
##'     are available and both computed by default.
##' @return scores for the predictions of the observations in `x` (maintaining
##'     their dimensions).
##' @importFrom stats setNames
##' @importFrom scoringRules dss_sample logs_sample
##' @export

scores_sample <- function (x, sims, which = c("dss", "logs"))
{
    scorelist <- lapply(X = setNames(paste0(which, "_sample"), nm = which),
                        FUN = do.call,
                        args = alist(y = x, dat = sims),
                        envir = environment())  # to resolve x and sims
    simplify2array(scorelist, higher = TRUE)
}
