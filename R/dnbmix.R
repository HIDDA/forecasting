################################################################################
### Derive Probability Mass Functions from Simulation-Based NegBin Forecasts
###
### Copyright (C) 2018 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Simulation-Based Forecast Distributions
##'
##' The function `dnbmix()` constructs a (vectorized) probability mass
##' function from a matrix of (simulated) `means` and corresponding `size`
##' parameters, as a function of the time point (row of `means`) within
##' the simulation period. The distribution at each time point is obtained
##' as a mixture of negative binomial (or Poisson) distributions.
##'
##' @param means a `n.ahead` x `n.sim` matrix of means.
##' @param size the dispersion parameter of the [dnbinom()] distribution
##'     or `NULL` (Poisson forecasts). Can also be time-varying (of length
##'     `n.ahead`).
##' @return a `function(x, tp = 1, log = FALSE)`, which takes a vector of
##'     counts `x` and calculates the (`log`-)probabilities of observing
##'     each of these numbers at the `tp`'th time point of the simulation
##'     period (indexing the rows of `means`).
##' @keywords distribution
##' @author Sebastian Meyer
##' @seealso [logs_nbmix()] where this function is used.
##' @examples
##' ## a GLARMA example
##' library("glarma")
##' y <- as.vector(CHILI)
##'
##' ## fit a simple NegBin-GLARMA model
##' X <- t(sapply(2*pi*seq_along(y)/52.1775,
##'               function (x) c(sin = sin(x), cos = cos(x))))
##' X <- cbind(intercept = 1, X)
##' fit <- glarma(y = y[1:883], X = X[1:883,], type = "NegBin", phiLags = 1)
##'
##' ## simulate the last four weeks (only 500 runs, for speed)
##' set.seed(1)
##' means <- replicate(500, {
##'     forecast(fit, n.ahead = 4, newdata = X[884:887,], newoffset = rep(0,4))$mu
##' })
##'
##' ## derive the weekly forecast distributions
##' dfun <- dnbmix(means, coef(fit, type = "NB"))
##' dfun(4000, tp = 1)
##' dfun(4000, tp = 4)
##' curve(dfun(x, tp = 4), 0, 30000, type = "h",
##'       main = "4-weeks-ahead forecast",
##'       xlab = "No. infected", ylab = "Probability")
##'
##' \dontshow{
##' ## verify distribution at the first time point (i.e., one-step-ahead NegBin)
##' stopifnot(identical(
##'     dfun(0:100, tp = 1),
##'     dnbinom(0:100,
##'             mu = forecast(fit, n.ahead=1, newdata=X[884,,drop=FALSE])$mu,
##'             size = coef(fit, type = "NB"))
##' ))
##' ## check that we have a probability distribution at the second time point
##' .xgrid <- seq(0, 200000, by = 500)
##' stopifnot(abs(1 -
##'     integrate(approxfun(.xgrid, dfun(.xgrid, tp = 2)), 0, 200000)$value
##' ) < 0.01)
##' }
##' @export
dnbmix <- function (means, size = NULL)
{
    stopifnot(is.matrix(means),
              is.null(size) || is.vector(size, mode = "numeric"))

    ## environment for the resulting functions
    env <- new.env(parent = getNamespace("stats"))
    env$means <- means
    env$size <- if (!is.null(size)) rep_len(size, nrow(means))

    ## one-step-ahead distribution
    env$dOSA <- with(env, if (is.null(size)) {
        function (x, mu, size) dpois(x, mu)
    } else {
        function (x, mu, size) dnbinom(x, mu = mu, size = size)
    })

    ## PMFs are mixtures of OSA distributions (with equal weight)
    env$nsim <- ncol(means)
    dfun <- with(env, function(x, tp = 1, log = FALSE) {
        if (length(tp) != 1) stop("'tp' must have length 1")
        dsamples <- vapply(X = x, FUN = dOSA, means[tp,], size[tp],
                           FUN.VALUE = numeric(nsim), USE.NAMES = FALSE)
        prob <- .colMeans(dsamples, nsim, length(x))
        if (log) log(prob) else prob
    })

    return(dfun)
}


##' Simulation-Based Logarithmic Score Via `dnbmix`
##'
##' The function `logs_nbmix` computes the logarithmic score of forecasts
##' based on mixtures of negative binomial (or Poisson) distributions via
##' [dnbmix()]. This is different from the kernel density estimation
##' approach available via [scores_sample()].
##'
##' @inheritParams dnbmix
##' @param observed a vector of observed counts during the simulation
##'     period.
##' @return a vector of log-scores for the `observed` counts.
##' @keywords univar
##' @author Sebastian Meyer
##' @seealso [scores_sample()] for an alternative approach of calculating
##'     the logarithmic score from simulation-based forecasts
##' @export
logs_nbmix <- function (observed, means, size)
{
    dfun <- dnbmix(means, size)
    stopifnot(is.vector(observed, mode = "numeric"),
              length(observed) == nrow(means))
    -vapply(X = seq_along(observed),
            FUN = function (tp) dfun(observed[tp], tp, log = TRUE),
            FUN.VALUE = 0, USE.NAMES = FALSE)
}
