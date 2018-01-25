################################################################################
### Derive Probability Mass Functions from hhh4 Simulations
###
### Copyright (C) 2018 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

## derive the mean values on which the samples are based (in simHHH4)
## NOTE: it would be more efficient to have these returned from simHHH4 as well
means_hhh4sims <- function (sims, model)
{
    stopifnot(inherits(sims, "hhh4sims"), inherits(model, "hhh4"))
    if (!is.array(sims))
        stop("only implemented for simulations with 'simplify = TRUE'")

    simSubset <- as.integer(rownames(sims))
    stsObj <- model$stsObj
    nsim <- dim(sims)[3L]

    getMeans1 <- function (i) {  # from the i'th simulation
        stsObj@observed[simSubset,] <- sims[,,i,drop=FALSE]
        terms <- surveillance:::interpretControl(model$control, stsObj)
        surveillance::meanHHH(model$coefficients, terms, simSubset, TRUE)
    }
    means <- vapply(X = seq_len(nsim), FUN = getMeans1,
                    FUN.VALUE = matrix(0, length(simSubset), model$nUnit),
                    USE.NAMES = FALSE)
    dimnames(means) <- dimnames(sims)

    ## at the first time point, means are the same for all simulations
    ## stopifnot(length(unique.default(means[1,1,])) == 1)

    return(means)
}


##' \code{hhh4}-Based Forecast Distributions
##'
##' The function \code{dhhh4sims} constructs a (non-vectorized)
##' probability mass function from the result of
##' \code{\link[surveillance]{simulate.hhh4}} (and the corresponding
##' model), as a function of the time point within the simulation period.
##' The distribution at each time point is obtained as a mixture of
##' negative binomial (or Poisson) distributions based on the samples from
##' the previous time point.
##'
##' @param sims a \code{"hhh4sims"} object from
##'     \code{\link[surveillance]{simulate.hhh4}}.
##' @param model the \code{"\link[surveillance]{hhh4}"} object underlying
##'     \code{sims}.
##' @return a \code{function(x, tp = 1, log = FALSE)}, which takes a
##'     vector of \code{model$nUnit} counts and calculates the
##'     (\code{log}-)probability of observing these counts (given the
##'     \code{model}) at the \code{tp}'th time point of the simulation
##'     period.
##' @keywords distribution
##' @author Sebastian Meyer
##' @references
##'     \Sexpr[stage=build,results=rd]{tools::toRd(citation("HIDDA.forecasting"))}
##' @seealso \code{\link{logs_hhh4sims}} where this function is used.
##' @noMd
##' @export
dhhh4sims <- function (sims, model)
{
    stopifnot(inherits(sims, "hhh4sims"), inherits(model, "hhh4"))

    ## environment for the resulting functions
    env <- new.env(parent = getNamespace("stats"))

    ## number of units
    env$nUnit <- model$nUnit

    ## sequential means on which the samples were based (in simHHH4)
    env$means <- means_hhh4sims(sims, model)  # array with same dim() as sims

    ## check against meanHHH() output for the first time point of the simulation
    ## means_observed <- surveillance::meanHHH(model$coefficients, stats::terms(model),
    ##                                         subset = as.integer(rownames(sims)))
    ## stopifnot(all(means_observed$mean[1,] == env$means[1,,]))

    ## overdispersion is time-constant (of length 1 or nUnit)
    env$size <- c(surveillance::sizeHHH(model$coefficients, stats::terms(model), subset = 1))

    ## one-step-ahead distribution
    env$dOSA <- with(env, if (is.null(size)) {
        dpois
    } else {
        ## "formals<-"(dnbinom, value = modifyList(formals(dnbinom), list(size = size)))
        function (x, mu) dnbinom(x, mu = mu, size = size)
    })

    ## PMFs are mixtures of OSA distributions (with equal weight)
    dfun <- with(env, function(x, tp = 1, log = FALSE) {
        dsamples <- dOSA(x, means[tp,,])  # drop = TRUE
        ## if nUnit == 1, dsamples is a vector else a nUnit x nsim matrix
        prob <- if (length(x) == 1) {
            mean(dsamples)
        } else if (length(x) == nUnit) {
            .rowMeans(dsamples, nUnit, ncol(dsamples))
        } else stop("'x' must have length ", nUnit)
        if (log) log(prob) else prob
    })

    dfun
}

## check distribution at the first time point
## stopifnot(identical(
##     sapply(0:20, dhhh4sims(hhh4sim1, hhh4fit)),
##     dnbinom(0:20,
##             mu = meanHHH(hhh4fit$coefficients, terms(hhh4fit), subset = as.integer(rownames(hhh4sim1)[1]), total.only = TRUE),
##             size = sizeHHH(hhh4fit$coefficients, terms(hhh4fit), subset = as.integer(rownames(hhh4sim1)[1])))
## ))

## visually compare distribution against simulations
## dfun <- dhhh4sims(hhh4sim1, hhh4fit)
## par(mfrow = n2mfrow(nrow(hhh4sim1)))
## for (tp in 1:nrow(hhh4sim1)) {
##     MASS::truehist(hhh4sim1[tp,,])
##     lines(0:10000, sapply(0:10000, dfun, tp = tp))
## }


##' Simulation-Based Logarithmic Score Using \code{dhhh4sims}
##'
##' The function `logs_hhh4sims` computes the logarithmic score of the
##' forecast distributions based on a [surveillance::hhh4()] `model` and
##' [simulations][surveillance::simulate.hhh4] (`sims`) thereof. The
##' forecast distributions are obtained via [dhhh4sims()] as sequential
##' mixtures of negative binomial (or Poisson) distributions, which is
##' different from the kernel density estimation approach employed in
##' [scores_sample()].
##'
##' @param sims a `"hhh4sims"` object from
##'     [surveillance::simulate.hhh4()].
##' @param model the [surveillance::hhh4()] fit underlying `sims`.
##' @param observed a vector or matrix of observed counts during the
##'     simulation period. By default (`NULL`), this is taken from
##'     `attr(sims, "stsObserved")`.
##' @return a vector or matrix of log-scores for the `observed` counts.
##' @keywords univar
##' @author Sebastian Meyer
##' @seealso [scores_sample()] for an alternative approach of calculating
##'     the logarithmic score from simulation-based forecasts
##' @export
logs_hhh4sims <- function (sims, model, observed = NULL)
{
    dfun <- dhhh4sims(sims, model)  # checks the classes of the arguments
    observed <- if (is.null(observed)) {
        attr(sims, "stsObserved")@observed
    } else {
        stopifnot(NROW(observed) == nrow(sims), NCOL(observed) == model$nUnit)
        as.matrix(observed)
    }
    -vapply(X = seq_len(nrow(observed)),
            FUN = function (tp) dfun(observed[tp,], tp, log = TRUE),
            FUN.VALUE = numeric(ncol(observed)), USE.NAMES = FALSE)
}
