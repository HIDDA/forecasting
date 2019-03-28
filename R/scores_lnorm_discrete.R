################################################################################
### Proper Scoring Rules for *Discretized* Log-Normal Forecasts
###
### Copyright (C) 2019 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Proper Scoring Rules for *Discretized* Log-Normal Forecasts
##'
##' Compute scores for discretized log-normal forecasts.
##' The function is vectorized and preserves the dimension of the input.
##'
##' @inheritParams scores_lnorm
##' @return scores for the predictions of the observations in `x` (maintaining
##'     their dimensions).
##' @importFrom stats setNames plnorm qlnorm
##' @export

scores_lnorm_discrete <- function (x, meanlog, sdlog, which = c("dss", "logs"))
{
    scorelist <- lapply(X = setNames(paste0(which, "_lnorm_discrete"), nm = which),
                        FUN = do.call,
                        args = alist(y = x, meanlog = meanlog, sdlog = sdlog),
                        envir = environment())  # to resolve x, meanlog, sdlog
    simplify2array(scorelist, higher = TRUE)
}

## log-score for a discretized log-normal forecast
logs_lnorm_discrete <- function (y, meanlog = 0, sdlog = 1)
{
    -logdlnorm_discrete(y, meanlog, sdlog)
}

## compute (log-)probabilty of x according to discretized LN
dlnorm_discrete <- function (x, meanlog = 0, sdlog = 1, log = FALSE)
{
    if (log) {
        logdlnorm_discrete(x, meanlog, sdlog)
    } else {
        plnorm(x + 0.5, meanlog, sdlog) - plnorm(x - 0.5, meanlog, sdlog)
    }
}
logdlnorm_discrete <- function (x, meanlog = 0, sdlog = 1)
{
    ## compute log(1 - exp(x)), R translation of R_Log1_Exp from src/nmath/dpq.h
    log_1mexp <- function (x) {
        ifelse(x > -log(2), log(-expm1(x)), log1p(-exp(x)))
    }

    ## compute log (exp (logx) - exp (logy)), C version in src/nmath/pgamma.c
    logspace_sub <- function (logx, logy) {
        logx + log_1mexp(logy - logx)
    }

    logspace_sub(plnorm(x + 0.5, meanlog, sdlog, log.p = TRUE),
                 plnorm(x - 0.5, meanlog, sdlog, log.p = TRUE))
}

## compute Dawid-Sebastiani score for discretized LN
dss_lnorm_discrete <- function (y, meanlog = 0, sdlog = 1)
{
    ## mean is preserved
    m <- exp(meanlog + sdlog^2/2)

    ## variance is increased by 1/12 through rounding
    ## (for reasonably large original mean and variance)
    ## see also https://stats.stackexchange.com/questions/209260
    v0 <- m^2 * expm1(sdlog^2)
    if (any(m < 3 | v0 < 0.25))
        warning("unsuitable approximation")
    v <- v0 + 1/12
    ## v_approx <- mapply(FUN = function (m, s) {
    ##     xgrid <- floor(qlnorm(1e-12, m, s)) : ceiling(qlnorm(1-1e-12, m, s))
    ##     p <- dlnorm_discrete(xgrid, m, s)
    ##     sum(p * xgrid^2) - sum(p * xgrid)^2
    ## }, m = meanlog, s = sdlog, USE.NAMES = FALSE)

    (y - m)^2 / v + log(v)
}
