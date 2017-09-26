################################################################################
### Plot one-step-ahead predictive distributions together with scores
###
### Copyright (C) 2017 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Plot One-Step-Ahead Forecasts with Scores
##'
##' This function produces a fan chart of sequential one-step-ahead forecasts.
##' Dots are added for the observed values. A [graphics::matplot()] of score
##' values at each time point is added below ("slicing").
##' @param quantiles a time x `probs` matrix of forecast quantiles at each time
##'     point.
##' @param probs numeric vector of probabilities with values between 0 and 1.
##' @param observed numeric vector of observed values. Can be `NULL`.
##' @param scores numeric vector (or matrix) of associated scores.
##' @param start time index (x-coordinate) of the first prediction.
##' @param xlab x-axis label.
##' @param fan.args a list of graphical parameters for the [fanplot::fan()],
##'     e.g., to employ a different [grDevices::colorRampPalette()] as
##'     `fan.col`, or to enable contour lines via `ln`.
##' @param observed.args a list of graphical parameters for [graphics::lines()]
##'     to modify the plotting style of the `observed` values.
##' @param key.args if a list, a color key (in [fanplot::fan()]'s
##'     `"boxfan"`-style) is added to the fan chart. The list may include
##'     positioning parameters `start` (the x-position) and `ylim` (the y-range
##'     of the color key), `space` to modify the width of the boxfan, and `rlab`
##'     to modify the labels. An alternative way of labeling the quantiles is
##'     via the argument `ln` in `fan.args`.
##' @param ... further arguments are passed to [graphics::plot.default()].
##' @param heights numeric vector of length 2 specifying the relative height of
##'     the two subplots.
##' @author Sebastian Meyer
##' @importFrom utils modifyList
##' @importFrom graphics par layout matplot
##' @importFrom grDevices colorRampPalette
##' @export

osaplot <- function (quantiles, probs, observed, scores, start = 1, xlab = "Time",
                     fan.args = list(), observed.args = list(), key.args = list(),
                     ..., heights = c(2,1))
{
    stopifnot(is.list(fan.args), is.list(observed.args), length(heights) == 2)
    if (is.vector(scores)) scores <- cbind(score = scores)

    ## modify defaults
    fan.args <- modifyList(
        list(fan.col = colorRampPalette(c("darkgreen", "gray90"))),
        fan.args)
    observed.args <- modifyList(
        list(type = "p", pch = 20),
        observed.args)

    layout(cbind(1:2), heights = heights)
    omar <- par("mar")
    on.exit(par(omar))
    par(mar = omar - c(omar[1L],0,0,0), xaxt = "n")
    surveillance:::fanplot(
        quantiles = quantiles, probs = probs, observed = observed,
        start = start, fan.args = fan.args, observed.args = observed.args,
        key.args = key.args,  xlab = "", ...)
    ## add scores
    par(mar = omar - c(0,0,omar[3L],0), xaxt = "s")
    matplot(x = seq(from = start, by = 1, length.out = nrow(scores)),
            y = scores, type = "l", xlab = xlab,
            ylab = if (ncol(scores) == 1) colnames(scores) else "Score")
}
