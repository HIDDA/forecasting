################################################################################
### Plot One-Step-Ahead Forecasts with Scores
###
### Copyright (C) 2017-2018 Sebastian Meyer
###
### This file is part of the R package "HIDDA.forecasting",
### free software under the terms of the GNU General Public License, version 2,
### or (at your option) any later version, a copy of which is available at
### https://www.R-project.org/Licenses/.
################################################################################

##' Plot (One-Step-Ahead) Forecasts with Scores
##'
##' This function produces a fan chart of sequential (one-step-ahead) forecasts
##' with dots for the observed values, using [surveillance::fanplot()], which
##' itself wraps [fanplot::fan()]. A [matplot()] of score
##' values at each time point is added below ("slicing").
##' @param quantiles a time x `probs` matrix of forecast quantiles at each time
##'     point.
##' @param probs numeric vector of probabilities with values between 0 and 1.
##' @param means (optional) numeric vector of point forecasts at each time
##'     point.
##' @param observed (optional) numeric vector of observed values.
##' @param scores (optional) numeric vector (or matrix) of associated scores.
##' @param start time index (x-coordinate) of the first prediction.
##' @param xlab x-axis label.
##' @param fan.args a list of graphical parameters for the [fanplot::fan()],
##'     e.g., to employ a different [colorRampPalette()] as
##'     `fan.col`, or to enable contour lines via `ln`.
##' @param means.args a list of graphical parameters for [lines()]
##'     to modify the plotting style of the point predictions.
##' @param observed.args a list of graphical parameters for [lines()]
##'     to modify the plotting style of the `observed` values.
##' @param key.args if a list, a color key (in [fanplot::fan()]'s
##'     `"boxfan"`-style) is added to the fan chart. The list may include
##'     positioning parameters `start` (the x-position) and `ylim` (the y-range
##'     of the color key), `space` to modify the width of the color key, and
##'     `rlab` to modify the labels. An alternative way of labeling the
##'     quantiles is via the argument `ln` in `fan.args`.
##' @param ... further arguments are passed to [plot.default()].
##' @param scores.args a list of graphical parameters for [matplot()] to modify
##'     the style of the `scores` subplot at the bottom.
##' @param legend.args if a list (of parameters for [legend()]) and
##'     `ncol(scores) > 1`, a legend is added to the `scores` subplot.
##' @param heights numeric vector of length 2 specifying the relative height of
##'     the two subplots.
##' @author Sebastian Meyer
##' @importFrom utils modifyList packageVersion
##' @importFrom graphics par layout plot.default matplot matlines legend
##' @importFrom grDevices colorRampPalette
##' @export

osaplot <- function (quantiles, probs, means, observed, scores, start = 1,
                     xlab = "Time", fan.args = list(), means.args = list(),
                     observed.args = list(), key.args = list(), ...,
                     scores.args = list(), legend.args = list(),
                     heights = c(.6,.4))
{
    if (!requireNamespace("surveillance", quietly = TRUE) ||
        packageVersion("surveillance") < "1.16.0") {
        stop("surveillance (>= 1.16.0) is not installed")
    }

    ## modify the default surveillance::fanplot style
    stopifnot(is.list(fan.args))
    fan.args <- modifyList(
        list(fan.col = colorRampPalette(c("darkgreen", "gray90"))),
        fan.args, keep.null = TRUE)
    if (missing(means))
        means <- NULL
    if (missing(observed)) {
        observed <- NULL
    } else {
        stopifnot(is.list(observed.args))
        observed.args <- modifyList(
            list(type = "p", pch = 20),
            observed.args)
    }

    if (!missing(scores)) {
        ## setup layout for subplots
        stopifnot(length(heights) == 2)
        layout(cbind(1:2), heights = heights)
        omar <- par("mar")
        par(mar = omar - c(omar[1L],0,0,0), xaxt = "n")
        on.exit({layout(1L); par(mar = omar)})
    }

    ## create the fan chart
    surveillance::fanplot(
        quantiles = quantiles, probs = probs, means = means,
        observed = observed, start = start,
        fan.args = fan.args, means.args = means.args,
        observed.args = observed.args, key.args = key.args,
        xlab = "", ...)

    if (missing(scores))
        return(invisible())

    ## add scores
    if (is.vector(scores)) {
        scores <- cbind("Score" = scores)
    } else {
        stopifnot(is.matrix(scores))
        if (is.null(colnames(scores)))
            colnames(scores) <- seq_len(ncol(scores))
    }
    par(mar = omar - c(0,0,omar[3L],0), xaxt = "s")
    ## manually enable panel.first since matplot does not support it (@R-3.4.3)
    scores.args_call <- substitute(scores.args)
    if (!is.null(panel.first <- scores.args_call[["panel.first"]])) {
        scores.args_call[["panel.first"]] <- NULL
        scores.args <- eval.parent(scores.args_call)
    }
    scores.args <- modifyList(
        list(x = seq(from = start, by = 1, length.out = nrow(scores)),
             y = scores, type = "l", lty = 1:5, lwd = 2, pch = NULL, col = 1:6,
             xlab = xlab,
             ylab = if (ncol(scores) == 1) colnames(scores) else "Score"),
        scores.args)
    if (is.null(panel.first)) {
        do.call("matplot", scores.args)
    } else {
        do.call("plot.default", modifyList(scores.args,
            list(x = range(scores.args$x, finite = TRUE),
                 y = range(scores.args$y, finite = TRUE),
                 type = "n")))
        eval.parent(panel.first)
        do.call("matlines", scores.args)
    }

    ## add legend for the scores
    if (ncol(scores) > 1 && is.list(legend.args)) {
        legend.args <- modifyList(
            list(x = "topright", legend = colnames(scores),
                 lty = scores.args$lty, lwd = scores.args$lwd,
                 pch = scores.args$pch, col = scores.args$col,
                 bty = "n", cex = 0.8),
            legend.args)
        do.call("legend", legend.args)
    }
    return(invisible())
}
