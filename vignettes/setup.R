library("HIDDA.forecasting")

## define start times of 30-week-ahead test periods
.T <- match(paste0(2013:2015, "-12"), strftime(index(CHILI), "%Y-%m"))
index(CHILI)[.T]
TEST <- lapply(.T, function (T) seq(from = T, by = 1, length.out = 30))

## at which time points to compute one-week-ahead forecasts
OWA <- (TEST[[1]][1]-1):(length(CHILI)-1)
length(OWA)

## auxiliary function for annotation
format_period <- function (index, fmt = "%Y-W%V", collapse = " to ") {
    paste0(strftime(index(CHILI)[range(index)], fmt), collapse = collapse)
}
