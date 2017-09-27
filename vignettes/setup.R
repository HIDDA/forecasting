library("HIDDA.forecasting")

## define start times of 30-week-ahead test periods
.T <- match(paste0(2013:2015, "-12"), strftime(index(CHILI), "%Y-%m"))
index(CHILI)[.T]
TEST <- lapply(.T, function (T) seq(from = T, by = 1, length.out = 30))
TRAIN <- lapply(TEST, function (test) 1:(test[1]-1))

## at which time points to compute one-week-ahead forecasts
OWA <- tail(TRAIN[[1]],1):(length(CHILI)-1)
length(OWA)
