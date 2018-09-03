# The R package `HIDDA.forecasting`

The *Handbook of Infectious Disease Data Analysis*
("HIDDA", to be published by Chapman & Hall/CRC)
contains a chapter on "Forecasting Based on Surveillance Data"
by Leonhard Held and Sebastian Meyer.
The R package **HIDDA.forecasting** provides the data and code to
reproduce results from the two applications presented in that chapter:

1. Univariate forecasting of Swiss ILI counts,
   available as `data("CHILI", package = "HIDDA.forecasting")`, using

    * [**forecast**](https://CRAN.R-project.org/package=forecast)`::arima()`

    * [**glarma**](https://CRAN.R-project.org/package=glarma)`::glarma()`

    * [**surveillance**](https://CRAN.R-project.org/package=surveillance)`::hhh4()`

    * [**prophet**](https://CRAN.R-project.org/package=prophet)`::prophet()`

2. Age-stratified analysis of norovirus counts in Berlin, available from
   [**hhh4contacts**](https://CRAN.R-project.org/package=hhh4contacts)`::noroBE()`,
   using the multivariate time-series model `hhh4` from **surveillance**


## Documentation

The package contains several vignettes which document each of the methods
applied in the book chapter. An online version of the documentation is
available at

<!-- TODO: pkgdown -->


## Installation

To compile the vignettes on your machine, you can install
**HIDDA.forecasting** and the other required packages with

```r
# install.packages("remotes")
remotes::install_github("HIDDA/forecasting", dependencies = TRUE)
```

and then, e.g.,

```r
browseVignettes("HIDDA.forecasting")
```

Note that the "CHILI" vignettes include `source("setup.R")`,
which refers to a small R script available from the installed package at

```r
system.file("doc", "setup.R", package = "HIDDA.forecasting")
```
