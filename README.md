# The R package HIDDA.forecasting

The *Handbook of Infectious Disease Data Analysis*
("HIDDA", to be published by Chapman & Hall/CRC)
contains a chapter on "Forecasting Based on Surveillance Data"
by Leonhard Held and Sebastian Meyer.
The R package **HIDDA.forecasting** provides the data and code to
reproduce results from the two applications presented in that chapter:

1. Univariate forecasting of
   [Swiss ILI counts](https://HIDDA.github.io/forecasting/articles/CHILI.html)
   using

    * [**forecast**](https://CRAN.R-project.org/package=forecast)`::arima()`
      --> `vignette("CHILI_arima")`

    * [**glarma**](https://CRAN.R-project.org/package=glarma)`::glarma()`
      --> `vignette("CHILI_glarma")`

    * [**surveillance**](https://CRAN.R-project.org/package=surveillance)`::hhh4()`
      --> `vignette("CHILI_hhh4")`

    * [**prophet**](https://CRAN.R-project.org/package=prophet)`::prophet()`
      --> `vignette("CHILI_prophet")`

    * experimental [**kcde**](https://github.com/reichlab/kcde)`::kcde()`
      --> `vignette("CHILI_kcde")`

    * and [**MASS**](https://CRAN.R-project.org/package=MASS)`::fitdistr()` for naive reference forecasts
      --> `vignette("CHILI_naive")`

2. Age-stratified analysis of norovirus counts in Berlin using "hhh4"
   --> `vignette("BNV")`


## Installation

To install the
[released **HIDDA.forecasting** package](https://github.com/HIDDA/forecasting/releases/tag/v1.0.0)
(containing the built [vignettes](https://HIDDA.github.io/forecasting/articles/))
and the other packages required to run the vignettes, you could use:

```r
# install.packages("remotes")
remotes::install_url("https://github.com/HIDDA/forecasting/releases/download/v1.0.0/HIDDA.forecasting_1.0.0.tar.gz", dependencies = TRUE)
```

To browse the installed vignettes:

```r
browseVignettes("HIDDA.forecasting")
```

Alternatively, to install **HIDDA.forecasting** from the current sources,
building the [vignettes](https://HIDDA.github.io/forecasting/articles/)
on your machine, you can use:

```r
# install.packages("devtools")
devtools::install_github("HIDDA/forecasting", dependencies = TRUE, build_vignettes = TRUE)
```

Note that the "CHILI" vignettes include `source("setup.R")`,
which refers to a small R script available from the installed package at

```r
system.file("doc", "setup.R", package = "HIDDA.forecasting")
```
