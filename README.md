# The R package HIDDA.forecasting

The *Handbook of Infectious Disease Data Analysis*
(Chapman & Hall/CRC, 2019)
contains a chapter on "Forecasting Based on Surveillance Data"
([preprint](https://arxiv.org/abs/1809.03735))
by Leonhard Held and Sebastian Meyer.
The R package **HIDDA.forecasting** provides the data and code to
reproduce results from the two applications presented in that chapter:

1. Univariate forecasting of
   [Swiss ILI counts](https://HIDDA.github.io/forecasting/articles/CHILI.html)
   using

    * [**forecast**](https://CRAN.R-project.org/package=forecast)`::auto.arima()`
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
[released **HIDDA.forecasting** package](https://github.com/HIDDA/forecasting/releases)
(with pre-built [vignettes](https://HIDDA.github.io/forecasting/articles/))
together with other packages required to run the analyses:

```r
# install.packages("remotes")
remotes::install_url("https://github.com/HIDDA/forecasting/releases/download/v1.1.1/HIDDA.forecasting_1.1.1.tar.gz", dependencies = TRUE)
```

Alternatively, to install **HIDDA.forecasting** from the current sources,
building the [vignettes](https://HIDDA.github.io/forecasting/articles/)
on your machine, you could use:

```r
# install.packages("devtools")
devtools::install_github("HIDDA/forecasting", dependencies = TRUE, build_vignettes = TRUE)
```

To browse the installed vignettes:

```r
browseVignettes("HIDDA.forecasting")
```

Note that the "CHILI" vignettes include `source("setup.R")`,
which refers to a small R script available from the installed package at

```r
system.file("doc", "setup.R", package = "HIDDA.forecasting")
```
