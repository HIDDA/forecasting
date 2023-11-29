# HIDDA.forecasting 1.1.2 (2023-11-29)

* Vignettes have been rebuilt using up-to-date versions of all involved
  packages in R 4.3.2 (on a new machine). This only gave minor numerical
  differences in the long-term forecasts of `vignette("CHILI_prophet")`.

* Accommodate restricted checks without suggested packages.


# HIDDA.forecasting 1.1.1 (2021-03-31)

* Vignettes have been rebuilt using up-to-date versions of all involved
  packages in R 4.0.4, resulting in the following changes:

  - `vignette("BNV")`: age-specific amplitude-shift parameter
     transformations were wrong in `summary(mg_Cpower)`;
     bug fixed in **surveillance** 1.18.0.

  - `surveillance::pit()` values for degenerate forecasts with zero
    probability for observed counts may differ due to a change in
    **surveillance** 1.17.1 (they still produce warnings).

  - `vignette("CHILI_hhh4")`: no rounding of `n` from 213 to 210 in
    printed `calibrationTest()` (a bug fixed in R 3.6.0).

  - `vignette("CHILI_prophet")`: minor numerical differences in model fit
    and predictions due to changes in **prophet**.


# HIDDA.forecasting 1.1.0 (2019-03-29)

* Use standard PIT for continuous forecasts (`arima`, `prophet`, `naive`).
  Differences to the previously used non-randomized PIT histograms for
  count data are negligible.

* Add scores for discretized log-normal forecasts, via new function
  `scores_lnorm_discrete()`. These scores are almost identical to the
  continuous scores, essentially due to the large counts.

* Vignettes have been rebuilt using up-to-date versions of all involved
  packages (**forecast** 8.5, **glarma** 1.6.0, **hhh4contacts** 0.13.0,
  **prophet** 0.4, **scoringRules** 0.9.5, **surveillance** 1.17.0)
  in R 3.5.3.


# HIDDA.forecasting 1.0.0 (2018-09-04)

* This is the version used for the book chapter.

* The contained vignettes have been built using R 3.5.1 with all dependent
  packages' versions as of 25 July 2018 from CRAN. The versions of the
  main packages were:

    * [**forecast**](https://CRAN.R-project.org/package=forecast) 8.4
    * [**glarma**](https://CRAN.R-project.org/package=glarma) 1.6.0
    * [**hhh4contacts**](https://CRAN.R-project.org/package=hhh4contacts) 0.13.0
    * [**prophet**](https://CRAN.R-project.org/package=prophet) 0.3.0.1
    * [**scoringRules**](https://CRAN.R-project.org/package=scoringRules) 0.9.4
    * [**surveillance**](https://CRAN.R-project.org/package=surveillance) 1.16.2
