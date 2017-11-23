
#' @importFrom zoo index
#' @export
zoo::index

#' @importFrom zoo autoplot.zoo
#' @export
zoo::autoplot.zoo

#' @importFrom zoo fortify.zoo
#' @export
zoo::fortify.zoo


#' Should the Package Vignettes Use `optipng`?
#'
#' If the environment variable \env{OPTIPNG} is set to `TRUE` (or
#' something regarded as such via [as.logical()]), the png files produced
#' when building the vignettes will be compressed (losslessly) using the
#' \command{optipng} program.
#'
#' @param level the optimization level (0-7). The default (2) is a good
#'     compromise between compression and speed.
#' @keywords internal
use_optipng <- function (level = 2)
{
    if (isTRUE(as.logical(Sys.getenv("OPTIPNG")))) {
        knitr::knit_hooks$set(optipng = knitr::hook_optipng)
        paste0("-o", level)
    } else NULL
}
