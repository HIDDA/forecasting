## This is a convenient Makefile for quick vignette compilation in Emacs.
## It requires GNU make.
## Because of this and because it does nothing special to build
## the vignettes, it is listed in the top-level .Rbuildignore file
## and called GNUmakefile so that it is not used by R CMD build.

R := R
RMDFILES := $(wildcard *.Rmd)
VIGNETTES := $(RMDFILES:.Rmd=)

vignettes: $(addsuffix .html, ${VIGNETTES})

%.html: %.Rmd setup.R
	LANGUAGE=en ${R} --vanilla --slave -e "options(save.defaults = list(version = 2)); tools::buildVignette('$<', clean = FALSE, quiet = FALSE)"

clean:
	rm -rf *.md cache figure *_cache/ *_files/ .build.timestamp
cleanall: clean
	rm -rf $(addsuffix .html, ${VIGNETTES}) $(addsuffix .R, ${VIGNETTES})

.PHONY: vignettes clean cleanall
