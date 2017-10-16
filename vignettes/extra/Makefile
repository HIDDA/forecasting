## This is a convenient Makefile for quick vignette compilation in Emacs.
## It does nothing special and is not necessary to build the package.
## It is thus listed in the top-level .Rbuildignore file.

R := R
RMDFILES := $(wildcard *.Rmd)
VIGNETTES := $(RMDFILES:.Rmd=)

vignettes: $(addsuffix .html, ${VIGNETTES})

%.html: %.Rmd ../setup.R
	${R} --vanilla --slave -e "tools::buildVignette('$<', clean = FALSE, quiet = FALSE)"

clean:
	rm -rf *.md *_cache/ *_files/ .build.timestamp
cleanall: clean
	rm -rf $(addsuffix .html, ${VIGNETTES}) $(addsuffix .R, ${VIGNETTES})

.PHONY: vignettes clean cleanall
