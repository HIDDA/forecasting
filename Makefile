################################################################################
## Useful rules to build, check and install an R source package
##
## Copyright (C) 2012,2014-2017 Sebastian Meyer
################################################################################

## define variable for R to enable 'make check R=R-devel'
R := R

## package name and version (taken from DESCRIPTION)
PKG := $(strip $(shell grep "^Package:" DESCRIPTION | cut -f 2 -d ":"))
VERSION := $(strip $(shell grep "^Version:" DESCRIPTION | cut -f 2 -d ":"))

## roxygenise (update NAMESPACE and Rd files)
document:
	$R --no-restore --no-save --no-init-file --slave -e "devtools::document()"

## build extra vignettes and copy results to inst/doc
extra_vignettes:
	make -C vignettes/extra
	cp -a -t inst/doc vignettes/extra/*.html

## build the package
build: document extra_vignettes
	$R CMD build .

## package installation
install: build
	$R CMD INSTALL ${PKG}_${VERSION}.tar.gz


## auxiliary functions ("canned recipes") for check rules
define check-report-warnings-in-examples
cd ${PKG}.Rcheck; \
nwarn=`grep -c "^Warning" ${PKG}-Ex.Rout`; \
if [ $$nwarn -gt 0 ]; then echo "\n\tWARNING: $$nwarn" \
	"warning(s) thrown when running examples,\n" \
	"\t         see file ${PKG}.Rcheck/${PKG}-Ex.Rout\n"; fi
endef

## standard --as-cran check with remote checks disabled
check: build
	_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE _R_CHECK_EXAMPLE_TIMING_THRESHOLD_=2 $R CMD check --as-cran --timings ${PKG}_${VERSION}.tar.gz
	@$(check-report-warnings-in-examples)

## almost all targets are "phony"
.PHONY: document extra_vignettes build install check
