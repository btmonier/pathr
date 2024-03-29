# pathr

[![Build Status](https://travis-ci.org/btmonier/pathr.png?branch=master)](https://travis-ci.org/btmonier/pathr)  [![Build status](https://ci.appveyor.com/api/projects/status/xxgcbg2bfi4p8tfl?svg=true)](https://ci.appveyor.com/project/btmonier/pathr)  [![codecov](https://codecov.io/gh/btmonier/pathr/branch/master/graph/badge.svg)](https://ci.appveyor.com/project/btmonier/pathr)


## Installing

<!-- If you're putting `yourpackagename` on CRAN, it can be installed with

    install.packages("yourpackagename") -->

The pre-release version of the package can be pulled from GitHub using the [devtools](https://github.com/hadley/devtools) package:

    # install.packages("devtools")
    devtools::install_github("yourgithub/yourpackagename", build_vignettes=TRUE)


## For developers

The repository includes a Makefile to facilitate some common tasks.

### Running tests

`$ make test`. Requires the [testthat](https://github.com/hadley/testthat) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=logging`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `testthat` package.

### Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.
