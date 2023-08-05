
<!-- README.md is generated from README.Rmd. Please edit that file -->

# densityLocPoly

<!-- badges: start -->
<!-- badges: end -->

This package is designed to estimate bivariate densities using
nonparametric local polynomial procedures for data that belong to
(complicated) domains in $R^2$. Examples can be found in the
subdirectory `examples/` of the package.

This package has been created to illustrate the properties of the
estimation procedure proposed in [this
paper](https://arxiv.org/abs/2308.01156).

## Installation

You can install the development version of densityLocPoly from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("klutchnikoff/densityLocPoly")
devtools::install_github("klutchnikoff/densityLocPoly", build_vignettes = TRUE)
```

## Getting started

``` r
library("densityLocPoly")
```

## Examples

Examples used in the companion paper
[arXiv:2308.01156](https://arxiv.org/abs/2308.01156) are accessible as
follows:

``` r
vignette("example-sector", package = "densityLocPoly")
```
