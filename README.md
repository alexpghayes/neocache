
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neocache

<!-- badges: start -->

[![R-CMD-check](https://github.com/alexpghayes/neocache/workflows/R-CMD-check/badge.svg)](https://github.com/alexpghayes/neocache/actions)
[![Codecov test
coverage](https://codecov.io/gh/alexpghayes/neocache/branch/main/graph/badge.svg)](https://codecov.io/gh/alexpghayes/neocache?branch=main)
<!-- badges: end -->

`neocache` facilitates robust sampling of the Twitter graph. The basic
idea is to save any data into a local cache as you as you get it.
`neocache` is build on top of [`rtweet`](https://rtweet.info/), Docker,
and Neo4J.

## Installation

You can install the development version of `neocache` with:

``` r
install.packages("devtools")
devtools::install_github("alexpghayes/neocache")
```

``` r
library(neocache)

nc_sitrep()
#> 
#> ── Caches ──────────────────────────────────────────────────────────────────────
#>   - get_friends_test (active) 
#>   - HQWHHAKFKLRTCZLNQOSQ (active) 
#>   - test_cache (active)
```
