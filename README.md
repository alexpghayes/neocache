
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neocache

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/alexpghayes/neocache/branch/master/graph/badge.svg)](https://codecov.io/gh/alexpghayes/neocache?branch=master)
[![R build
status](https://github.com/alexpghayes/neocache/workflows/R-CMD-check/badge.svg)](https://github.com/alexpghayes/neocache/actions)
<!-- badges: end -->

`neocache` facilitates robust sampling of the Twitter graph. The basic
idea is to save any data into a local cache as you as you get it.
`neocache` is build on top of
[`socialsampler`](https://github.com/alexpghayes/socialsampler) and
[`rtweet`](https://rtweet.info/).

Please see the `socialsampler` documentation for information on how to
register Twitter tokens.

## Installation

You can install the development version of `neocache` with:

``` r
install.packages("devtools")
devtools::install_github("alexpghayes/neocache")
```

## Running Neo4J through Docker

``` bash
docker run --name neo4j --env NEO4J_AUTH=neo4j/password --publish=7474:7474 --publish=7687:7687 -d neo4j:3.5.21
```

# Ignore everything below here

## Inspect the current neocache

``` r
library(neocache)

print_cache()
```

## What kind of data can I cache?

-   edges
-   nodes
-   you can get information on nodes without getting information on
    edges

## `rtweet` replacements

-   Use `cache_lookup_users()` instead of `lookup_users()`
-   Use `cache_get_friends()` instead of `get_friends()`
-   Use `cache_get_followers()` instead of `get_followers()`

## Get the cached data

-   Use `get_node_table()` and `get_edge_table()`

## Frequently asked questions

**How do you manage the API rate limits?**

Currently we just sample one node a minute. This complies with the
Twitter API rate limits, but only if you don’t use the registered tokens
for anything else while sampling.

**Where is the cache itself?**

It’s at `~/.twittergraph/`.

**What about users with big follower counts? Do you sample all the
followers?**

No. We only sample up to 5,000 friends and 5,000 followers. This is
mostly because I’m lazy and also it’s wasteful to spend API calls on
someone with tons of followers.

Recall that you can pick up an edge in the network from either node. We
recommend trying to make sure the node with smaller degree is in your
sample.

**What happens when I try to sample protected users?**

We remove these users from the sample at export time.

## Contributing

Please note that the `neocache` project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
