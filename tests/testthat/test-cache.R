context("Cache functions")
library(neocache)
library(logger)
logger::log_threshold("FATAL")


# This function ensures that the Docker containers we generate have unique names
random_name <- function(n = 20) {
  letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  paste0(sample(letters, size=25, replace=TRUE), collapse="")
}

# Make sure that test_cache exists before we continue
tryCatch({
  new_cache("test_cache", http_port = 1513, bolt_port = 5013)
}, error = function(cond) {})


################
## Unit Tests ##
################
test_that("new_cache is a list of cache information", {
  name <- random_name()
  expect_warning(cache <- new_cache(name))
  expected_cache <- list(
    container_name = name,
    neo4j_pass     = "password",
    url            = "http://localhost:7474",
    http_port      = 7474,
    bolt_port      = 7687
  )
  expect_equal(cache, expected_cache)
  remove_cache(cache)

  name <- random_name()
  cache <- new_cache(
    cache_name = name,
    neo4j_pass = "my_password",
    http_port  = 1903,
    bolt_port  = 1309,
    url        = "http://127.0.0.1:1903"
  )
  expected_cache <- list(
    container_name = name,
    neo4j_pass = "my_password",
    url        = "http://127.0.0.1:1903",
    http_port  = 1903,
    bolt_port  = 1309
  )
  expect_equal(cache, expected_cache)
  remove_cache(cache)

  expect_error(new_cache(name, neo4j_pass="something with spaces"))
  expect_error(new_cache(name, neo4j_pass="one space"))
})

test_that("get_cache retrieves expected information", {
  name <- random_name()
  cache <- expect_warning(new_cache(name))
  expect_equal(cache, get_cache(name))
  remove_cache(cache)

  name <- random_name()
  cache <- new_cache(
    cache_name = name,
    neo4j_pass = "my_password",
    http_port  = 1903,
    bolt_port  = 1309,
    url        = "http://127.0.0.1:1903"
  )
  expect_equal(cache, get_cache(name))
  remove_cache(cache)
})

test_that("get_cache on non-existent name throws an error", {
  name <- random_name()
  expect_error(get_cache(name))
  expect_error(get_cache(name))
})

test_that("load_cache and unload_cache work in sequence", {
  name <- random_name()
  expect_warning(cache <- new_cache(name))
  load_cache(cache)
  unload_cache(cache)
  remove_cache(cache)
})

test_that("new_cache throws error on occupied port", {
  name <- random_name()
  cache <- get_cache("test_cache")
  load_cache(cache)

  expect_error(new_cache("error", http_port = cache$http_port))
  expect_error(new_cache("error", bolt_port = cache$bolt_port))

  unload_cache(cache)
})

test_that("load_cache and new_cache throw errors on occupied ports", {
  name <- random_name()
  cache <- new_cache(
    cache_name = name,
    neo4j_pass = "my_password",
    http_port  = 1903,
    bolt_port  = 1309,
    url        = "http://127.0.0.1:1903"
  )
  load_cache(cache)
  expect_error(load_cache(cache))

  unload_cache(cache)
  remove_cache(cache)
})
