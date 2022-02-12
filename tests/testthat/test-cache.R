library(logger)

log_threshold("FATAL")

log_threshold("INFO")


random_name <- function(n = 20) {
  paste0(sample(LETTERS, size = n, replace = TRUE), collapse = "")
}

nc_destroy_cache("test_cache")

test_that("can create test cache", {
  expect_silent(
    nc_create_cache("test_cache", http_port = 1513, bolt_port = 5013)
  )

  expect_error(
    nc_create_cache("test_cache", http_port = 1513, bolt_port = 5013),
    regexp = "A Docker container by the name of test_cache already exists."
  )
})

test_that("nc_create_cache is a list of cache information", {
  name <- random_name()
  expect_warning(cache <- nc_create_cache(name))
  expected_cache <- list(
    container_name = name,
    neo4j_pass = "password",
    url = "http://localhost:7474",
    http_port = 7474,
    bolt_port = 7687
  )
  expect_equal(cache, expected_cache)
  nc_destroy_cache(cache)

  name <- random_name()
  cache <- nc_create_cache(
    cache_name = name,
    neo4j_pass = "my_password",
    http_port = 1903,
    bolt_port = 1309,
    url = "http://127.0.0.1:1903"
  )
  expected_cache <- list(
    container_name = name,
    neo4j_pass = "my_password",
    url = "http://127.0.0.1:1903",
    http_port = 1903,
    bolt_port = 1309
  )
  expect_equal(cache, expected_cache)
  nc_destroy_cache(cache)

  expect_error(nc_create_cache(name, neo4j_pass = "something with spaces"))
  expect_error(nc_create_cache(name, neo4j_pass = "one space"))
})

test_that("get_cache retrieves expected information", {
  name <- random_name()
  cache <- expect_warning(nc_create_cache(name))
  expect_equal(cache, get_cache(name))
  nc_destroy_cache(cache)

  name <- random_name()
  cache <- nc_create_cache(
    cache_name = name,
    neo4j_pass = "my_password",
    http_port = 1903,
    bolt_port = 1309,
    url = "http://127.0.0.1:1903"
  )
  expect_equal(cache, get_cache(name))
  nc_destroy_cache(cache)
})

test_that("get_cache on non-existent name throws an error", {
  name <- random_name()
  expect_error(get_cache(name))
  expect_error(get_cache(name))
})

test_that("nc_activate_cache and unload_cache work in sequence", {
  name <- random_name()
  expect_warning(cache <- nc_create_cache(name))
  nc_activate_cache(cache)
  unload_cache(cache)
  nc_destroy_cache(cache)
})

test_that("nc_create_cache throws error on occupied port", {
  name <- random_name()
  cache <- get_cache("test_cache")
  nc_activate_cache(cache)

  expect_error(nc_create_cache("error", http_port = cache$http_port))
  expect_error(nc_create_cache("error", bolt_port = cache$bolt_port))

  unload_cache(cache)
})

test_that("nc_activate_cache and nc_create_cache throw errors on occupied ports", {
  name <- random_name()
  cache <- nc_create_cache(
    cache_name = name,
    neo4j_pass = "my_password",
    http_port = 1903,
    bolt_port = 1309,
    url = "http://127.0.0.1:1903"
  )
  nc_activate_cache(cache)
  expect_error(nc_activate_cache(cache))

  unload_cache(cache)
  nc_destroy_cache(cache)
})
