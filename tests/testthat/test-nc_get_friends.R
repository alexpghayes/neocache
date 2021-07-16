library(rtweet)

library(logger)
log_level(DEBUG)

auth_as("default")

test_ids <- c("780429268866052096", "1191642560")

# need to be careful of port collisions

if (!nc_cache_exists("get_friends_test")) {
  nc_create_cache("get_friends_test", http_port = 8001)
}

nc_empty_cache("get_friends_test", check_with_me_first = FALSE)

test_that("stuff", {
  first_call <- nc_get_friends(test_ids[1], "get_friends_test")
  second_call <- nc_get_friends(test_ids[1], "get_friends_test")

  expect_equal(
    colnames(first_call),
    colnames(second_call)
  )

  expect_equal(
    first_call,
    second_call
  )

  third_call <- nc_get_friends(test_ids, "get_friends_test")
  fourth_call <- nc_get_friends(test_ids, "get_friends_test")

  expect_equal(
    colnames(first_call),
    colnames(third_call)
  )

  expect_equal(
    colnames(first_call),
    colnames(fourth_call)
  )

  expect_equal(third_call, fourth_call)
})
