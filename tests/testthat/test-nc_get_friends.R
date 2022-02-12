library(rtweet)
library(logger)

log_level(FATAL)

auth_as("default")

test_ids <- c("780429268866052096", "1191642560")

# need to be careful of port collisions

if (!nc_cache_exists("get_friends_test")) {
  nc_create_cache("get_friends_test", http_port = 8001)
}

nc_empty_cache("get_friends_test", check_with_me_first = FALSE)

test_that("basic test", {
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
    colnames(third_call),
    colnames(fourth_call)
  )

  # order of edges might not match up since we don't guarantee that

  third_sorted <- third_call %>%
    arrange(from_id, to_id)

  fourth_sorted <- fourth_call %>%
    arrange(from_id, to_id)

  expect_equal(third_sorted, fourth_sorted)
})
