library(rtweet)

auth_as("default")

test_ids <- c("780429268866052096", "1191642560")

# need to be careful of port collisions

if (!nc_cache_exists("lookup_users_test")) {
  nc_create_cache("lookup_users_test", http_port = 8000)
}

nc_empty_cache("lookup_users_test", check_with_me_first = FALSE)

test_that("works with screen names", {

  first_call <- nc_lookup_users("alexpghayes", "lookup_users_test")

  second_call <- nc_lookup_users("alexpghayes", "lookup_users_test")

  expect_equal(
    colnames(first_call),
    colnames(second_call)
  )

  expect_equal(
    first_call,
    second_call
  )

  third_call <- nc_lookup_users(c("alexpghayes", "karlrohe"), "lookup_users_test")
  fourth_call <- nc_lookup_users(c("alexpghayes", "karlrohe"), "lookup_users_test")

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

test_that("works with user ids", {

  first_call <- nc_lookup_users(test_ids[1], "lookup_users_test")
  second_call <- nc_lookup_users(test_ids[1], "lookup_users_test")

  expect_equal(first_call, second_call)

  third_call <- nc_lookup_users(test_ids, "lookup_users_test")
  fourth_call <- nc_lookup_users(test_ids, "lookup_users_test")

  expect_equal(third_call, fourth_call)
})

nc_destroy_cache("lookup_users_test")
