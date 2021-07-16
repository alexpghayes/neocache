library(neo4r)
library(rtweet)
library(tidyverse)
library(magrittr)
library(glue)


#' start_neo4j but used exclusively in unit testing
#'
#' @export
start_test_neo4j <- function() {
  message("Checking the status of Docker...")
  # Check if Docker is running
  if (system("docker system info", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("Docker does not appear to be running. Please start Docker and try again.")
  }
  message("Docker seems to be running.\n")

  message("Attempting to start Neocache container...")
  # Start the Docker container
  # First try to run the existing Docker container
  if (system("docker start neocache_test_container", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    # Container does not exist, create the container
    if (system("docker run --name neocache_test_container -p7474:7474 -p7687:7687 -d -e NEO4J_AUTH=neo4j/pass -e NEO4J_apoc_export_file_enabled=true -e NEO4J_apoc_import_file_enabled=true -e NEO4J_apoc_import_file_use__neo4j__config=true -e NEO4JLABS_PLUGINS=[\\\"apoc\\\"] neo4j:3.5.21",
      ignore.stdout = TRUE
    ) != 0) {
      stop("Failed to initiate docker container.")
    }
  }

  message(
    "Neocache container successfully launched.\n",
    "\nWaiting for Neo4j to come online, this may take a while..."
  )
  # Now we wait for Neo4j to finish starting up
  con <- get_connexion()
  while (TRUE) {
    Sys.sleep(12)
    tryCatch(
      {
        if (con$ping() == 200) {
          break
        }
      },
      error = function(e) {},
      condition = function(e) {}
    )
  }
  message("Neo4j successfully started.")
}


#' stop_neo4j but used exclusively in unit testing
stop_test_neo4j <- function() {
  if (system("docker stop neocache_test_container", ignore.stdout = TRUE) != 0) {
    warning("Error returned when attempting to stop docker container.")
  } else {
    message("Successfully shut down Neo4j and the Neocache Docker container.")
  }
}

#' HELPER FUNCTION USED TO CLEAR THE DATABASE FOR TESTING,
#' REMOVE THIS FUNCTION ONCE TESTING IS DONE
clear____db <- function() {
  con <- get_connexion()

  "MATCH (n) DETACH DELETE n" %>%
    sup4j(con)
}


# Users: NathanKolbow, nuhthan_kelbith, makpetersdorff, jacobpopp_,
#        andrew25baker, # JoeBiden,
user_ids <- c(
  "1319331638497640449", "1286145534185570306", "3321897342", "478015213",
  "2379365523"
) # , "939091")
SAMPLE_SIZE <- 1e5

prep_tests <- function() {
  assign("r_lookup", rtweet::lookup_users(user_ids), envir = globalenv())
  assign("r_get_friends", rtweet::get_friends(user_ids, n = SAMPLE_SIZE) %>% rename(from = user, to = user_id), envir = globalenv())
  r_get_followers <- NULL
  for (user in user_ids) {
    r_get_followers <- r_get_followers %>%
      bind_rows(tibble(to = user, rtweet::get_followers(user, n = SAMPLE_SIZE)))
  }

  r_get_followers <- r_get_followers %>%
    rename(from = user_id)

  assign("r_get_followers", r_get_followers, envir = globalenv())
}


run_tests <- function() {
  lookup_properties <- c(
    "screen_name", "protected", "followers_count", "friends_count",
    "listed_count", "statuses_count", "favourites_count", "verified", "profile_url",
    "profile_expanded_url", "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url",
    "name", "location", "description", "url"
  )

  clear____db()

  tests <- vector()

  # TEST 1: Initial lookup_users is equivalent to rtweet::lookup_users
  print("Running test 1.")
  tictoc::tic()
  tests[1] <- TRUE
  d_lookup <- lookup_users(user_ids)
  for (i in length(user_ids)) {
    if (tests[1] == FALSE) {
      break
    }

    for (prop in lookup_properties) {
      if ((is.na(r_lookup[[i, prop]]) || is.na(d_lookup[[i, prop]])) &&
        !(is.na(r_lookup[[i, prop]]) && is.na(d_lookup[[i, prop]]))) {
        if (!setequal(r_lookup[[i, prop]], d_lookup[[i, prop]])) {
          print(paste(
            "TEST 1:", prop, "for", user_ids[i], "differs (",
            r_lookup[[i, prop]], ") vs. (", d_lookup[[i, prop]], ")"
          ))
          tests[1] <- FALSE
          break
        }
      }
    }
  }
  tictoc::toc()


  # TEST 2, 3, 4: Subsequent call to lookup_users is equivalent to
  # rtweet::lookup_users
  for (test_num in c(2, 3, 4)) {
    print(paste("Running test", test_num))
    tictoc::tic()
    tests[test_num] <- TRUE
    d_lookup <- lookup_users(user_ids)
    for (i in length(user_ids)) {
      if (tests[test_num] == FALSE) {
        break
      }

      for (prop in lookup_properties) {
        if ((is.na(r_lookup[[i, prop]]) || is.na(d_lookup[[i, prop]])) &&
          !(is.na(r_lookup[[i, prop]]) && is.na(d_lookup[[i, prop]]))) {
          if (!setequal(r_lookup[[i, prop]], d_lookup[[i, prop]])) {
            print(paste(
              "TEST", test_num, "-", prop, "for", user_ids[i], "differs, (",
              r_lookup[[i, prop]], ") vs. (", d_lookup[[i, prop]], ")"
            ))
            tests[test_num] <- FALSE
            break
          }
        }
      }
    }
    tictoc::toc()
  }


  # TEST 5: Initial call to get_friends is equivalent to
  # rtweet::get_friends when the users are already in the graph w/o
  # friends edge data
  print("Running test 5")
  tictoc::tic()
  tests[5] <- TRUE
  d_friends <- get_friends(user_ids, sample_size = SAMPLE_SIZE)
  for (user in user_ids) {
    if (!setequal(d_friends[d_friends$from == user, ]$to, r_get_friends[r_get_friends$from == user, ]$to)) {
      print(paste(
        "TEST 5: Lists for", user, "differ, (",
        paste(d_friends[d_friends$from == user, ]$to, collapse = ","), ") vs. (",
        paste(r_get_friends[r_get_friends$from == user, ]$to, collapse = ","),
      ))
      tests[5] <- FALSE
      break
    }
  }
  tictoc::toc()


  # TEST 6, 7, 8: Subsequent calls to get_friends still return reponses
  # equivalent to rtweet::get_friends
  for (test_num in c(6, 7, 8)) {
    print(paste("Running test", test_num))
    tictoc::tic()
    tests[test_num] <- TRUE
    d_friends <- get_friends(user_ids, sample_size = SAMPLE_SIZE)
    for (user in user_ids) {
      if (!setequal(d_friends[d_friends$from == user, ]$to, r_get_friends[r_get_friends$from == user, ]$to)) {
        print(paste("TEST", test_num, "- Lists for", user, "differ"))
        tests[test_num] <- FALSE
        break
      }
    }
    tictoc::toc()
  }


  # TEST 9: Call to lookup_users is equivalent to rtweet::lookup_users when
  # some users are in the graph with lookup/edge data and some are not
  clear____db()
  print("Running test 9")
  tictoc::tic()
  tests[9] <- TRUE
  get_friends(user_ids[1:2], sample_size = SAMPLE_SIZE)
  update_users(user_ids[3], lookup = TRUE)
  update_users(user_ids[4], lookup = FALSE)

  assign("dlookup", lookup_users(user_ids), envir = globalenv())
  for (i in length(user_ids)) {
    if (tests[9] == FALSE) {
      break
    }

    for (prop in lookup_properties) {
      if ((is.na(r_lookup[[i, prop]]) || is.na(d_lookup[[i, prop]])) &&
        !(is.na(r_lookup[[i, prop]]) && is.na(d_lookup[[i, prop]]))) {
        if (!setequal(r_lookup[[i, prop]], d_lookup[[i, prop]])) {
          print(paste(
            "TEST 9:", prop, "for", user_ids[i], "differs, (",
            r_lookup[[i, prop]], ") vs. (", d_lookup[[i, prop]], ")"
          ))
          tests[9] <- FALSE
          break
        }
      }
    }
  }
  tictoc::toc()


  # TEST 10: Call to get_friends is equivalent to rtweet::get_friends
  # when some users are not in the graph to begin with, some users are in
  # the graph w/o friend edge data, and some users are in the graph
  # with edge data
  print("Running test 10")
  tictoc::tic()
  tests[10] <- TRUE
  d_friends <- get_friends(user_ids, sample_size = SAMPLE_SIZE)
  for (user in user_ids) {
    if (!setequal(d_friends[d_friends$from == user, ]$to, r_get_friends[r_get_friends$from == user, ]$to)) {
      print(paste("TEST 10: Lists for", user, "differ"))
      tests[10] <- FALSE
      break
    }
  }
  tictoc::toc()


  # TEST 11: lookup_users still returns the correct data after get_friends
  # is called
  print("Running test 11")
  tictoc::tic()
  tests[11] <- TRUE
  d_lookup <- lookup_users(user_ids)
  for (i in length(user_ids)) {
    if (tests[11] == FALSE) {
      break
    }

    for (prop in lookup_properties) {
      if ((is.na(r_lookup[[i, prop]]) || is.na(d_lookup[[i, prop]])) &&
        !(is.na(r_lookup[[i, prop]]) && is.na(d_lookup[[i, prop]]))) {
        if (!setequal(r_lookup[[i, prop]], d_lookup[[i, prop]])) {
          print(paste(
            "TEST 11:", prop, "for", user_ids[i], "differs, (",
            r_lookup[[i, prop]], ") vs. (", d_lookup[[i, prop]], ")"
          ))
          tests[11] <- FALSE
          break
        }
      }
    }
  }
  tictoc::toc()

  # TEST 12: Initial call to get_followers is equivalent to
  # rtweet::get_followers when the users are already in the graph w/o
  # follower edge data
  print("Running test 12")
  tictoc::tic()
  tests[12] <- TRUE
  print("Getting followers...")
  d_followers <- get_followers(user_ids, sample_size = SAMPLE_SIZE)
  print("Entering loop...")
  for (user in user_ids) {
    if (!setequal(d_followers[d_followers$to == user, ]$from, r_get_followers[r_get_followers$to == user, ]$from)) {
      print(paste(
        "TEST 12: Lists for", user, "differ, (",
        paste(d_followers[d_followers$to == user, ]$from, collapse = ","), ") vs. (",
        paste(r_get_followers[r_get_followers$to == user, ]$from, collapse = ","), ")"
      ))
      tests[12] <- FALSE
      break
    }
  }
  tictoc::toc()


  # TEST 13, 14, 15: Subsequent calls to get_followers still return responses
  # equivalent to rtweet::get_followers
  for (test_num in c(13, 14, 15)) {
    print(paste("Running test", test_num))
    tictoc::tic()
    tests[test_num] <- TRUE
    d_followers <- get_followers(user_ids, sample_size = SAMPLE_SIZE)
    for (user in user_ids) {
      if (!setequal(d_followers[d_followers$to == user, ]$from, r_get_followers[r_get_followers$to == user, ]$from)) {
        print(paste("TEST", test_num, "- Lists for", user, "differ"))
        tests[test_num] <- FALSE
        break
      }
    }
    tictoc::toc()
  }


  print(paste("Passed", sum(tests), "/", length(tests), "tests."))
}
