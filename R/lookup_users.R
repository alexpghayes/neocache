
#' @param users A character vector of user ids (never screen names)
#'
#' @return A tibble where each row corresponds to a User and each column
#' to one of the User properties. If a user cannot be sampled, should
#' return nothing for that user. If no users can be sampled, should
#' return an empty tibble with appropriate columns.
lookup_users <- function(user_ids) {
  user_data <- db_lookup_users(user_ids)
  not_in_graph <- setdiff(user_ids, user_data$user_id)
  new_user_data <- update_users(not_in_graph, lookup = TRUE)
  not_sampled <- filter(user_data, is.na(sampled_at))

  if (dim(not_sampled)[1] == 0) {
    upgraded_user_data <- empty_lookup()
  } else {
    upgraded_user_data <- not_sampled %>%
      pull(user_id) %>%
      update_users(lookup = TRUE)
  }

  user_data <- user_data %>%
    filter(!is.na(sampled_at))

  bind_rows(user_data, new_user_data, upgraded_user_data)
}




#' @param user_ids list of user_ids to fetch existing lookup_user data for in
#' the db
#'
#' @return a tibble with any existing data for user_ids
db_lookup_users <- function(user_ids) {

  con <- get_connexion()

  # return a tibble where each row corresponds to a User and each column
  # to one of the User properties. when a user in not present in the
  # database, should not return a row in the output tibble for that
  # user. if no users are in the db should return an empty tibble with one
  # column for each User property
  user_ids <- c(user_ids)
  data <- paste('MATCH (n) WHERE n.user_id in ["',
                paste(user_ids, collapse = '","'),
                '"] RETURN n',
                sep = ""
  ) %>%
    sup4j(con)

  if (length(data) == 0) {
    empty_lookup()
  } else {
    data[[1]] %>%
      bind_rows(empty_lookup())
  }
}


#' TODO: Rename this function. This function is used both to update present users
#' and add new users to the db.
#'
#' @param user_ids the user_ids to update
#' @param lookup should new Twitter profile data be updated for each user_id?
#' @param sample_size how many friends/followers should be looked up at a time if
#' the respective argument is set to TRUE?
#'
#' @return The tibble of user data, with one row for each (accessible)
#' user in `users` and one column for each property of `User` nodes
#' in the graph database.
update_users <- function(user_ids, sample_size = 150, lookup = FALSE) {

  con <- get_connexion()

  # make sure to set sampled_at to Sys.time() and
  # sampled_friends_at and sampled_followers_at to NULL
  # return data on users
  user_ids <- c(user_ids)
  if (length(user_ids) == 0) {
    return(empty_lookup())
  }

  if (lookup) {
    user_info <- rtweet::lookup_users(user_ids)
  } else {
    user_info <- empty_lookup() %>%
      bind_rows(tibble(user_id = user_ids))
  }

  if (length(user_info) == 0) {
    return(empty_lookup())
  }

  properties <- c(
    "screen_name", "protected", "followers_count", "friends_count",
    "listed_count", "statuses_count", "favourites_count", "account_created_at", "verified", "profile_url",
    "profile_expanded_url", "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url",
    "name", "location", "description", "url"
  )
  prop_types <- c(
    "chr", "bool", "num", "num", "num", "num", "num", "chr", "bool", "chr",
    "chr", "chr", "chr", "chr", "chr", "chr", "chr", "chr", "chr"
  )
  nodes <- empty_lookup()
  for (i in seq(1, dim(user_info)[1])) {
    info <- user_info[i, ]
    create_node <- paste('MERGE (n:User {user_id:"', info$user_id, '"}) SET ',
                         "n.sampled_at=",
                         if (lookup) {
                           paste('"', Sys.time(), '"', sep = "")
                         } else {
                           "NULL"
                         },
                         ",",
                         sep = ""
    )

    # Adds each property to to the Neo4j CYPHER query
    for (j in seq(1, length(properties))) {
      if (is.na(info[[properties[j]]])) {
        next
      }

      create_node <- paste(create_node, "n.", properties[j], "=", sep = "")

      if (prop_types[j] == "chr") {
        create_node <- paste(create_node, '"', info[[properties[j]]], '",', sep = "")
      } else {
        create_node <- paste(create_node, info[[properties[j]]], ",", sep = "")
      }
    }

    new_node <- paste(substr(create_node, 1, nchar(create_node) - 1), " RETURN n", sep = "") %>%
      sup4j(con)

    if (length(new_node) != 0) {
      nodes <- nodes %>% bind_rows(new_node$n)
    }
  }

  nodes
}
