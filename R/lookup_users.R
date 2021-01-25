
#' @param users A character vector of user ids (never screen names)
#'
#' @return A tibble where each row corresponds to a User and each column
#' to one of the User properties. If a user cannot be sampled, should
#' return nothing for that user. If no users can be sampled, should
#' return an empty tibble with appropriate columns.
#'
#' @export
lookup_users <- function(user_ids) {
  user_data <- db_lookup_users(user_ids)
  not_in_graph <- setdiff(user_ids, user_data$user_id)
  new_user_data <- add_users_data(not_in_graph, lookup = TRUE)
  not_sampled <- filter(user_data, is.na(sampled_at))

  if (dim(not_sampled)[1] == 0) {
    upgraded_user_data <- empty_lookup()
  } else {
    upgraded_user_data <- not_sampled %>%
      pull(user_id) %>%
      merge_users(lookup = TRUE)
  }

  user_data <- filter(user_data, !is.na(sampled_at))
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
  user_string <- glue_collapse(user_ids, sep = '","')
  query <- glue('MATCH (n) WHERE n.user_id in ["{user_string}"] RETURN n')

  user_data <- sup4j(query)

  # If the users' data does not exist in the DB, return an empty lookup,
  # otherwise return the users' data
  if (length(user_data) == 0) {
    return(empty_lookup())
  }

  user_data[[1]] %>%
    bind_rows(empty_lookup())
}


USER_DATA_PROPERTIES <- c(
  "screen_name", "protected", "followers_count", "friends_count",
  "listed_count", "statuses_count", "favourites_count", "account_created_at", "verified", "profile_url",
  "profile_expanded_url", "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url",
  "name", "location", "description", "url"
)

#' TODO: Rename this function. This function is used both to update present users
#' and add new users to the db.
#'
#' @param user_ids the user_ids to update
#' @param lookup should new Twitter profile data be updated for each user_id?
#' @param n how many friends/followers should be looked up at a time if
#' the respective argument is set to TRUE?
#'
#' @return The tibble of user data, with one row for each (accessible)
#' user in `users` and one column for each property of `User` nodes
#' in the graph database.
add_users_data <- function(user_ids, lookup, n = 150) {

  con <- get_connexion()

  # make sure to set sampled_at to Sys.time() and
  # sampled_friends_at and sampled_followers_at to NULL
  # return data on users
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

  # NATHAN: look into the following approach
  # https://neo4j-rstats.github.io/user-guide/send.html#transform-elements-to-cypher-queries

  # the current approach you are taking very incrementally grows a
  # character vector and seems not ideal to me
  #
  # don't forget about setting sampled_at

  query <- user_info %>%
    select(USER_DATA_PROPERTIES) %>%
    vec_to_cypher("User")

  sup4j(paste("MERGE", query))

  nodes <- empty_lookup()
  for (i in seq(1, nrow(user_info))) {
    info <- user_info[i, ]
    create_node <- glue('MERGE (n:User {{user_id:"{info$user_id}"}}) ',
                        'SET n.sampled_at={if(lookup) Sys.time() else NULL},')

    # Adds each property to to the Neo4j CYPHER query
    for (j in seq(1, length(USER_DATA_PROPERTIES))) {
      property_data <- info[[USER_DATA_PROPERTIES[j]]]
      if (is.na(property_data)) {
        next
      }

      if (class(property_data) == "character") {
        create_node <- glue('{create_node} n.{USER_DATA_PROPERTIES[j]}="{property_data}",')
      } else {
        create_node <- glue('{create_node} n.{USER_DATA_PROPERTIES[j]}={property_data}",')
      }
    }

    browser()

    new_node <- glue('{substr(create_node, 1, nchar(create_node) - 1} RETURN n') %>%
      sup4j(con)

    if (length(new_node) != 0) {
      nodes <- bind_rows(nodes, new_node$n)
    }
  }

  nodes
}
