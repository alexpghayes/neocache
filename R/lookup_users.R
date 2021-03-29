#' Looks up information pertaining to a vector of provided users.  Fetches cached
#' data for users who have already been looked up, and fetches new data for users
#' who have not bee looked up yet.
#'
#' @param users A character vector of user ids (never screen names)
#' @return A tibble where each row corresponds to a User and each column
#' to one of the User properties. If a user cannot be sampled, should
#' return nothing for that user. If no users can be sampled, should
#' return an empty tibble with appropriate columns.
#' @export
lookup_users <- function(user_ids) {
  user_data <- db_lookup_users(user_ids)

  sampled_data <- filter(user_data, !is.na(sampled_at))
  not_in_graph_ids <- setdiff(user_ids, user_data$user_id)
  not_sampled_ids <- filter(user_data, is.na(sampled_at))$user_id

  if(length(not_sampled_ids) != 0) {
    upgraded_user_data <- fetch_lookup_update(not_sampled_ids)
  } else {
    upgraded_user_data <- empty_lookup()
  }
  if(length(not_in_graph_ids) != 0) {
    new_user_data <- merge_fetch_lookup_update(not_in_graph_ids)
  } else {
    new_user_data <- empty_lookup()
  }

  bind_rows(sampled_data, upgraded_user_data, new_user_data)
}


#' Fetches the user's lookup_users data then updates their info in the graph.
#'
#' @param user_ids a vector of user_ids
#' @return tibble of user data
fetch_lookup_update <- function(user_ids) {
  properties <- c(
    "screen_name", "protected", "followers_count", "friends_count",
    "listed_count", "statuses_count", "favourites_count", "account_created_at", "verified", "profile_url",
    "profile_expanded_url", "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url",
    "name", "location", "description", "url"
  )
  set_string <- "SET n.sampled_at=row.sampled_at SET n.screen_name=row.screen_name SET n.protected=toBoolean(row.protected) SET n.followers_count=toInteger(row.followers_count) SET n.friends_count=toInteger(row.friends_count) SET n.listed_count=toInteger(row.listed_count) SET n.statuses_count=toInteger(row.statuses_count) SET n.favourites_count=toInteger(row.favourites_count) SET n.account_created_at=toInteger(row.account_created_at) SET n.verified=toBoolean(row.verified) SET n.profile_url=row.profile_url SET n.profile_expanded_url=row.profile_expanded_url SET n.account_lang=row.account_lang SET n.profile_banner_url=row.profile_banner_url SET n.profile_background_url=row.profile_background_url SET n.profile_image_url=row.profile_image_url SET n.name=row.name SET n.location=row.location SET n.description=row.description SET n.url=row.url"

  user_info <- fetch_lookup(user_ids)[properties] %>%
    bind_cols(sampled_at = as.character(Sys.time())) %>%
    bind_cols(tibble(user_id=user_ids))
  user_info$account_created_at <- as.character(user_info$account_created_at)

  tmp <- tempfile()
  write_csv(user_info, tmp, na = "")
  system(glue("docker cp {tmp} neocache_docker:/var/lib/neo4j/import/lookup.csv"))
  file.remove(tmp)

  res <- sup4j(
    glue("LOAD CSV WITH HEADERS FROM 'file:///lookup.csv' AS row MATCH (n:User {{user_id:row.user_id}}) ",
         "{set_string} RETURN n.user_id"),
    get_connexion()
  )

  user_info
}


#' Creates nodes for the given user_ids in the graph, then fetches their
#' info and updates that info in the graph.
#'
#' @param user_ids a vector of user_ids
#' @return tibble of user data
merge_fetch_lookup_update <- function(user_ids) {
  docker_bulk_merge_users(user_ids)
  fetch_lookup_update(user_ids)
}


#' Takes in a vector of user_ids and returns their lookup_users info.
#'
#' @param user_ids vector of user_ids
#' @return tibble of user data
fetch_lookup <- function(user_ids) {
  print("FETCH LOOKUP CALLED")

  if(length(user_ids) == 0)
    return(empty_lookup())

  rtweet::lookup_users(user_ids)
}


#' Looks up users that are already in the database.
#'
#' @param user_ids list of user_ids to fetch existing lookup_user data for in
#' the db
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
