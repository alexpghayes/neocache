#' Get (cached) information about Twitter users
#'
#' Looks up information pertaining to a vector of provided users. Retrieves
#' cached data for users who have already been looked up, and fetches new
#' data for users who have not bee looked up yet from the Twitter API.
#'
#' @param user_ids Character vector of Twitter user ids. **DO NOT**
#'   pass Twitter screen names, as `user_ids` are taken to be unique
#'   node identifiers.
#'
#' @param cache A `cache` object. See [nc_create_cache()].
#'
#' @inheritParams rtweet::lookup_users
#'
#' @return A tibble where each row corresponds to a User and each column
#'   to one of the User properties. If a user cannot be sampled, should
#'   return nothing for that user. If no users can be sampled, should
#'   return an empty tibble with appropriate columns.
#'
#' @export
nc_lookup_users <- function(user_ids, cache_name, token = NULL, retryonratelimit = NULL, verbose = TRUE) {

  cache <- nc_activate_cache(cache_name)

  log_debug("Looking for users in Neo4J database ...")

  user_data <- db_lookup_users(user_ids, cache)

  log_debug("Looking for users in Neo4J database ... done")

  log_formatter(formatter_pander)
  log_trace("user_data (from Neo4J) key columns:")
  log_trace(select(user_data, user_id, screen_name, sampled_at), style = "simple")
  log_formatter(formatter_glue)

  sampled_data <- filter(user_data, !is.na(.data$sampled_at))

  not_in_graph_ids <- setdiff(user_ids, user_data$user_id)
  not_sampled_ids <- filter(user_data, is.na(.data$sampled_at))$user_id

  if (length(not_sampled_ids) != 0) {
    log_trace(glue("Needed to get data from API for: {not_sampled_ids}"))
    upgraded_user_data <- add_lookup_users_info_to_nodes_in_graph(not_sampled_ids, cache)

    log_formatter(formatter_pander)
    log_trace("upgraded_user_data key columns:")
    log_trace(select(upgraded_user_data, user_id, screen_name, sampled_at), style = "simple")
    log_formatter(formatter_glue)

  } else {
    log_trace("Did not need to update any nodes already in Neo4J.")
    upgraded_user_data <- empty_lookup()
  }

  if (length(not_in_graph_ids) != 0) {
    log_trace(glue("Need to add entirely new nodes: {not_in_graph_ids}"))
    db_add_new_users(not_in_graph_ids, cache)
    new_user_data <- add_lookup_users_info_to_nodes_in_graph(not_in_graph_ids, cache)

    log_formatter(formatter_pander)
    log_trace("new_user_data key columns:")
    log_trace(select(new_user_data, user_id, screen_name, sampled_at), style = "simple")
    log_formatter(formatter_glue)
  } else {
    log_trace("Did not need to add any new nodes to Neo4J.")
    new_user_data <- empty_lookup()
  }

  log_trace(glue("sampled_data is {nrow(sampled_data)} x {ncol(sampled_data)} with type signature"))
  log_trace(type_signature(sampled_data))
  log_trace(glue("upgraded_user_data is {nrow(upgraded_user_data)} x {ncol(upgraded_user_data)} with type signature"))
  log_trace(type_signature(upgraded_user_data))
  log_trace(glue("new_user_data is {nrow(new_user_data)} x {ncol(new_user_data)} with type signature"))
  log_trace(type_signature(new_user_data))

  bind_rows(empty_lookup(), sampled_data, upgraded_user_data, new_user_data)
}


#' Fetches the user's lookup_users data then updates their info in the graph.
#'
#' @param user_ids a vector of user_ids. do NOT pass screen names
#' @param cache the cache to interface with
#'
#' @return tibble of user data
add_lookup_users_info_to_nodes_in_graph <- function(user_ids, cache) {

  # NOTE: do not set friends_sampled_at or followers_sampled_at here -- we need
  # to preserve whatever value those have in the database already

  log_debug(glue("Need to request information on {length(user_ids)} users from API"))

  if (length(user_ids) == 0) {
    return(empty_lookup())
  }

  log_debug("Making API request with rtweet::lookup_users")

  user_info_raw <- rtweet::lookup_users(user_ids)  # TODO: pass twitter API arguments

  log_debug(glue("Received information on {nrow(user_info_raw)} users."))

  user_info <- user_info_raw %>%
    select(user_id, all_of(properties)) %>%
    mutate(
      sampled_at = as.character(Sys.time()),
      account_created_at = as.character(account_created_at)
    )

  tmp <- tempfile()

  write_csv(user_info, tmp, na = "")
  copy_csv_to_docker(tmp, "lookup.csv", cache$container_name)

  set_string <- "SET n.sampled_at=row.sampled_at SET n.screen_name=row.screen_name SET n.protected=toBoolean(row.protected) SET n.followers_count=toInteger(row.followers_count) SET n.friends_count=toInteger(row.friends_count) SET n.listed_count=toInteger(row.listed_count) SET n.statuses_count=toInteger(row.statuses_count) SET n.favourites_count=toInteger(row.favourites_count) SET n.account_created_at=toInteger(row.account_created_at) SET n.verified=toBoolean(row.verified) SET n.profile_url=row.profile_url SET n.profile_expanded_url=row.profile_expanded_url SET n.account_lang=row.account_lang SET n.profile_banner_url=row.profile_banner_url SET n.profile_background_url=row.profile_background_url SET n.profile_image_url=row.profile_image_url SET n.name=row.name SET n.location=row.location SET n.description=row.description SET n.url=row.url"

  query <- glue(
    "LOAD CSV WITH HEADERS FROM 'file:///lookup.csv' AS row MATCH (n:User {{user_id:row.user_id}}) ",
    "{set_string} RETURN n.user_id"
  )

  query_neo4j(query, cache)

  user_info
}

#' Looks up users that are already in the database.
#'
#' @param user_ids list of user_ids to fetch existing lookup_user data for in the db
#' @param cache the cache to interface with
#'
#' @return a tibble with any existing data for user_ids
db_lookup_users <- function(user_ids, cache) {
  # return a tibble where each row corresponds to a User and each column
  # to one of the User properties. when a user in not present in the
  # database, should not return a row in the output tibble for that
  # user. if no users are in the db should return an empty tibble with one
  # column for each User property
  user_string <- glue_collapse(user_ids, sep = '","')
  query <- glue('MATCH (n) WHERE n.user_id in ["{user_string}"] RETURN n')

  user_data <- query_neo4j(query, cache)

  # If the users' data does not exist in the DB, return an empty lookup,
  # otherwise return the users' data
  if (length(user_data) == 0) {
    return(empty_lookup())
  }

  # there's a cheeky hacky here: when nodes are added to Neo4J in the simplest
  # case we add nothing except the user_id. this bind_rows creates NA values
  # for sampled_*_at node properties if they don't exist in the underlying
  # Neo4J database. we should consider changing this behavior, because it
  # means that get_friends() has to rely on db_lookup_users() to learn
  # about node properties and can't use a separate Neo4J query, which seems
  # perfectly reasonable

  bind_rows(empty_lookup(), user_data[[1]])
}
