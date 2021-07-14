#' Fetches the friends of each of the users contained in the user_ids vector.
#' Returns the friend relationships via a tibble edge list.
#'
#' @param cache The name of the cache to save data in.
#' @inheritParams rtweet::get_friends
#'
#' @return A tibble where each row corresponds to a follower relationship
#' from the user in the 'from' column to the user in to 'to' column
#'
#' @export
nc_get_friends <- function(user_ids, cache_name, n = 5000, retryonratelimit = NULL, cursor = "-1", verbose = TRUE, token = NULL) {
  # here we will need to query twice: once to ask who we actually
  # have *complete* friendship edges for, and then a second time to get
  # those friendship edges

  cache <- nc_activate_cache(cache_name)

  status <- friend_sampling_status(user_ids, cache)

  # status might be NULL for any set of these things

  # sample the friends of all the users w/o sampled friends
  new_edges <- merge_then_fetch_connect_friends(status$not_in_graph, n, cache)
  upgraded_edges <- merge_then_fetch_connect_friends(status$sampled_friends_at_is_null, n, cache)
  existing_edges <- db_get_friends(status$sampled_friends_at_not_null, cache)

  # need to be careful about duplicate edges here. ideally
  # we guarantee that edges are unique somehow before this, but if not
  # we can use dplyr::distinct(), although this is an expensive operation

  bind_rows(empty_user_edges(), new_edges, upgraded_edges, existing_edges)
}


#' user_ids is a list of COMPLETELY NEW users. This function performs the following:
#'   1. Fetch the friends of each user (call these main users) listed in user_ids (call these blank friends)
#'   2. MERGE nodes for main users and blank friends (each of these nodes will only contain a user_id field)
#'   3. Create edges between main users and their respective blank friends
#'   4. Set the sampled_friends_at property for nodes that were sampled
#'
#' @param user_ids a list of user_ids to add friend edges to the db for
#' @param n how many friends to sample at a time for each user
#' @param cache the cache to interface with
#'
#' @return a 2-column tibble edge list from user_ids to their friends
merge_then_fetch_connect_friends <- function(user_ids, n, cache) {
  # set sampled_friends_at to Sys.time()
  # sampled_at and sampled_followers_at default to NULL
  # return friends of each user

  if (length(user_ids) <= 1) {
    return(empty_user_edges())
  }

  sample_time <- Sys.time()

  edge_list <- rtweet::get_friends(user_ids, n = n) %>%
    rename(from = .data$user, to = .data$ids)

  ### 2.
  db_add_new_users(c(user_ids, edge_list$to), cache)

  ### 3.
  docker_bulk_connect_nodes(edge_list, cache)

  ### 4.
  update_qry <- glue(
    'WITH ["', glue_collapse(user_ids, sep = '","'), '"] AS user_ids UNWIND user_ids AS id ',
    'MATCH (n:User {{user_id:id}}) SET n.sampled_friends_at = "{sample_time}"'
  )

  query_neo4j(update_qry, cache)

  edge_list
}

#' Checks whether friend data has already been sampled for the provided
#' vector of users.
#'
#' @param user_ids to fetch the sampling status for
#' @param cache the cache to interface with
#'
#' @return a list of all users who either (1) are not currently in the
#' graph, (2) are in the graph but their friends have not been sampled,
#' (3) are in the graph and have sampled friends
friend_sampling_status <- function(user_ids, cache) {

  # might be a tibble with zero rows if no users are present in graph
  present_users <- db_lookup_users(user_ids, cache)

  not_in_graph <- setdiff(user_ids, present_users$user_id)

  unsampled_users <- present_users %>%
    filter(is.na(.data$sampled_friends_at)) %>%
    pull(.data$user_id)

  sampled_users <- setdiff(user_ids, c(unsampled_users, not_in_graph))

  # all of these take value character(0) when empty
  list(
    not_in_graph = not_in_graph,
    sampled_friends_at_is_null = unsampled_users,
    sampled_friends_at_not_null = sampled_users
  )
}

#' Gets the friends for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have friend edge data
#' @param cache the cache to interface with
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their friends
db_get_friends <- function(user_ids, cache) {
  if (is.na(user_ids)) {
    return(empty_user_edges())
  }

  query_neo4j(
    paste0(
      'WITH "MATCH (from:User),(to:User) WHERE from.user_id in [\\\'',
      glue_collapse(user_ids, sep = "\\',\\'"),
      '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id" AS query ',
      'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'
    ),
    cache
  )

  tmp <- tempfile()

  copy_csv_from_docker("get_friends.csv", tmp, cache$container_name)

  on.exit(file.remove(tmp))

  results <- readr::read_csv(
    tmp,
    col_types = readr::cols(
      from.user_id = readr::col_character(),
      to.user_id = readr::col_character()
    )
  )

  if (length(results) != 2) {
    return(empty_user_edges())
  }

  tibble(from = results$from.user_id, to = results$to.user_id)
}
