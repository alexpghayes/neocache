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
nc_get_friends <- function(user_ids, cache_name, n = 5000, retryonratelimit = TRUE, cursor = "-1", verbose = TRUE, token = NULL) {
  # here we will need to query twice: once to ask who we actually
  # have *complete* friendship edges for, and then a second time to get
  # those friendship edges

  log_trace(glue("nc_get_friends(): {user_ids}"))

  cache <- nc_activate_cache(cache_name)

  log_debug("Getting friend sampling status ..")

  status <- friend_sampling_status(user_ids, cache)

  log_debug(
    glue(
      "Sampling status summary: {length(status$sampled_friends_at_not_null)} sampled / ",
      "{length(status$sampled_friends_at_is_null)} unsampled / ",
      "{length(status$not_in_graph)} new",
      .trim = FALSE
    )
  )

  if (length(status$not_in_graph) > 0) {
    log_trace("Adding new users to graph ...")
    db_add_new_users(status$not_in_graph, cache)
    log_trace("Adding new users to graph ... done.")
  }

  # status might be character(0) for any set of these things

  new_edges <- add_friend_edges_to_nodes_in_graph(status$not_in_graph,
    n = n,
    retryonratelimit = retryonratelimit,
    cursor = cursor,
    verbose = verbose,
    token = token,
    cache = cache
  )

  upgraded_edges <- add_friend_edges_to_nodes_in_graph(status$sampled_friends_at_is_null,
    n = n,
    retryonratelimit = retryonratelimit,
    cursor = cursor,
    verbose = verbose,
    token = token,
    cache = cache
  )

  log_trace("Getting cached edges ...")
  existing_edges <- db_get_friends(status$sampled_friends_at_not_null, cache)
  log_trace("Getting cached edges ... done.")

  # need to be careful about duplicate edges here. ideally
  # we guarantee that edges are unique somehow before this, but if not
  # we can use dplyr::distinct(), although this is an expensive operation

  log_trace(glue("new_edges is {nrow(new_edges)} x {ncol(new_edges)} with type signature"))
  log_trace(type_signature(new_edges))
  log_trace(glue("upgraded_edges is {nrow(upgraded_edges)} x {ncol(upgraded_edges)} with type signature"))
  log_trace(type_signature(upgraded_edges))
  log_trace(glue("existing_edges is {nrow(existing_edges)} x {ncol(existing_edges)} with type signature"))
  log_trace(type_signature(existing_edges))

  bind_rows(empty_edge_list(), new_edges, upgraded_edges, existing_edges)
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
add_friend_edges_to_nodes_in_graph <- function(user_ids, n, retryonratelimit, cursor, verbose, token, cache) {

  log_trace(glue("add_friend_edges_to_nodes_in_graph(): {user_ids}"))

  if (length(user_ids) < 1) {
    return(empty_edge_list())
  }

  sample_time <- Sys.time()

  # make sure data returned by API is in the right scope

  tryCatch(
    expr = {
      log_trace(glue("Making API request with rtweet::get_friends for: {user_ids}"))

      edge_list <<- rtweet::get_friends(
        users = user_ids,
        n = n,
        retryonratelimit = retryonratelimit,
        cursor = cursor,
        parse = TRUE,
        verbose = verbose,
        token = token
      )

      if (NCOL(edge_list) == 2) {
        edge_list <<- rename(edge_list, from = .data$user, to = .data$ids)
      } else {
        log_warn(
          glue(
            "Edge list returned from Twitter API had {NCOL(iris)} columns, treating as empty edgelist."
          )
        )

        edge_list <<- empty_edge_list()
      }
    },
    error = function(cnd) {

      # to understand HTTPs codes returned by Twitter see
      # https://developer.twitter.com/en/support/twitter-api/error-troubleshooting

      msg <- conditionMessage(cnd)

      log_warn(glue("Failure making API request. Condition message: {msg}"))
      log_debug("Call of failed API request: ")
      log_debug(conditionCall(cnd))

      # don't want to match on actual text of error message, which may change
      # with user locale / language settings. blargh

      if (grepl("401", msg)) {
        log_warn("Treating 401 error as invalid user id, adding to Neo4J with missing data, and returning empty friend list.")
        edge_list <<- empty_edge_list()
      } else if (grepl("404", msg)) {
        log_warn("Treating 404 error as invalid user id, adding to Neo4J with missing data, and returning empty friend list.")
        edge_list <<- empty_edge_list()
      } else {
        stop(cnd)
      }
    }
  )

  db_add_new_users(c(user_ids, edge_list$to), cache)

  docker_bulk_connect_nodes(edge_list, cache)

  set_sampled_at_query <- glue(
    'WITH ["', glue_collapse(user_ids, sep = '","'), '"] AS user_ids UNWIND user_ids AS id ',
    'MATCH (n:User {{user_id:id}}) SET n.sampled_friends_at = "{sample_time}"'
  )

  query_neo4j(set_sampled_at_query, cache)

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

  log_trace(glue("friend_sampling_status(): {user_ids}"))

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

  log_trace(glue("db_get_friends(): {user_ids}"))

  if (length(user_ids) < 1) {
    return(empty_edge_list())
  }

  friend_query <- paste0(
    'WITH "MATCH (from:User),(to:User) WHERE from.user_id in [\\\'',
    glue_collapse(user_ids, sep = "\\',\\'"),
    '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id" AS query ',
    'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'
  )

  query_neo4j(friend_query, cache)

  tmp <- tempfile()

  copy_csv_from_docker("get_friends.csv", tmp, cache$container_name)

  results <- readr::read_csv(
    tmp,
    col_types = readr::cols(
      from.user_id = readr::col_character(),
      to.user_id = readr::col_character()
    )
  )

  if (ncol(results) != 2) {
    return(empty_edge_list())
  }

  tibble(from = results$from.user_id, to = results$to.user_id)
}
