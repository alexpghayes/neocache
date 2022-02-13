#' Fetches the friends of each of the users contained in the users vector.
#' Returns the friend relationships via a tibble edge list.
#'
#' @param cache_name The name of the cache to save data in.
#' @inheritParams rtweet::get_friends
#'
#' @return A tibble where each row corresponds to a follower relationship
#' from the user in the 'from' column to the user in to 'to' column
#'
#' @export
nc_get_friends <- function(users, cache_name, n = 5000, retryonratelimit = TRUE, cursor = "-1", verbose = TRUE, token = NULL) {
  # here we will need to query twice: once to ask who we actually
  # have *complete* friendship edges for, and then a second time to get
  # those friendship edges

  log_trace(glue("nc_get_friends(): {users}"))

  cache <- nc_activate_cache(cache_name)

  log_trace(glue("Getting friend sampling status for {length(users)} users ..."))

  status <- friend_sampling_status(users, cache)

  num_not_in_graph <- length(status$not_in_graph)

  log_trace(
    glue(
      "Getting friend sampling status for {length(users)} users ... ",
      "{length(status$sampled_friends_at_not_null)} in graph with friends already sampled / ",
      "{length(status$sampled_friends_at_is_null)} in graph with friends not already sampled / ",
      "{num_not_in_graph} not in graph",
      .trim = FALSE
    )
  )

  length(status$not_in_graph)
  if (num_not_in_graph > 0) {
    log_trace(glue("Adding {num_not_in_graph} new users to graph ..."))
    db_add_new_users(status$not_in_graph, cache)
    log_trace(glue("Adding {num_not_in_graph} new users to graph ... done."))
  }

  # status might be character(0) for any set of these things

  new_edges <- add_friend_edges_to_nodes_in_graph(
    status$not_in_graph,
    n = n,
    retryonratelimit = retryonratelimit,
    cursor = cursor,
    verbose = verbose,
    token = token,
    cache = cache
  )

  upgraded_edges <- add_friend_edges_to_nodes_in_graph(
    status$sampled_friends_at_is_null,
    n = n,
    retryonratelimit = retryonratelimit,
    cursor = cursor,
    verbose = verbose,
    token = token,
    cache = cache
  )

  log_trace(glue("Getting cached friends of {{length(status$sampled_friends_at_not_null)}} users ..."))
  existing_edges <- db_get_friends(status$sampled_friends_at_not_null, cache)
  log_trace(glue("Getting cached friends of {{length(status$sampled_friends_at_not_null)}} users ... done"))

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


#' users is a list of COMPLETELY NEW users. This function performs the following:
#'   1. Fetch the friends of each user (call these main users) listed in users (call these blank friends)
#'   2. MERGE nodes for main users and blank friends (each of these nodes will only contain a id_str field)
#'   3. Create edges between main users and their respective blank friends
#'   4. Set the sampled_friends_at property for nodes that were sampled
#'
#' @inheritParams rtweet::get_friends
#' @param cache The cache to store information in. Must be a cache, not a
#'   cache name.
#'
#' @return a 2-column tibble edge list from users to their friends
add_friend_edges_to_nodes_in_graph <- function(users, n, retryonratelimit, cursor, verbose, token, cache) {

  log_trace(glue("add_friend_edges_to_nodes_in_graph(): {users}"))

  if (length(users) < 1) {
    return(empty_edge_list())
  }

  sample_time <- Sys.time()

  # make sure data returned by API is in the right scope
  edge_list <- NULL

  tryCatch(
    expr = {

      log_info(glue("Making API request with rtweet::get_friends for {length(users)} users"))

      edge_list <<- rtweet::get_friends(
        users = users,
        n = n,
        retryonratelimit = retryonratelimit,
        cursor = cursor,
        parse = TRUE,
        verbose = verbose,
        token = token
      )

      log_trace(glue("Making API request with rtweet::get_friends for: {users} ... results received."))
      log_trace(glue("Parsing results from API ... "))

      if (is.null(edge_list)) {
        stop("Results from API are `NULL`. This may be due to authentication failure.")
      } else if (NCOL(edge_list) == 2) {

        edge_list <<- rename(edge_list, from_id = .data$from_id, to_id = .data$to_id)
        log_trace(glue("Parsing results from API ... columns renamed."))
      } else {
        log_warn(
          glue(
            "Parsing results from API ... results had {NCOL(iris)} columns, treating as empty edgelist."
          )
        )

        edge_list <<- empty_edge_list()
      }

      log_trace(glue("Parsing results from API ... done."))
      log_trace(glue("Parsed friend list of {length(users)} users returned from API:"))
      log_trace(edge_list, style = "simple")
    },
    error = function(cnd) {

      # to understand HTTPs codes returned by Twitter see
      # https://developer.twitter.com/en/support/twitter-api/error-troubleshooting

      msg <- conditionMessage(cnd)

      log_warn(
        glue(
          "Making API request with rtweet::get_friends for {length(users)} users ... ",
          "API request failed. Condition message: {msg}",
          .trim = FALSE
        )
      )

      # don't want to match on actual text of error message, which may change
      # with user locale / language settings. blargh

      if (grepl("401", msg)) {
        log_warn(
          glue(
            "Making API request with rtweet::get_friends for {length(users)} users ... ",
            "401 error likely due to invalid user id, adding user to Neo4J DB ",
            "and  treating as if user has empty friend list.",
            .trim = FALSE
          )
        )
        edge_list <<- empty_edge_list()
      } else if (grepl("404", msg)) {
        log_warn(
          glue(
            "Making API request with rtweet::get_friends for {length(users)} users ... ",
            "404 error likely due to invalid user id, adding user to Neo4J DB ",
            "and treating as if user has empty friend list.",
            .trim = FALSE
          )
        )
        edge_list <<- empty_edge_list()
      } else {
        stop(cnd)
      }
    }
  )

  need_to_be_present_in_graph <- c(users, edge_list$to_id)

  log_trace(glue("Adding up to {length(need_to_be_present_in_graph)} new users to Neo4J DB ..."))

  db_add_new_users(need_to_be_present_in_graph, cache)

  log_trace(glue("Adding up to {length(need_to_be_present_in_graph)} new users to Neo4J DB ... done"))
  log_trace(glue("Adding {NROW(edge_list)} edges from API result into Neo4J graph ..."))

  docker_bulk_connect_nodes(edge_list, cache)

  log_trace(glue("Adding {NROW(edge_list)} edges from API result into Neo4J graph ... done"))
  log_trace(glue("Setting sampled_friends_at for {length(users)} users ..."))

  set_sampled_at_query <- glue(
    'WITH ["', glue_collapse(users, sep = '","'), '"] AS users UNWIND users AS id ',
    'MATCH (n:User {{id_str:id}}) SET n.sampled_friends_at = "{sample_time}"'
  )

  query_neo4j(set_sampled_at_query, cache)

  log_trace(glue("Setting sampled_friends_at for {length(users)} users ... done"))

  edge_list
}

#' Checks whether friend data has already been sampled for the provided
#' vector of users.
#'
#' @inheritParams add_friend_edges_to_nodes_in_graph
#'
#' @return a list of all users who either (1) are not currently in the
#' graph, (2) are in the graph but their friends have not been sampled,
#' (3) are in the graph and have sampled friends
friend_sampling_status <- function(users, cache) {

  # it might be possible to speed this up with a more focused query
  # than that used in db_lookup_users() -- in particular, potentially
  # could do less reading and writing to disk?

  log_trace(glue("friend_sampling_status(): {users}"))

  # might be a tibble with zero rows if no users are present in graph
  present_users <- db_lookup_users(users, cache)

  not_in_graph <- setdiff(users, present_users$id_str)

  unsampled_users <- present_users %>%
    filter(is.na(.data$sampled_friends_at)) %>%
    pull(.data$id_str)

  sampled_users <- setdiff(users, c(unsampled_users, not_in_graph))

  # all of these take value character(0) when empty
  list(
    not_in_graph = not_in_graph,
    sampled_friends_at_is_null = unsampled_users,
    sampled_friends_at_not_null = sampled_users
  )
}

#' Gets the friends for the given user that already exist in the DB.
#'
#' @inheritParams add_friend_edges_to_nodes_in_graph
#'
#' @return a 2-column tibble edge list with entries from the users in users
#' to their friends
db_get_friends <- function(users, cache) {

  log_trace(glue("db_get_friends(): {users}"))

  if (length(users) < 1) {
    return(empty_edge_list())
  }

  friend_query <- paste0(
    'WITH "MATCH (from:User),(to:User) WHERE from.id_str in [\\\'',
    glue_collapse(users, sep = "\\',\\'"),
    '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.id_str, to.id_str" AS query ',
    'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'
  )

  query_neo4j(friend_query, cache)

  log_trace("db_get_friends(): Neo4J query executed")

  tmp <- tempfile()

  copy_csv_from_docker("get_friends.csv", tmp, cache$container_name)

  results <- readr::read_csv(
    tmp,
    col_types = readr::cols(
      from.id_str = readr::col_character(),
      to.id_str = readr::col_character()
    )
  )

  log_trace("db_get_friends(): Data extracted from docker csv")

  if (ncol(results) != 2) {
    return(empty_edge_list())
  }

  tibble(from_id = results$from.id_str, to_id = results$to.id_str)
}
