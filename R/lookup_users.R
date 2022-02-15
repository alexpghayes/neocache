#' Get (cached) information about Twitter users
#'
#' Looks up information pertaining to a vector of provided users. Retrieves
#' cached data for users who have already been looked up, and fetches new
#' data for users who have not bee looked up yet from the Twitter API.
#'
#' @inheritParams nc_create_cache
#' @inheritParams rtweet::lookup_users
#'
#' @return A tibble where each row corresponds to a User and each column
#'   to one of the User properties. If a user cannot be sampled, should
#'   return nothing for that user. If no users can be sampled, should
#'   return an empty tibble with appropriate columns.
#'
#' @export
nc_lookup_users <- function(users, cache_name, token = NULL, retryonratelimit = TRUE, verbose = TRUE) {
  if (any(is.na(users))) {
    stop("`users` must not contain any `NA` entries.")
  }

  if (!is.character(users)) {
    stop("`users` must be a character vector.")
  }

  cache <- nc_activate_cache(cache_name)
  user_data <- db_lookup_users(users, cache)

  log_trace("User data found in Neo4J database:")
  log_trace(select(user_data, id_str, screen_name, sampled_at))

  sampled_data <- filter(user_data, !is.na(.data$sampled_at))

  not_in_graph_ids <- setdiff(users, user_data$id_str)
  not_sampled_ids <- filter(user_data, is.na(.data$sampled_at))$id_str

  if (length(not_sampled_ids) != 0) {

    log_debug(glue("Need data from Twitter API for {length(not_sampled_ids)} users already in graph..."))

    log_trace(
      glue(
        "Need data from Twitter API for {length(not_sampled_ids)} users ... ",
        "user(s): {users}",
        .trim = FALSE
      )
    )

    upgraded_user_data <- add_lookup_users_info_to_nodes_in_graph(not_sampled_ids, token, retryonratelimit, verbose, cache)

    log_trace(glue("User data found from Twitter for {length(not_sampled_ids)} users already in graph:"))
    log_trace(select(upgraded_user_data, id_str, screen_name, sampled_at))
  } else {

    log_debug(glue("Did not need data from Twitter API for any users already in graph."))
    upgraded_user_data <- empty_user()
  }

  if (length(not_in_graph_ids) != 0) {

    log_debug(glue("Need data from Twitter API for {length(not_in_graph_ids)} users not in graph..."))

    log_trace(
      glue(
        "Need data from Twitter API for {length(not_in_graph_ids)} users ... ",
        "user(s): {users}",
        .trim = FALSE
      )
    )

    db_add_new_users(not_in_graph_ids, cache)
    new_user_data <- add_lookup_users_info_to_nodes_in_graph(not_in_graph_ids, token, retryonratelimit, verbose, cache)

    log_trace(glue("User data found from Twitter for {length(not_in_graph_ids)} users not in graph:"))
    log_trace(select(new_user_data, id_str, screen_name, sampled_at))
  } else {
    log_trace("Did not need to add any new nodes to Neo4J.")
    new_user_data <- empty_user()
  }

  log_trace(glue("sampled_data is {nrow(sampled_data)} x {ncol(sampled_data)} with type signature"))
  log_trace(type_signature(sampled_data))
  log_trace(glue("upgraded_user_data is {nrow(upgraded_user_data)} x {ncol(upgraded_user_data)} with type signature"))
  log_trace(type_signature(upgraded_user_data))
  log_trace(glue("new_user_data is {nrow(new_user_data)} x {ncol(new_user_data)} with type signature"))
  log_trace(type_signature(new_user_data))

  bind_rows(empty_user(), sampled_data, upgraded_user_data, new_user_data)
}


#' Fetches the user's lookup_users data then updates their info in the graph.
#'
#' @param cache A `cache` object. See [nc_create_cache()].
#' @inheritParams nc_lookup_users
#'
#' @return tibble of user data
add_lookup_users_info_to_nodes_in_graph <- function(users, token, retryonratelimit, verbose, cache) {
  log_trace(glue("add_lookup_users_info_to_nodes_in_graph(): {users}"))

  # NOTE: do not set friends_sampled_at or followers_sampled_at here -- we need
  # to preserve whatever value those have in the database already

  # assumes all users already exist in the Neo4J database

  log_debug(glue("Need to request user data on {length(users)} users from API"))

  if (length(users) == 0) {
    return(empty_user())
  }

  tryCatch(
    expr = {
      log_info(glue("Making API request with rtweet::lookup_users for {length(users)} users"))

      user_info_raw <<- rtweet::lookup_users(
        users = users,
        token = token,
        retryonratelimit = retryonratelimit,
        verbose = verbose
      )

      log_trace(glue("Making API request with rtweet::lookup_users for: {users} ... results received."))
      log_trace(glue("Parsing results from API ... "))

      if (is.null(user_info_raw)) {
        stop("Results from API are `NULL`. This may be due to authentication failure.")
      }

      log_trace(glue("Parsing results from API ... done."))
      log_trace(glue("Parsed user data of {length(users)} users returned from API:"))
      log_trace(select(user_info_raw, any_of(twitter_properties)))
    },
    error = function(cnd) {

      # to understand HTTPs codes returned by Twitter see
      # https://developer.twitter.com/en/support/twitter-api/error-troubleshooting

      msg <- conditionMessage(cnd)

      log_warn(
        glue(
          "Making API request with rtweet::lookup_users for {length(users)} users ... ",
          "API request failed. Condition message: {msg}",
          .trim = FALSE
        )
      )

      # don't want to match on actual text of error message, which may change
      # with user locale / language settings. blargh

      if (grepl("404", msg)) {
        log_warn(
          glue(
            "Making API request with rtweet::lookup_users for {length(users)} users ... ",
            "404 error likely due to invalid user id, adding user to Neo4J DB ",
            "and treating as if user is missing all data.",
            .trim = FALSE
          )
        )
        user_info_raw <<- empty_user()
      } else {
        stop(cnd)
      }
    }
  )

  log_debug(glue("Received information on {nrow(user_info_raw)} users."))
  log_trace("user_info_raw is: ")
  log_trace(select(user_info_raw, id_str, screen_name))

  user_info_typed <- as_user_data(user_info_raw)

  user_info <- tibble(id_str = users) %>%
    left_join(user_info_typed, by = "id_str") %>%
    select(id_str, any_of(twitter_properties)) %>%
    mutate(
      sampled_at = Sys.time()
    )

  tmp <- tempfile()

  write_csv(user_info, tmp, na = "")
  copy_csv_to_docker(tmp, "lookup.csv", cache$container_name)

  # helper to generate SET string -- don't use this automatically because
  # if rtweet changes underlying return types might get a mixture of
  # return types in the node metadata and this would lead to hard to find
  # bugs when row-binding data from Neo4J with one column type and data from
  # rtweet with another column type

  # type <- sapply(user_info, class)
  #
  # open_cast <- case_when(
  #   types == "logical" ~ "toBoolean(",
  #   types == "integer" | types == "numeric" ~ "toInteger(",
  #   TRUE ~ ""
  # )
  #
  # close_cast <- case_when(
  #   type == "character" ~ "",
  #   TRUE ~ ")"
  # )
  #
  # set_string_help <- glue("SET n.{names(types)}={open_cast}row.{names(types)}{close_cast}")

  # don't include id_str

  set_string <- "SET n.id=toInteger(row.id) SET n.name=row.name SET n.screen_name=row.screen_name SET n.location=row.location SET n.description=row.description SET n.url=row.url SET n.protected=toBoolean(row.protected) SET n.followers_count=toInteger(row.followers_count) SET n.friends_count=toInteger(row.friends_count) SET n.listed_count=toInteger(row.listed_count) SET n.created_at=row.created_at SET n.favourites_count=toInteger(row.favourites_count) SET n.verified=toBoolean(row.verified) SET n.statuses_count=toInteger(row.statuses_count) SET n.profile_image_url_https=row.profile_image_url_https SET n.profile_banner_url=row.profile_banner_url SET n.default_profile=toBoolean(row.default_profile) SET n.default_profile_image=toBoolean(row.default_profile_image) SET n.sampled_at=row.sampled_at SET n.account_created_at=row.account_created_at"

  query <- glue(
    "LOAD CSV WITH HEADERS FROM 'file:///lookup.csv' AS row MATCH (n:User {{id_str:row.id_str}}) ",
    "{set_string} RETURN n.id_str"
  )

  query_neo4j(query, cache)

  user_info
}

#' Looks up users that are already in the database.
#'
#' @inheritParams add_lookup_users_info_to_nodes_in_graph
#' @include schema.R
#'
#' @return a tibble with any existing data for users
db_lookup_users <- function(users, cache) {

  log_debug(glue("Looking up information on {length(users)} users in Neo4J DB ..."))
  log_trace(glue("Looking up information on {length(users)} users in Neo4J DB ... user(s): {users}"))

  # return a tibble where each row corresponds to a User and each column
  # to one of the User properties. when a user in not present in the
  # database, should not return a row in the output tibble for that
  # user. if no users are in the db should return an empty tibble with one
  # column for each User property
  user_string <- glue_collapse(users, sep = '","')
  query <- glue('MATCH (n) WHERE n.id_str in ["{user_string}"] RETURN n')

  user_data <- query_neo4j(query, cache)

  log_debug(
    glue(
      "Looking up information on {length(users)} users in Neo4J DB ... ",
      "{NROW(user_data)} found in Neo4J database.",
      .trim = FALSE
    )
  )

  typed_user_data <- as_user_data(user_data[[1]])

  # If the users' data does not exist in the DB, return an empty lookup,
  # otherwise return the users' data
  if (length(typed_user_data) == 0) {
    return(empty_user())
  }

  # there's a cheeky hacky here: when nodes are added to Neo4J in the simplest
  # case we add nothing except the id_str. this bind_rows creates NA values
  # for sampled_*_at node properties if they don't exist in the underlying
  # Neo4J database. we should consider changing this behavior, because it
  # means that get_friends() has to rely on db_lookup_users() to learn
  # about node properties and can't use a separate Neo4J query, which seems
  # perfectly reasonable

  bind_rows(empty_user(), typed_user_data)
}
