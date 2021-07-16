neo4j_api_connection <- function(cache) {
  neo4j_api$new(
    url = cache$url,
    user = "neo4j",
    password = cache$neo4j_pass
  )
}

#' Sends CYPHER queries to a given connexion object while suppressing output
#' messages that call_neo4j throws.
#'
#' @param query the CYPHER query to be passed to call_neo4j
#' @param cache the cache to interface with
#'
#' @return the return value from call_neo4j
#'
#' @keywords internal
query_neo4j <- function(query, cache) {
  con <- neo4j_api_connection(cache)

  # ignore one very specific message that we know we can safely ignore
  # `No data returned`, but let all other messages through. see
  # https://adv-r.hadley.nz/conditions.html#handling-conditions for some
  # limited background but not much and
  # https://stackoverflow.com/questions/65035810/is-it-possible-to-handle-simple-messages-in-r-if-yes-how
  # which is where this code comes from.

  # this is too fancy for you future alex, if shit break do something simple
  # instead of wasting a day trying to figure this out

  withCallingHandlers(
    message = function(cnd) {
      if (conditionMessage(cnd) == "No data returned.")
        invokeRestart("muffleMessage")
    },
    call_neo4j(query, con)
  )

}


#' This function creates edges en masse between all the nodes provided in the
#' tbl argument.
#'
#' @param tbl tibble containing columns for 'to' and 'from' consisting of user_ids
#' @param cache the cache to interface with
#'
#' @return the same tibble edge list that was provided as an argument
docker_bulk_connect_nodes <- function(tbl, cache) {
  # Create temp file to write the data into
  tmp <- tempfile()

  # Write the data; we use cat instead of write to eliminate any trailing newline
  cat(paste0("to,from\n", paste0('"', tbl$to, '","', tbl$from, '"', collapse = "\n")), file = tmp)

  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  # Add a node for each of the root user's friends and connect the root user to them
  ## This query runs on joe50k in ~2.75 minutes
  connect_qry <- glue(
    "LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row ",
    "MATCH (to:User {{user_id:row.to}}) MATCH (from:User {{user_id:row.from}}) CREATE (from)-[:FOLLOWS]->(to)"
  )

  query_neo4j(connect_qry, cache)

  tbl
}


#' Merges a batch of nodes to the graph with nothing but user_id's
#'
#' @param user_ids a vector of user_ids to generate MERGE queries for
#' @param cache the cache to interface with
db_add_new_users <- function(user_ids, cache) {
  tmp <- tempfile()

  cat(paste0("user_id\n", paste0('"', user_ids, '"', collapse = "\n")), file = tmp)

  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  add_qry <- glue("LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row MERGE (n:User {{user_id:row.user_id}})")
  query_neo4j(add_qry, cache)
}





#' Gets the followers for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have follower edge data
#' @param cache the cache to interface with
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their followers
db_get_followers <- function(user_ids, cache) {
  query_neo4j(
    paste0(
      'WITH "MATCH (from:User),(to:User) WHERE to.user_id in [\\\'',
      glue_collapse(user_ids, sep = "\\',\\'"),
      '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id" AS query ',
      'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'
    ),
    cache
  )

  tmp <- tempfile()
  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  results <- readr::read_csv(
    tmp,
    col_types = readr::cols(
      from.user_id = readr::col_character(),
      to.user_id = readr::col_character()
    )
  )

  if (length(results) != 2) {
    return(empty_edge_list())
  }

  tibble(from = results$from.user_id, to = results$to.user_id)
}
