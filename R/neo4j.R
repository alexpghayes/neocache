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
#' @inheritDotParams neo4r::call_neo4j
#'
#' @return the return value from call_neo4j
#'
#' @keywords internal
query_neo4j <- function(query, cache, ...) {
  con <- neo4j_api_connection(cache)

  # ignore one very specific message that we know we can safely ignore
  # `No data returned`, but let all other messages through. see
  # https://adv-r.hadley.nz/conditions.html#handling-conditions for some
  # limited background but not much and
  # https://stackoverflow.com/questions/65035810/is-it-possible-to-handle-simple-messages-in-r-if-yes-how
  # which is where this code comes from.

  # this is too fancy for you future alex, if shit breaks do something simple
  # instead of wasting a day trying to figure this out

  withCallingHandlers(
    message = function(cnd) {

      if (grepl("No data returned.", conditionMessage(cnd), )) {
        invokeRestart("muffleMessage")
      }
    },
    call_neo4j(query, con, ...)
  )

}


#' This function creates edges en masse between all the nodes provided in the
#' tbl argument.
#'
#' @param tbl tibble containing columns for 'to' and 'from' consisting of users
#' @param cache the cache to interface with
#'
#' @return the same tibble edge list that was provided as an argument
docker_bulk_connect_nodes <- function(tbl, cache) {

  tmp <- tempfile()

  # use cat instead of write to eliminate any trailing newline
  cat(paste0("from_id,to_id\n", paste0('"', tbl$from_id, '","', tbl$to_id, '"', collapse = "\n")), file = tmp)

  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  connect_query <- glue(
    "LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row ",
    "MATCH (from:User {{id_str:row.from_id}}) MATCH (to:User {{id_str:row.to_id}}) CREATE (from)-[:FOLLOWS]->(to)"
  )

  query_neo4j(connect_query, cache)

  tbl
}


#' Merges a batch of nodes to the graph with nothing but user_id's
#'
#' @param users a vector of users to generate MERGE queries for
#' @param cache the cache to interface with
db_add_new_users <- function(users, cache) {
  tmp <- tempfile()

  cat(paste0("id_str\n", paste0('"', users, '"', collapse = "\n")), file = tmp)

  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  add_qry <- glue("LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row MERGE (n:User {{id_str:row.id_str}})")
  query_neo4j(add_qry, cache)
}


#' Gets the followers for the given user that already exist in the DB.
#'
#' @param users a list of users who are already in the DB and
#' already have follower edge data
#' @param cache the cache to interface with
#'
#' @return a 2-column tibble edge list with entries from the users in users
#' to their followers
db_get_followers <- function(users, cache) {
  query_neo4j(
    paste0(
      'WITH "MATCH (from:User),(to:User) WHERE to.id_str in [\\\'',
      glue_collapse(users, sep = "\\',\\'"),
      '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.id_str, to.id_str" AS query ',
      'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'
    ),
    cache
  )

  tmp <- tempfile()
  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  results <- readr::read_csv(
    tmp,
    col_types = readr::cols(
      from.id_str = readr::col_character(),
      to.id_str = readr::col_character()
    )
  )

  if (length(results) != 2) {
    return(empty_edge_list())
  }

  tibble(from_id = results$from.id_str, to_id = results$to.id_str)
}

#' Export all follows from the Neo4J database to a csv
#'
#' @inheritParams nc_cache_exists
#' @param local_path path of csv
#'
#' @export
nc_export_all_follows <- function(cache_name, local_path) {
  cache <- nc_activate_cache(cache_name)
  log_trace(glue("Exporting all Follows edges from {cache_name} cache to CSV in Docker ..."))

  query <- glue(
    'CALL apoc.export.csv.query(',
    '"MATCH (a)-[r:FOLLOWS]->(b) ',
    'RETURN ',
    'a.id_str AS from, ',
    'b.id_str AS to", ',
    '"relationships.csv", null)'
  )

  # TODO: parse call_status to check from done: true, for now assume
  # someone will yell at us in a GH issue if they can't export their data
  call_status <- query_neo4j(query, cache, output = "json")

  log_trace(glue("Exporting all Follows edges from {cache_name} cache to CSV in Docker ... ... done"))
  log_trace(glue("Copying relationships.csv out of {cache_name} Docker container ..."))

  copy_csv_from_docker("relationships.csv", local_path, cache_name)

  log_trace(glue("Copying relationships.csv out of {cache_name} Docker container ... done"))
  log_trace(glue("Removing relationships.csv from {cache_name} Docker container ..."))

  remove_file_in_docker_container("relationships.csv", cache_name)

  log_trace(glue("Removing relationships.csv from {cache_name} Docker container ... done"))
  log_info(glue("All Follows relationships in {cache_name} cache exported to {local_path}"))
}

#' Export all users from the Neo4J database to a csv
#'
#' @inheritParams nc_export_all_follows
#'
#' @export
nc_export_all_users <- function(cache_name, local_path) {
  cache <- nc_activate_cache(cache_name)
  log_trace(glue("Exporting all Users from {cache_name} cache to `users.csv` in Docker ..."))

  query <- paste(
    c('CALL apoc.export.csv.query("MATCH (u:User) RETURN ',
      glue('n.{all_properties} AS {all_properties}'),
      '"./users.csv", null)'),
    collapse = " "
  )

  # TODO: parse call_status to check from done: true, for now assume
  # someone will yell at us in a GH issue if they can't export their data
  call_status <- query_neo4j(query, cache, output = "json")

  log_trace(glue("Exporting all Users from {cache_name} cache to `users.csv` in Docker ... done"))
  log_trace(glue("Copying `users.csv` out of {cache_name} Docker container ..."))

  copy_csv_from_docker("users.csv", local_path, cache_name)

  log_trace(glue("Copying `users.csv` out of {cache_name} Docker container ... done"))
  log_trace(glue("Removing `users.csv` from {cache_name} Docker container ..."))

  remove_file_in_docker_container("users.csv", cache_name)

  log_trace(glue("Removing `users.csv` from {cache_name} Docker container ... done"))
  log_info(glue("All User nodes in {cache_name} cache exported to {local_path}"))
}
