#' Get the connexion object that facilitates access to the Neo4j database
#'
#' @return connexion object
get_connexion <- function(cache) {
  con <- neo4j_api$new(
    url = cache$url,
    user = cache$neo4j_user,
    password = cache$neo4j_pass
  )

  con
}




#' Stops the Neo4j Docker container.
stop_neo4j <- function() {
  if (!stop_neocache_docker()) {
    warning("Error returned when attempting to stop docker container.")
  } else {
    log_debug("Successfully shut down Neo4j and the Neocache Docker container.")
  }
}


#' Sends CYPHER queries to a given connexion object while suppressing output
#' messages that call_neo4j throws.
#'
#' @param query the CYPHER query to be passed to call_neo4j
#' @param con the neo4j connection object to be passed to call_neo4j
#'
#' @return the return value from call_neo4j
#'
#' @keywords internal
sup4j <- function(query, con) {
  suppressMessages(call_neo4j(query, con))
}

#' This function creates edges en masse between all the nodes provided in the
#' tbl argument.
#'
#' @param tbl tibble containing columns for 'to' and 'from' consisting of user_ids
#' @return the same tibble edge list that was provided as an argument
docker_bulk_connect_nodes <- function(tbl, cache) {
  # Create temp file to write the data into
  tmp <- tempfile()

  # Write the data; we use cat instead of write to eliminate any trailing newline
  cat(paste0("to,from\n", paste0('"', tbl$to, '","', tbl$from, '"', collapse = "\n")), file = tmp)

  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  on.exit(file.remove(tmp))

  # Add a node for each of the root user's friends and connect the root user to them
  ## This query runs on joe50k in ~2.75 minutes
  connect_qry <- glue(
    "LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row ",
    "MATCH (to:User {{user_id:row.to}}) MATCH (from:User {{user_id:row.from}}) CREATE (from)-[:FOLLOWS]->(to)"
  )

<<<<<<< HEAD
  sup4j(connect_qry, get_connexion(cache))
=======
  sup4j(connect_qry)

  tbl
>>>>>>> 4dcc5a00cbe71259977598624d838758224f1d05
}


#' Merges a batch of nodes to the graph with nothing but user_id's
#'
#' @param user_ids a vector of user_ids to generate MERGE queries for
docker_bulk_merge_users <- function(user_ids, cache) {
  tmp <- tempfile()

  cat(paste0("user_id\n", paste0('"', user_ids, '"', collapse = "\n")), file = tmp)
<<<<<<< HEAD

  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

  on.exit(file.remove(tmp))
=======
  copy_csv_to_docker(tmp)
>>>>>>> 4dcc5a00cbe71259977598624d838758224f1d05

  add_qry <- glue("LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row MERGE (n:User {{user_id:row.user_id}})")
  sup4j(add_qry, get_connexion(cache))

}


#' Gets the friends for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have friend edge data
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their friends
db_get_friends <- function(user_ids, cache) {
  if(is.na(user_ids)) {
    return(empty_user_edges())
  }

  con <- get_connexion(cache)

  sup4j(
    paste0(
      'WITH "MATCH (from:User),(to:User) WHERE from.user_id in [\\\'',
      glue_collapse(user_ids, sep = "\\',\\'"),
      '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id" AS query ',
      'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'
    ),
    con
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


#' Gets the followers for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have follower edge data
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their followers
db_get_followers <- function(user_ids, cache) {
  con <- get_connexion(cache)

  sup4j(
    paste0(
      'WITH "MATCH (from:User),(to:User) WHERE to.user_id in [\\\'',
      glue_collapse(user_ids, sep = "\\',\\'"),
      '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id" AS query ',
      'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'
    ),
    con
  )

  tmp <- tempfile()

  copy_csv_to_docker(tmp, "data.csv", cache$container_name)

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

