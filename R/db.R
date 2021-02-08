#' Title
#'
#' @return TODO
#' @export
#'
get_connexion <- function() {

  # model on gargle_auth / rtweet::get_token() roughly

  con <- neo4j_api$new(
    url = "http://localhost:7474",
    user = "neo4j",
    password = "pass"
  )

  if (!(con$ping() == 200)) {
    warning("Database connection not established.")
  }

  con
}

#' Title
#' TODO: Add docs
start_neo4j <- function() {
  # Start the Docker container
  # First try to run the existing Docker container
  if(system("docker start neocache_docker") == 1) {
    # Container does not exist, create the container
    if(system("docker run --name neocache_docker -p7474:7474 -p7687:7687 -d --env NEO4J_AUTH=neo4j/pass neo4j:3.5.21") != 0) {
      warning("Failed to initiate docker container.")
    }
  }
}

#' Title
#' TODO: Add docs
stop_neo4j <- function() {
  if(system("docker stop neocache_docker") == 1) {
    warning("Error returned when attempting to stop docker container.")
  }
}

#' TODO: Add docs
#'
#' @param query the CYPHER query to be passed to call_neo4j
#' @param con the neo4j connection object to be passed to call_neo4j
#'
#' @return the return value from call_neo4j
#'
#' @keywords internal
sup4j <- function(query, con = get_connexion()) {
  suppressMessages(call_neo4j(query, con))
}




#' This function only creates edges between nodes according to the supplied tbl.
#' Node creation must happen BEFORE this function is called.
#'
#' @param tbl tibble containing columns for 'to' and 'from'
#' TODO: add docs
docker_bulk_connect_nodes <- function(tbl) {
  # Create temp file to write the data into
  tmp <- tempfile()

  # Write the data; we use cat instead of write to eliminate any trailing newline
  cat(paste0('to,from\n', paste0('"', tbl$to, '","', tbl$from, '"', collapse='\n')), file = tmp)

  # Copy the data into the docker container
  system(glue("docker cp {tmp} neocache_docker:/var/lib/neo4j/import/data.csv"))

  # Add a node for each of the root user's friends and connect the root user to them
  ## This query runs on joe50k in ~2.75 minutes
  connect_qry <- glue("LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row ",
                      "MATCH (to:User {{user_id:row.to}}) MATCH (from:User {{user_id:row.from}}) CREATE (from)-[:FOLLOWS]->(to)")
  sup4j(connect_qry)

  file.remove(tmp)

  tbl
}


#' Merges a batch of nodes to the graph with nothing but user_id's
docker_bulk_merge_users <- function(user_ids) {
  tmp <- tempfile()

  cat(paste0('user_id\n', paste0('"', user_ids, '"', collapse='\n')), file = tmp)
  system(glue("docker cp {tmp} neocache_docker:/var/lib/neo4j/import/data.csv"))

  add_qry <- glue("LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row MERGE (n:User {{user_id:row.user_id}})")
  sup4j(add_qry)

  file.remove(tmp)
}


#' Gets the friends for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have friend edge data
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their friends
db_get_friends <- function(user_ids) {

  con <- get_connexion()

  results <- sup4j(
    glue('MATCH (from:User),(to:User) WHERE from.user_id in ["',
         glue_collapse(user_ids, sep = '","'),
         '"] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id',
    ),
    con
  )

  if (length(results) != 2) {
    return(empty_user_edges())
  }

  tibble(from = results$from.user_id$value, to = results$to.user_id$value)
}


#' Gets the followers for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have follower edge data
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their followers
db_get_followers <- function(user_ids) {

  con <- get_connexion()

  # TODO: This cypher query needs to be made faster by matching based on each
  #       user ID individually instead of using `WHERE from.user_id in [...]`
  results <- sup4j(
    glue('MATCH (from:User),(to:User) WHERE to.user_id in ["',
         glue_collapse(user_ids, sep = '","'),
         '"] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id',
    ),
    con
  )

  if (length(results) != 2) {
    return(empty_user_edges())
  }

  tibble(from = results$from.user_id$value, to = results$to.user_id$value)
}


#' HELPER FUNCTION USED TO CLEAR THE DATABASE FOR TESTING,
#' REMOVE THIS FUNCTION ONCE TESTING IS DONE
clear____db <- function() {
  con <- get_connexion()

  "MATCH (n) DETACH DELETE n" %>%
    sup4j(con)
}
