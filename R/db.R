#' Get the connexion object that facilitates access to the Neo4j database
#'
#' @return connexion object
get_connexion <- function() {
  con <- neo4j_api$new(
    url = "http://localhost:7474",
    user = "neo4j",
    password = "pass"
  )

  con
}


#' Starts the Neo4j Docker container; creates and sets up the container if it
#' does not already exist.
start_neo4j <- function() {
  message("Checking the status of Docker...")
  # Check if Docker is running
  if(system("docker system info", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("Docker does not appear to be running. Please start Docker and try again.")
  }
  message("Docker seems to be running.\n")

  message("Attempting to start Neocache container...")
  # Start the Docker container
  # First try to run the existing Docker container
  if(system("docker start neocache_docker", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    # Container does not exist, create the container
    if(system("docker run --name neocache_docker -p7474:7474 -p7687:7687 -d -e NEO4J_AUTH=neo4j/pass -e NEO4J_apoc_export_file_enabled=true -e NEO4J_apoc_import_file_enabled=true -e NEO4J_apoc_import_file_use__neo4j__config=true -e NEO4JLABS_PLUGINS=[\\\"apoc\\\"] neo4j:3.5.21",
              ignore.stdout = TRUE) != 0) {
      stop("Failed to initiate docker container.")
    }
  }

  message("Neocache container successfully launched.\n",
          "\nWaiting for Neo4j to come online, this may take a while...")
  # Now we wait for Neo4j to finish starting up
  con <- get_connexion()
  while(TRUE) {
    Sys.sleep(12)
    tryCatch({
      if(con$ping() == 200) {
        break
      }
    }, error = function(e) {}, condition = function(e) {})
  }
  message("Neo4j successfully started.")
}


#' Stops the Neo4j Docker container.
stop_neo4j <- function() {
  if(system("docker stop neocache_docker", ignore.stdout = TRUE) != 0) {
    warning("Error returned when attempting to stop docker container.")
  } else {
    message("Successfully shut down Neo4j and the Neocache Docker container.")
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
sup4j <- function(query, con = get_connexion()) {
  suppressMessages(call_neo4j(query, con))
}


#' This function creates edges en masse between all the nodes provided in the
#' tbl argument.
#'
#' @param tbl tibble containing columns for 'to' and 'from' consisting of user_ids
#' @return the same tibble edge list that was provided as an argument
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
#'
#' @param user_ids a vector of user_ids to generate MERGE queries for
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

  sup4j(
    paste0('WITH "MATCH (from:User),(to:User) WHERE from.user_id in [\\\'',
            glue_collapse(user_ids, sep = "\\',\\'"),
            '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id" AS query ',
            'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'),
    con
  )

  tmp <- tempfile()
  system(glue("docker cp neocache_docker:/var/lib/neo4j/import/get_friends.csv {tmp}"))
  results <- readr::read_csv(tmp, col_types = readr::cols(from.user_id=readr::col_character(), to.user_id=readr::col_character()))
  file.remove(tmp)


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
db_get_followers <- function(user_ids) {
  con <- get_connexion()

  sup4j(
    paste0('WITH "MATCH (from:User),(to:User) WHERE to.user_id in [\\\'',
           glue_collapse(user_ids, sep = "\\',\\'"),
           '\\\'] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id" AS query ',
           'CALL apoc.export.csv.query(query, "get_friends.csv", {}) YIELD file RETURN file'),
    con
  )

  tmp <- tempfile()
  system(glue("docker cp neocache_docker:/var/lib/neo4j/import/get_friends.csv {tmp}"))
  results <- readr::read_csv(tmp, col_types = readr::cols(from.user_id=readr::col_character(), to.user_id=readr::col_character()))
  file.remove(tmp)

  if (length(results) != 2) {
    return(empty_user_edges())
  }

  tibble(from = results$from.user_id, to = results$to.user_id)
}


#' HELPER FUNCTION USED TO CLEAR THE DATABASE FOR TESTING,
#' REMOVE THIS FUNCTION ONCE TESTING IS DONE
clear____db <- function() {
  con <- get_connexion()

  "MATCH (n) DETACH DELETE n" %>%
    sup4j(con)
}
