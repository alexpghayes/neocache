main_cache <- function() {

}
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
#'
#' @export
start_neo4j <- function() {
  log_debug("Checking the status of Docker...")
  # Check if Docker is running
  if (!find_docker()) {
    stop("Docker does not appear to be running. Please start Docker and try again.")
  }
  log_debug("Docker seems to be running.\n")

  log_debug("Attempting to start Neocache container...")
  # Start the Docker container
  # First try to run the existing Docker container
  if (!start_neocache_docker()) {
    # Container does not exist, create the container
    if (!create_neocache_docker()) {
      stop("Failed to initiate docker container.")
    }
  }

  log_debug(
    "Neocache container successfully launched.\n",
    "\nWaiting for Neo4j to come online, this may take a while..."
  )

  # Now we wait for Neo4j to finish starting up
  con <- get_connexion()

  while (try(con$ping()) != 200) {
    Sys.sleep(3)
  }

  log_debug("Neo4j successfully started.")
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
sup4j <- function(query, con = get_connexion()) {
  suppressMessages(call_neo4j(query, con))
}
