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
  # Check if the container is already running
  ps <- system("docker ps -a", intern = TRUE)
  if(any(grepl("neocache_docker", ps, fixed = TRUE))) {
    # Exit immediately if the container is already running
    return(invisible())
  }

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




#' HELPER FUNCTION USED TO CLEAR THE DATABASE FOR TESTING,
#' REMOVE THIS FUNCTION ONCE TESTING IS DONE
clear____db <- function() {
  con <- get_connexion()

  "MATCH (n) DETACH DELETE n" %>%
    sup4j(con)
}
