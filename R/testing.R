
#' start_neo4j but used exclusively in unit testing
#'
#' @export
start_test_neo4j <- function() {
  message("Checking the status of Docker...")
  # Check if Docker is running
  if (system("docker system info", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("Docker does not appear to be running. Please start Docker and try again.")
  }
  message("Docker seems to be running.\n")

  message("Attempting to start Neocache container...")
  # Start the Docker container
  # First try to run the existing Docker container
  if (system("docker start neocache_test_container", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    # Container does not exist, create the container
    if (system("docker run --name neocache_test_container -p7474:7474 -p7687:7687 -d -e NEO4J_AUTH=neo4j/pass -e NEO4J_apoc_export_file_enabled=true -e NEO4J_apoc_import_file_enabled=true -e NEO4J_apoc_import_file_use__neo4j__config=true -e NEO4JLABS_PLUGINS=[\\\"apoc\\\"] neo4j:3.5.21",
               ignore.stdout = TRUE
    ) != 0) {
      stop("Failed to initiate docker container.")
    }
  }

  message(
    "Neocache container successfully launched.\n",
    "\nWaiting for Neo4j to come online, this may take a while..."
  )
  # Now we wait for Neo4j to finish starting up
  con <- get_connexion()
  while (TRUE) {
    Sys.sleep(12)
    tryCatch(
      {
        if (con$ping() == 200) {
          break
        }
      },
      error = function(e) {},
      condition = function(e) {}
    )
  }
  message("Neo4j successfully started.")
}


#' stop_neo4j but used exclusively in unit testing
stop_test_neo4j <- function() {
  if (system("docker stop neocache_test_container", ignore.stdout = TRUE) != 0) {
    warning("Error returned when attempting to stop docker container.")
  } else {
    message("Successfully shut down Neo4j and the Neocache Docker container.")
  }
}

#' HELPER FUNCTION USED TO CLEAR THE DATABASE FOR TESTING,
#' REMOVE THIS FUNCTION ONCE TESTING IS DONE
clear____db <- function() {
  con <- get_connexion()

  "MATCH (n) DETACH DELETE n" %>%
    sup4j(con)
}
