#' Creates and saves a new cache instance with the given information.  Also
#' generates a new Docker container with the given information if one does
#' not already exist.
#'
#' @param cache_name the name of the cache and associated Docker container
#' @param url the url used to connect to the Neo4j database
#' @param neo4j_user the username for the Neo4j instance
#' @param neo4j_pass the password for the Neo4j instance
#'
#' @importFrom readr write_rds
#' @export
new_cache <- function(cache_name, neo4j_user = "neo4j", neo4j_pass = "password", url = "http://localhost:7474", http_port = 7474, bolt_port = 7687) {
  if(!startsWith(url, "http://")) {
    url <- "http://" + url
    log_debug(glue("Appending http:// to beginning of URL; new URL: {url}"))
  }
  if(!endsWith(url, glue(":{http_port}"))) {
    url <- url + ":" + as.character(http_port)
    log_debug(glue("Appending port to end of URL; new URL: {url}"))
  }

  log_debug("Creating {cache_name} Docker container...")
  if(create_docker_container(cache_name, neo4j_user, neo4j_pass, http_port, bolt_port)) {
    log_debug("{cache_name} Docker container successfully created.")

    cache <- list(
      container_name = cache_name,
      neo4j_user = neo4j_user,
      neo4j_pass = neo4j_pass,
      url = url,
      http_port = http_port
    )

    cache_folder <- rappdirs::app_dir("neocache")$cache()
    if(!dir.exists(cache_folder)) {
      dir.create(cache_folder, recursive = TRUE)
    }

    cache_save_path <- file.path(
      rappdirs::app_dir("neocache")$cache(),
      glue("{cache_name}.rds")
    )

    log_debug(glue("Writing cache rds to {cache_save_path}"))
    write_rds(cache, cache_save_path)
    cache
  } else {
    log_error("Failed to create the {cache_name} Docker container.")
  }
}


#' @param name the name of the cache to use
#' @return a list object containing information pertaining to the associated
#'         Docker container and Neo4j instance
#'
#' @importFrom readr read_rds
#' @export
get_cache <- function(cache_name) {
  cache_save_path <- file.path(
    rappdirs::app_dir("neocache")$cache(),
    glue("{cache_name}.rds")
  )

  log_debug(glue("Loading cache rds from {cache_save_path}"))

  if(file.exists(cache_save_path)) {
    read_rds(cache_save_path)
  } else {
    log_error("This cache does not exist. Try using new_cache(...) instead.")
  }
}



#' Starts the Docker container and Neo4j instance for the given cache
#'
#' @param cache the cache object that the user wishes to start
#'
#' @export
load_cache <- function(cache) {
  # First check if the container is already running
  if(is_docker_container_running(cache$container_name)) {
    log_info(glue(
      "{cache$container_name} Docker container appears to be running already."
    ))
  } else {
    # Check if Docker is running
    log_info("Checking the status of Docker...")
    if(!find_docker()) {
      stop("Docker does not appear to be running. Please start Docker and try again.")
    }
    log_info("Docker seems to be running.\n")

    # Start the Docker container
    log_info(glue("Attempting to start {cache$container_name} Docker container..."))
    if(!start_docker(cache$container_name)) {
      stop("This Docker container does not appear to exist. If you create a cache ",
           "with new_cache(...) this container will be automatically created for you.")
    }
  }

  # Wait for Neo4j to launch
  log_info(
    "{cache$container_name} Docker container successfully launched.\n",
  )
  log_info(
    "Waiting for Neo4j to come online, this may take a while..."
  )

  con <- get_connexion(cache)
  while(try(con$ping()) != 200) {
    Sys.sleep(8)
  }

  log_info("Neo4j successfully started.")
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
