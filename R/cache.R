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

  if(create_docker_container(cache_name, neo4j_user, neo4j_pass, http_port, bolt_port)) {
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

    write_rds(cache, cache_save_path)

    cache
  } else {
    log_error("Failed to create a Docker container.")
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

  print(cache_save_path)

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
start_cache <- function(cache_name) {
  start_docker(cache_name)
}
