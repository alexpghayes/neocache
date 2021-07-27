#' Create a cache object
#'
#' Creates and saves a new cache instance with the given information.  Also
#' generates a new Docker container with the given information if one does
#' not already exist.
#'
#' @param cache_name the name of the cache and associated Docker container
#' @param url the url used to connect to the Neo4j database
#' @param neo4j_pass the password for the Neo4j instance
#' @param http_port the port that Neo4j will use from inside of the Docker container for its HTTP connection
#' @param bolt_port the port that Neo4j will use from inside of the Docker container for its Bolt connection
#'
#' @importFrom readr write_rds
#' @importFrom pingr is_up
#' @importFrom rappdirs app_dir
#'
#' @export
#'
nc_create_cache <- function(cache_name, neo4j_pass = "password", http_port = 7474, bolt_port = 7687, url = NULL) {
  if (!find_docker()) {
    stop("Docker does not appear to be running. Please start Docker and then try again.")
  }

  if (docker_container_exists(cache_name)) {
    stop(glue("A Docker container by the name of {cache_name} already exists."))
  }

  if (grepl(" ", neo4j_pass)) {
    stop("The Neo4j password must not contain any spaces. Please try again with a new password.")
  }

  current_caches <- get_all_caches()

  current_http_ports <- vapply(current_caches, function(x) x$http_port, numeric(1))
  current_bolt_ports <- vapply(current_caches, function(x) x$bolt_port, numeric(1))

  if (http_port %in% current_http_ports) {
    stop(glue("http port must be unique amongst caches and port {http_port} is already assigned to another cache."))
  }

  if (bolt_port %in% current_bolt_ports) {
    stop(glue("bolt port must be unique amongst caches and port {bolt_port} is already assigned to another cache."))
  }

  log_trace("Creating {cache_name} Docker container...")

  if (!create_docker_container(cache_name, neo4j_pass, http_port, bolt_port)) {
    stop(glue("Failed to create the {cache_name} Docker container."))
  }


  log_trace("Creating {cache_name} Docker container ... done")

  cache_metadata <- list(
    container_name = cache_name,
    neo4j_pass = neo4j_pass,
    url = glue("http://localhost:{http_port}"),
    http_port = http_port,
    bolt_port = bolt_port
  )

  class(cache_metadata) <- "neocache_metadata"

  cache_save_path <- cache_path(cache_name)

  log_trace(glue("Writing cache metadata to {cache_save_path}"))
  write_rds(cache_metadata, cache_save_path)

  log_trace("Creating unique user_id constraint in Neo4J database ... ")

  cache <- nc_activate_cache(cache_name)

  query_neo4j("CREATE CONSTRAINT ON (n:User) ASSERT n.user_id IS UNIQUE", cache)

  log_trace("Creating unique user_id constraint in Neo4J database ... done")

  invisible(NULL)
}


#' @method print neocache_metadata
#' @export
print.neocache_metadata <- function(x, ...) {
  cat(glue("Neocache [name = {x$container_name}, url = {x$url}]"))
}


#' Retrieves a previously generated cache object.
#'
#' @param cache_name the name of the cache to use
#' @return a list object containing information pertaining to the associated
#'         Docker container and Neo4j instance
#'
#' @importFrom readr read_rds
#' @importFrom rappdirs app_dir
#'
#' @keywords internal
#'
#' @export
get_cache <- function(cache_name) {

  # first preference to named cache
  # second preference to the "use_cache" cache
  # error

  if (!nc_cache_exists(cache_name)) {
    stop(glue("There is no cache named {cache_name}."))
  }

  read_rds(cache_path(cache_name))
}

# TODO: get_cache_and_activate_if_necessary()


#' Starts the Docker container and Neo4j instance for the given cache
#'
#' @param cache the cache object that the user wishes to start
#'
#' @keywords internal
#'
#' @export
nc_activate_cache <- function(cache_name) {

  # DO NOT PASS A CACHE, pass a cache name

  log_trace(glue("Retrieving {cache_name} cache metadata file ... "))

  cache <- get_cache(cache_name)

  log_trace(glue("Retrieving {cache_name} cache metadata file ... done."))

  log_trace(glue("Activating {cache_name} cache ..."))

  if (is_active(cache)) {
    log_trace(glue("Activating {cache_name} cache ... already active"))
    return(invisible(cache))
  }

  log_trace("Cache in not active, attempting to activate.")

  if (is_up("127.0.0.1", port = cache$http_port)) {
    stop("The selected HTTP port is already in use.")
  }

  # if (is_up("127.0.0.1", port = cache$bolt_port)) {
  #   stop("The selected Bolt port is already in use.")
  # }

  log_trace("Looking for docker ...")

  if (!find_docker()) {
    stop("Could not find docker. See `find_docker()` for details.")
  }

  log_trace("Looking for docker ... found.")
  log_trace(glue("Starting {cache$container_name} docker container ..."))

  if (!start_docker(cache$container_name)) {
    stop(
      glue(
        "Docker container {cache$container_name} may not exist. See `nc_create_cache()`."
      )
    )
  }

  log_trace(glue("Starting {cache$container_name} docker container ... done."))
  log_trace(glue("Establishing connection with {cache_name} Neo4J database ..."))

  while (!is_active(cache)) {
    Sys.sleep(3)

    log_trace(glue("Establishing connection with {cache_name} Neo4J database ... waiting"))

    if (!is_docker_container_running(cache$container_name)) {
      stop(glue("Docker container {cache$container_name} is not longer running, cannot activate cache."))
    }
  }

  log_trace(glue("Establishing connection with {cache_name} Neo4J database ... done."))
  invisible(cache)
}

is_active <- function(cache) {
  con <- neo4j_api_connection(cache)
  try(con$get_version(), silent = TRUE) == "3.5.21" # this is a hack
}


#' Stops the Docker container and Neo4j instance for the given cache
#'
#' @param cache the cache object that the user wishes to start
#'
#' @keywords internal
#'
#' @export
nc_deactivate_cache <- function(cache_name) {
  log_trace(glue("Deactivating {cache_name} cache ..."))

  if (stop_docker(cache_name)) {
    log_trace(glue("Deactivating {cache_name} cache ... done."))
  } else {
    log_warn(glue("Deactivating {cache_name} cache ... failed."))
  }
}

#' Removes the Docker container and cache save file corresponding to the given
#' cache
#'
#' @param cache the cache object that the user wishes to start
#'
#' @importFrom rappdirs app_dir
#'
#' @export
nc_destroy_cache <- function(cache_name) {
  if (docker_container_exists(cache_name)) {
    log_trace(glue("Removing {cache_name} Docker container ..."))

    nc_deactivate_cache(cache_name)

    if (remove_docker_container(cache_name)) {
      log_trace(glue("Removing {cache_name} cache Docker container ... done"))
    } else {
      log_warn(glue("Removing {cache_name} cache Docker container ... FAILED"))
    }
  } else {
    log_trace(glue("Removing {cache_name} cache Docker container ... none exists."))
  }

  path <- cache_path(cache_name)

  if (file.exists(path)) {
    log_trace(glue("Removing {cache_name} cache metadata file ..."))

    if (file.remove(path)) {
      log_trace(glue("Removing {cache_name} cache metadata file ... done"))
    } else {
      log_warn(glue("Removing {cache_name} cache metadata file ... FAILED"))
    }
  }

  invisible(NULL)
}

cache_path <- function(cache_name) {
  cache_folder <- rappdirs::app_dir("neocache")$cache()

  if (!dir.exists(cache_folder)) {
    dir.create(cache_folder, recursive = TRUE)
  }

  file.path(
    cache_folder,
    glue("{cache_name}.rds")
  )
}

#' Remove all data from a cache
#'
#' @param cache_name TODO
#'
#' @return
#' @export
#'
nc_empty_cache <- function(cache_name, check_with_me_first = TRUE) {
  if (check_with_me_first) {
    if (usethis::ui_yeah("Do you want to empty the {cache_name} cache?")) {
      cache <- nc_activate_cache(cache_name)
      query_neo4j("MATCH (n) DETACH DELETE n", cache)
      return(invisible(NULL))
    }
  }

  cache <- nc_activate_cache(cache_name)
  query_neo4j("MATCH (n) DETACH DELETE n", cache)
  invisible(NULL)
}

#' Check if a cache with a given name exists
#'
#' @param cache_name
#'
#' @return
#' @export
#'
nc_cache_exists <- function(cache_name) {
  docker_container_exists(cache_name)
}

#' Check caches with saved information
#'
#' @export
#'
nc_sitrep <- function() {
  caches <- get_all_caches()

  cli::cli_h1("Caches")

  if (length(caches) < 1) {
    cat("  - No caches yet")
  }


  for (cache in caches) {
    status <- if (is_active(cache)) "active" else "not active"
    cat(glue("  - {cache$container_name} ({status}) \n", .trim = FALSE))
  }

  invisible(NULL)
}

get_all_caches <- function() {
  cache_metadata_files <- list.files(
    path = rappdirs::app_dir("neocache")$cache(),
    pattern = "*.\\.rds",
    full.names = TRUE
  )

  lapply(cache_metadata_files, read_rds)
}
