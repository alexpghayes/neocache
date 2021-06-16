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
#' @examples
#'
#' example_cache <- new_cache("example_cache")
#' example_cache
#'
#' cache_sitrep()
#'
#' remove_cache(example_cache)
#'
#' cache_sitrep()
#'
new_cache <- function(cache_name, neo4j_pass = "password", http_port = 7474, bolt_port = 7687, url = NULL) {
  # Make sure the Docker is running first
  if (!find_docker()) {
    stop("Docker does not appear to be running. Please start Docker and then try again.")
  }

  # Make sure the container does not already exist
  if (docker_container_exists(cache_name)) {
    stop(glue("A Docker container by the name of {cache_name} already exists."))
  }

  # Make sure the password does not contain spaces
  if (grepl(" ", neo4j_pass)) {
    stop("The Neo4j password must not contain any spaces. Please try again with a new password.")
  }

  # Check to make sure that the selected ports are currently available
  if (is_up("127.0.0.1", port = http_port)) {
    stop("The selected HTTP port is currently in use. Please try another port.")
  }
  if (is_up("127.0.0.1", port = bolt_port)) {
    stop("The selected Bolt port is currently in use. Please try another port.")
  }

  # Warn the user that conflicts may arise when using default ports
  if (http_port == 7474) {
    warning(
      "You have selected the default HTTP port 7474. Conflicts will arise if you try ",
      "to run multiple caches with the same port selected."
    )
  }
  if (bolt_port == 7687) {
    warning(
      "You have selected the default Bolt port 7687. Conflicts will arise if you try ",
      "to run multiple caches with the same port selected."
    )
  }

  # Tidy up the URL string to make sure that it is formatted properly
  if (is.null(url)) {
    url <- paste0("http://localhost:", http_port)
    message("While this cache is running, the Neo4j web interace will be available at ", url)
  }

  # Create the Docker container and cache rds file
  log_debug("Creating {cache_name} Docker container...")
  if (create_docker_container(cache_name, neo4j_pass, http_port, bolt_port)) {
    log_debug("{cache_name} Docker container successfully created.")

    cache <- list(
      container_name = cache_name,
      neo4j_pass = neo4j_pass,
      url = url,
      http_port = http_port,
      bolt_port = bolt_port
    )

    cache_folder <- rappdirs::app_dir("neocache")$cache()
    if (!dir.exists(cache_folder)) {
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
    stop("Failed to create the {cache_name} Docker container.")
  }
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
#' @export
get_cache <- function(cache_name) {
  cache_save_path <- file.path(
    rappdirs::app_dir("neocache")$cache(),
    glue("{cache_name}.rds")
  )

  log_debug(glue("Loading cache rds from {cache_save_path}"))

  if (file.exists(cache_save_path)) {
    read_rds(cache_save_path)
  } else {
    stop("This cache does not exist. Try using new_cache(...) instead.")
  }
}


#' Starts the Docker container and Neo4j instance for the given cache
#'
#' @param cache the cache object that the user wishes to start
#'
#' @export
load_cache <- function(cache) {
  # Check to make sure that the selected ports are currently available
  if (is_up("127.0.0.1", port = cache$http_port)) {
    stop("The selected HTTP port is already in use.")
  }
  if (is_up("127.0.0.1", port = cache$bolt_port)) {
    stop("The selected Bolt port is already in use.")
  }

  # First check if the container is already running
  if (is_docker_container_running(cache$container_name)) {
    log_info(glue(
      "{cache$container_name} Docker container appears to be running already."
    ))
  } else {
    # Check if Docker is running
    log_info("Checking the status of Docker...")
    if (!find_docker()) {
      stop("Docker does not appear to be running. Please start Docker and try again.")
    }
    log_info("Docker seems to be running.\n")

    # Start the Docker container
    log_info(glue("Attempting to start {cache$container_name} Docker container..."))
    if (!start_docker(cache$container_name)) {
      stop(
        "This Docker container does not appear to exist. If you create a cache ",
        "with new_cache(...) this container will be automatically created for you."
      )
    }

    log_info(
      glue("{cache$container_name} Docker container successfully launched.\n")
    )
  }

  # Wait for Neo4j to launch
  log_info(
    "Waiting for Neo4j to come online, this may take a while..."
  )

  while (!is_up("localhost", port = cache$http_port)) {
    # Wait to try again
    Sys.sleep(10)

    # Check to make sure the container is still running before trying again
    if (!is_docker_container_running(cache$container_name)) {
      stop("Something went wrong and the Docker container shut itself down.")
    }
  }

  log_info("Neo4j successfully started.")
}


#' Stops the Docker container and Neo4j instance for the given cache
#'
#' @param cache the cache object that the user wishes to start
#'
#' @export
unload_cache <- function(cache) {
  # Make sure that the Docker container is actually running
  if (!is_docker_container_running(cache$container_name)) {
    stop(glue("The {cache$container_name} Docker container does not appear to be running."))
  }

  if (!stop_docker(cache$container_name)) {
    stop(glue("An error occurred when trying to stop the {cache$container_name} Docker container."))
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
remove_cache <- function(cache) {
  # Make sure that the container actually exists
  if (!docker_container_exists(cache$container_name)) {
    stop(glue("A Docker container by the name of {cache$container_name} does not appear to exist."))
  }

  # Make sure that the Docker container is stopped
  tryCatch(
    {
      unload_cache(cache)
    },
    error = function(cond) {}
  )

  # Remove the Docker container
  remove_docker_container(cache$container_name)

  # Remove the cache .rds save file
  cache_save_path <- file.path(
    rappdirs::app_dir("neocache")$cache(),
    glue("{cache$container_name}.rds")
  )
  file.remove(cache_save_path)
}

cache_sitrep <- function() {
  cache_metadata_files <- list.files(
    path = rappdirs::app_dir("neocache")$cache(),
    pattern = "*.\\.rds"
  )

  if (length(cache_metadata_files) > 0) {
    message(glue("We found the following cache files: {cache_metadata_files}"))
  } else {
    message("No neocache cache objects found.")
  }
}
