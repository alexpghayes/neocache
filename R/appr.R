#' Create an abstract representation of the Twitter friendship graph
#'
#' Signifies that `aPPR` should query the Twitter friendship graph via
#' `neocache`.
#'
#' @param cache_name The name of the `neocache` to use to save data into.
#' @inheritParams rtweet::get_friends
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
#'
#' test_ids <- c("780429268866052096", "1191642560")
#'
#' graph <- neocache_graph()
#'
#' check(graph, test_ids)
#' node_degrees(graph, test_ids)
#' neighborhood(graph, test_ids[1])
#'
#'
#' }
#'
neocache_graph <- function(cache_name = "aPPR", retryonratelimit = TRUE) {

  if (!requireNamespace("aPPR", quietly = TRUE)) {
    stop(
      "`aPPR` package must be installed to use `neocache_graph()`",
      call. = FALSE
    )
  }

  if (!nc_cache_exists(cache_name)) {
    nc_create_cache(cache_name = cache_name, http_port = 28491, bolt_port = 28492)
  }

  nc_activate_cache(cache_name)

  agraph <- aPPR::abstract_graph(
    "neocache_graph",
    cache_name = cache_name,
    retryonratelimit = retryonratelimit
  )

  agraph
}

#' @inherit aPPR::appr title description params return references
#'
#' @examples
#'
#' library(aPPR)
#' library(neocache)
#'
#' set.seed(27)
#'
#' graph <- neocache_graph()
#'
#' \dontrun{
#' estimates <- appr(graph, "alexpghayes")
#' estimates
#' }
#'
#'
#' @seealso [aPPR::appr()]
#' @importFrom aPPR appr
#' @method appr neocache_graph
#'
#' @export
appr.neocache_graph <- function(graph, seeds, ..., alpha = 0.15,
                                epsilon = 1e-6, tau = NULL,
                                max_visits = Inf) {

  if (!requireNamespace("aPPR", quietly = TRUE)) {
    stop(
      "`aPPR` package must be installed to use `appr.neocache_graph()`",
      call. = FALSE
    )
  }

  seed_data <- rtweet::lookup_users(seeds, retryonratelimit = graph$retryonratelimit)

  # convert seeds, potentially passed as screen names, to user ids
  seeds <- seed_data$id_str

  # have to double call the API to get information safely into the cache
  nc_lookup_users(seeds, cache_name = graph$cache_name)

  if (any(seed_data$protected)) {
    stop("Seed nodes should not be protected Twitter accounts.", call. = FALSE)
  }

  NextMethod()
}

#' @export
#' @importFrom glue glue
#' @importFrom aPPR check
#' @method check neocache_graph
check.neocache_graph <- function(graph, nodes) {

  log_debug(glue("Checking nodes"))

  if (length(nodes) < 1)
    return(character(0))

  node_data <- nc_lookup_users(nodes, cache_name = graph$cache_name)

  if (is.null(node_data) || nrow(node_data) < 1)
    return(character(0))

  good_nodes <- !is.na(node_data$protected) & !node_data$protected & node_data$friends_count > 0

  log_debug(glue("Done checking nodes"))

  node_data$id_str[good_nodes]
}

#' @export
#' @importFrom aPPR node_degrees
#' @method node_degrees neocache_graph
node_degrees.neocache_graph <- function(graph, nodes) {

  log_debug(glue("Getting node degrees"))

  # assumes that you want any errors / empty rows when accessing this
  # data, i.e. that the nodes have already been checked

  log_trace(glue("Getting node degree(s): {nodes}"))

  node_data <- nc_lookup_users(nodes, cache_name = graph$cache_name)

  log_debug(glue("Done getting node degrees"))

  list(
    in_degree = node_data$followers_count,
    out_degree = node_data$friends_count
  )
}

#' @export
#' @importFrom aPPR neighborhood
#' @method neighborhood neocache_graph
neighborhood.neocache_graph <- function(graph, node) {

  log_debug(glue("Getting neighborhood: {node}"))

  # if a user doesn't follow anyone, safe_get_friends returns an empty
  # tibble, but instead it should return an empty character vector?
  friends <- nc_get_friends(node, cache_name = graph$cache_name)

  log_debug(glue("Done getting neighborhood"))

  if (nrow(friends) < 1) character(0) else friends$to_id
}
