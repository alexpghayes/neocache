#' #' Fetches the follows of all of the users in a provided vector. Returns
#' #' the follower relationships in the form of an edge list.
#' #'
#' #' @param user_ids A character vector of user ids (never screen names)
#' #' @param cache the cache to interface with
#' #' @param n the number of followers to retrieve for each user_id
#' #'
#' #' @return A tibble where each row corresponds to a follower relationship
#' #' from the user in the 'from' column to the user in to 'to' column
#' #'
#' #' @importFrom dplyr bind_rows
#' #' @export
#' nc_get_followers <- function(user_ids, cache_name, n = 150) {
#'   # here we will need to query twice: once to ask who we actually
#'   # have *complete* followship edges for, and then a second time to get
#'   # those followship edges
#'
#'   cache <- nc_activate_cache(cache_name)
#'
#'   user_ids <- c(user_ids)
#'   status <- follower_sampling_status(user_ids, cache)
#'
#'
#'   # sample the followers of all the users w/o sampled followers
#'   new_edges <- merge_then_fetch_connect_followers(status$not_in_graph, n, cache)
#'   upgraded_edges <- merge_then_fetch_connect_followers(status$sampled_followers_at_is_null, n, cache = cache)
#'   existing_edges <- db_get_followers(status$sampled_followers_at_not_null, cache = cache)
#'
#'   # need to be careful about duplicate edges here. ideally
#'   # we guarantee that edges are unique somehow before this, but if not
#'   # we can use dplyr::distinct(), although this is an expensive operation
#'
#'   bind_rows(new_edges, upgraded_edges, existing_edges)
#' }
#'
#'
#' #' user_ids is a list of COMPLETELY NEW users. This function performs the following:
#' #'   1. Fetch the followers of each user (call these main users) listed in user_ids (call these blank followers)
#' #'   2. MERGE nodes for main users and blank followers (each of these nodes will only contain a user_id field)
#' #'   3. Create edges between main users and their respective blank followers
#' #'   4. Set the sampled_followers_at property for nodes that were sampled
#' #'
#' #' @param user_ids a list of user_ids to add follower edges to the db for
#' #' @param n how many followers to sample at a time for each user
#' #' @param cache the cache to interface with
#' #'
#' #' @return a 2-column tibble edge list from user_ids to their followers
#' merge_then_fetch_connect_followers <- function(user_ids, n, cache) {
#'   # set sampled_followers_at to Sys.time()
#'   # sampled_at and sampled_followers_at default to NULL
#'   # return followers of each user
#'   user_ids <- c(user_ids)
#'
#'   if (length(user_ids) <= 1 && is.na(user_ids)) {
#'     return(empty_edge_list())
#'   }
#'
#'   ### 1.
#'   sample_time <- Sys.time()
#'   to_id <- user_ids
#'   edge_list <- rtweet::get_followers(to_id, n = n) %>%
#'     rename(from = .data$user_id) %>%
#'     mutate(to = to_id)
#'
#'   ### 2.
#'   db_add_new_users(c(user_ids, edge_list$from), cache)
#'
#'   ### 3.
#'   return_val <- docker_bulk_connect_nodes(edge_list, cache)
#'
#'   ### 4.
#'   update_qry <- glue(
#'     'WITH ["', glue_collapse(user_ids, sep = '","'), '"] AS user_ids UNWIND user_ids AS id ',
#'     'MATCH (n:User {{user_id:id}}) SET n.sampled_followers_at = "{sample_time}"'
#'   )
#'
#'   query_neo4j(update_qry, cache)
#'
#'   return_val
#' }
#'
#'
#' #' Fetches the followers of the provided user ID's via `rtweet`.
#' #'
#' #' @param user_ids vector of user_ids whose followers are being fetches
#' #' @param n the number of followers to fetch for each user_id
#' #'
#' #' @return an nx2 tibble edge list
#' #'
#' #' @importFrom dplyr rename bind_cols bind_rows
#' #' @importFrom rtweet get_followers
#' fetch_followers <- function(user_ids, n) {
#'   final <- empty_edge_list()
#'   for (to_id in user_ids) {
#'     final <- rtweet::get_followers(to_id, n = n) %>%
#'       rename(from = .data$user_id) %>%
#'       bind_cols(to = to_id) %>%
#'       bind_rows(final)
#'   }
#'   final
#' }
#'
#'
#' #' Checks whether follower data has already been sampled for the provided
#' #' vector of users.
#' #'
#' #' @param user_ids to fetch the sampling status for
#' #' @param cache the cache to interface with
#' #'
#' #' @return a list of all users who either (1) are not currently in the
#' #' graph, (2) are in the graph but their followers have not been sampled,
#' #' (3) are in the graph and have sampled followers
#' follower_sampling_status <- function(user_ids, cache) {
#'   # generate based on queries of user.sampled_followers_at node property
#'   present_users <- db_lookup_users(user_ids, cache)
#'   not_in_graph <- setdiff(user_ids, present_users$user_id)
#'   unsampled_users <- present_users %>%
#'     filter(is.na(.data$sampled_followers_at)) %>%
#'     pull(.data$user_id)
#'   sampled_users <- setdiff(user_ids, c(unsampled_users, not_in_graph))
#'
#'   if (length(not_in_graph) == 0) {
#'     not_in_graph <- NA
#'   }
#'
#'   if (length(unsampled_users) == 0) {
#'     unsampled_users <- NA
#'   }
#'
#'   if (length(sampled_users) == 0) {
#'     sampled_users <- NA
#'   }
#'
#'   list(
#'     not_in_graph = not_in_graph,
#'     sampled_followers_at_is_null = unsampled_users,
#'     sampled_followers_at_not_null = sampled_users
#'   )
#' }
