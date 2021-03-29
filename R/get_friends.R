#' Fetches the friends of each of the users contained in the user_ids vector.
#' Returns the friend relationships via a tibble edge list.
#'
#' @param user_ids A character vector of user ids (never screen names)
#'
#' @return A tibble where each row corresponds to a follower relationship
#' from the user in the 'from' column to the user in to 'to' column
#'
#' @export
get_friends <- function(user_ids, n = 150) {
  # here we will need to query twice: once to ask who we actually
  # have *complete* friendship edges for, and then a second time to get
  # those friendship edges
  user_ids <- c(user_ids)
  status <- friend_sampling_status(user_ids)


  # sample the friends of all the users w/o sampled friends
  new_edges <- merge_then_fetch_connect_friends(status$not_in_graph, n)
  upgraded_edges <- merge_then_fetch_connect_friends(status$sampled_friends_at_is_null, n)
  existing_edges <- db_get_friends(status$sampled_friends_at_not_null)

  # need to be careful about duplicate edges here. ideally
  # we guarantee that edges are unique somehow before this, but if not
  # we can use dplyr::distinct(), although this is an expensive operation

  bind_rows(new_edges, upgraded_edges, existing_edges)
}


#' user_ids is a list of COMPLETELY NEW users. This function performs the following:
#'   1. Fetch the friends of each user (call these main users) listed in user_ids (call these blank friends)
#'   2. MERGE nodes for main users and blank friends (each of these nodes will only contain a user_id field)
#'   3. Create edges between main users and their respective blank friends
#'   4. Set the sampled_friends_at property for nodes that were sampled
#'
#' @param user_ids a list of user_ids to add friend edges to the db for
#' @param n how many friends to sample at a time for each user
#'
#' @return a 2-column tibble edge list from user_ids to their friends
merge_then_fetch_connect_friends <- function(user_ids, n) {
  # set sampled_friends_at to Sys.time()
  # sampled_at and sampled_followers_at default to NULL
  # return friends of each user
  user_ids <- c(user_ids)

  if (length(user_ids) <= 1 && is.na(user_ids)) {
    return(empty_user_edges())
  }

  ### 1.
  sample_time <- Sys.time()
  edge_list <- fetch_friends(user_ids, n = n)

  ### 2.
  docker_bulk_merge_users(c(user_ids, edge_list$to))

  ### 3.
  return_val <- docker_bulk_connect_nodes(edge_list)

  ### 4.
  update_qry <- glue(
    'WITH ["', glue_collapse(user_ids, sep = '","'), '"] AS user_ids UNWIND user_ids AS id ',
    'MATCH (n:User {{user_id:id}}) SET n.sampled_friends_at = "{sample_time}"'
  )
  sup4j(update_qry)

  return_val
}


#' Uses rtweet to fetch the friend list of the users provided in user_ids
#'
#' @param user_ids a vector of user ids
#' @return an nx2 tibble edge list
#'
#' @importFrom rtweet get_friends
#' @importFrom dplyr rename
fetch_friends <- function(user_ids, n) {
  get_friends(user_ids, n = n) %>%
    rename(from = user, to = user_id)
}


#' Checks whether friend data has already been sampled for the provided
#' vector of users.
#'
#' @param user_ids to fetch the sampling status for
#'
#' @return a list of all users who either (1) are not currently in the
#' graph, (2) are in the graph but their friends have not been sampled,
#' (3) are in the graph and have sampled friends
friend_sampling_status <- function(user_ids) {
  # generate based on queries of user.sampled_friends_at node property
  present_users <- db_lookup_users(user_ids)
  not_in_graph <- setdiff(user_ids, present_users$user_id)
  unsampled_users <- present_users %>%
    filter(is.na(sampled_friends_at)) %>%
    pull(user_id)
  sampled_users <- setdiff(user_ids, c(unsampled_users, not_in_graph))

  if (length(not_in_graph) == 0) {
    not_in_graph <- NA
  }
  if (length(unsampled_users) == 0) {
    unsampled_users <- NA
  }
  if (length(sampled_users) == 0) {
    sampled_users <- NA
  }

  list(
    not_in_graph = not_in_graph,
    sampled_friends_at_is_null = unsampled_users,
    sampled_friends_at_not_null = sampled_users
  )
}
