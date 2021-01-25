#' @param tbl tibble containing columns for 'to' and 'from'
#' TODO: add docs
docker_bulk_connect_friends <- function(tbl) {
  # Create the root user's node if it does not already exist
  root_qry <- glue('MERGE (n:User {{user_id:"{tbl$from[1]}"}})')
  sup4j(root_qry)
  print("Root done.")

  # Create temp file to write the data into
  tmp <- tempfile()

  # Write the data; we use cat instead of write to eliminate any trailing newline
  cat(paste0('to\n', glue_collapse(tbl$to, sep="\n")), file = tmp)

  # Copy the data into the docker container
  system(glue("docker cp {tmp} neocache_docker:/var/lib/neo4j/import/data.csv"))

  # Add a node for each of the root user's friends and connect the root user to them
  ## This query runs on joe50k in ~2.75 minutes
  connect_qry <- glue("LOAD CSV WITH HEADERS FROM 'file:///data.csv' AS row ",
                     "MATCH (to:User {{user_id:'{tbl$from[1]}'}}) CREATE (from:User {{user_id:row.to}}) CREATE (from)-[:FOLLOWS]->(to)")

  print(sup4j(connect_qry))
  print("Connections done.")

  file.remove(tmp)
}


#' Gets the friends for the given user_id and creates the edges in the graph.
#' This function is only ever called by add_new_friends.
#'
#' @param user_ids user_ids who are already in the database and
#' do not have friend edge data
#'
#' @return a nx2 tibble where the <from> column is user_id and the <to> column
#' is the user_id of user_id's friends
db_connect_friends <- function(user_ids, n) {

  con <- get_connexion()

  friends <- empty_user_edges()
  for(user_id in user_ids) {
    friends <- friends %>%
      bind_rows(from=user_id, to=rtweet::get_friends(user_id, n = n))
  }
  results <- docker_bulk_connect(friends)


  # OLD, SLOW IMPLEMENTATION
  #
  # to_ret <- empty_user_edges()
  # for (user_id in user_ids) {
  #   friends <- rtweet::get_friends(user_id, n = n)
  #   sup4j(
  #     glue('MATCH (n:User {{user_id:"{user_id}"}}) SET n.sampled_friends_at="{Sys.time()}"'),
  #     con
  #   )
  #
  #   results <- NULL
  #   for (user in friends$user_id) {
  #     # TODO: Improve this CYPHER query, there should be a way to create all of the edges at once
  #     temp <- sup4j(
  #       glue('MERGE (from:User {{user_id:"{user_id}"}}) MERGE (to:User {{user_id:"{user}"}}) ',
  #             'MERGE (from)-[r:FOLLOWS]->(to)'
  #       ),
  #       con
  #     )
  #     results <- results %>%
  #       bind_rows(tibble(from = user_id, to = user))
  #   }
  #
  #   if (length(results) == 2) {
  #     # If length(results) == 2 then the user existed and everything worked properly
  #     to_ret <- to_ret %>%
  #       bind_rows(tibble(from = user_id, to = friends$user_id))
  #   }
  # }
  #
  # to_ret
}



#' Gets the friends for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have friend edge data
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their friends
db_get_friends <- function(user_ids) {

  con <- get_connexion()

  results <- sup4j(
    glue('MATCH (from:User),(to:User) WHERE from.user_id in ["',
         glue_collapse(user_ids, sep = '","'),
         '"] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id',
    ),
    con
  )

  if (length(results) != 2) {
    return(empty_user_edges())
  }

  tibble(from = results$from.user_id$value, to = results$to.user_id$value)
}



#' @param user_ids A character vector of user ids (never screen names)
#'
#' @return A tibble where each row corresponds to a follower relationship
#' from the user in the 'from' column to the user in to 'to' column
get_friends <- function(user_ids, n = 150) {
  # here we will need to query twice: once to ask who we actually
  # have *complete* friendship edges for, and then a second time to get
  # those friendship edges
  user_ids <- c(user_ids)
  status <- friend_sampling_status(user_ids)


  # sample the friends of all the users w/o sampled friends
  new_edges <- add_new_friends(status$not_in_graph, n)
  upgraded_edges <- add_new_friends(status$sampled_friends_at_is_null, n)
  existing_edges <- db_get_friends(status$sampled_friends_at_not_null)

  # need to be careful about duplicate edges here. ideally
  # we guarantee that edges are unique somehow before this, but if not
  # we can use dplyr::distinct(), although this is an expensive operation

  # TODO: I believe that these edges should all be duplicate free, but this
  #       needs to be verified
  bind_rows(new_edges, upgraded_edges, existing_edges)
}


#' @param user_ids a list of user_ids to add friend edges to the db for
#' @param n how many friends to sample at a time for each user
#'
#' @return a 2-column tibble edge list from user_ids to their friends
add_new_friends <- function(user_ids, n) {
  # set sampled_friends_at to Sys.time()
  # sampled_at and sampled_followers_at default to NULL
  # return friends of each user
  user_ids <- c(user_ids)

  if (length(user_ids) <= 1 && is.na(user_ids)) {
    return(empty_user_edges())
  }

  # Add the users to the graph, then give them edge data
  merge_users(user_ids, lookup = FALSE)
  db_connect_friends(user_ids, n = n)
}


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



#' Tests how quickly the current implementation can add 50,000 nodes to the graphs
#' and connect each of these nodes to one other root node.
speedtest <- function() {
  if(!exists("joe")) {
    joe <- rtweet::get_followers("joebiden", n="all")$user_id
  }

  start_neo4j()
  clear____db()
  con <- get_connexion()

  # Old, much slower method that strictly uses neo4r:
  #
  # tictoc::tic()
  # results <- NULL
  # # Current implementation, from lines 23-30
  # for (user in joe[1:250]) {
  #   # TODO: Improve this CYPHER query, there should be a way to create all of the edges at once
  #   temp <- sup4j(
  #     glue('MERGE (from:User {{user_id:"{user_id}"}}) MERGE (to:User {{user_id:"{user}"}}) ',
  #          'MERGE (from)-[r:FOLLOWS]->(to)'
  #     ),
  #     con
  #   )
  #   results <- results %>%
  #     bind_rows(tibble(from = user_id, to = user))
  # }
  # tictoc::toc() # takes ~15 seconds to add 250 nodes to the Neo4J graph


  tictoc::tic()

  tbl <- bind_cols(joe, from='0') %>% rename(to=user_id)
  docker_bulk_connect_friends(tbl)

  tictoc::toc() # takes 1-2 seconds to do 75,000 entries with Docker

  cat(glue("Total nodes in graph: {sup4j('MATCH (n) RETURN COUNT(n)')[[1]]$value[1]}\n"))
  cat(glue("Total edges in graph: {sup4j('MATCH ()-[r]->() RETURN COUNT(r)')[[1]]$value[1]}\n"))
}















