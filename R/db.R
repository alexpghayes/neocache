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
    password = "password"
  )

  if (!(con$ping() == 200)) {
    warning("Database connection not established.")
  }

  con
}

#' TODO
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
  "MATCH (n) DETACH DELETE n" %>%
    sup4j(con)
}
