% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neo4j.R
\name{db_get_followers}
\alias{db_get_followers}
\title{Gets the followers for the given user that already exist in the DB.}
\usage{
db_get_followers(users, cache)
}
\arguments{
\item{users}{a list of users who are already in the DB and
already have follower edge data}

\item{cache}{the cache to interface with}
}
\value{
a 2-column tibble edge list with entries from the users in users
to their followers
}
\description{
Gets the followers for the given user that already exist in the DB.
}
