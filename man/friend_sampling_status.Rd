% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_friends.R
\name{friend_sampling_status}
\alias{friend_sampling_status}
\title{Checks whether friend data has already been sampled for the provided
vector of users.}
\usage{
friend_sampling_status(user_ids, cache)
}
\arguments{
\item{user_ids}{to fetch the sampling status for}

\item{cache}{the cache to interface with}
}
\value{
a list of all users who either (1) are not currently in the
graph, (2) are in the graph but their friends have not been sampled,
(3) are in the graph and have sampled friends
}
\description{
Checks whether friend data has already been sampled for the provided
vector of users.
}
