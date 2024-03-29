% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_friends.R
\name{add_friend_edges_to_nodes_in_graph}
\alias{add_friend_edges_to_nodes_in_graph}
\title{users is a list of COMPLETELY NEW users. This function performs the following:
\enumerate{
\item Fetch the friends of each user (call these main users) listed in users (call these blank friends)
\item MERGE nodes for main users and blank friends (each of these nodes will only contain a id_str field)
\item Create edges between main users and their respective blank friends
\item Set the sampled_friends_at property for nodes that were sampled
}}
\usage{
add_friend_edges_to_nodes_in_graph(
  users,
  n,
  retryonratelimit,
  cursor,
  verbose,
  token,
  cache
)
}
\arguments{
\item{users}{Screen name or user ID of target user from which the
user IDs of friends (accounts followed BY target user) will be
retrieved.}

\item{n}{Desired number of results to return. Results are downloaded
in pages when \code{n} is large; the default value will download a single
page. Set \code{n = Inf} to download as many results as possible.

The Twitter API rate limits the number of requests you can perform
in each 15 minute period. The easiest way to download more than that is
to use \code{retryonratelimit = TRUE}.

You are not guaranteed to get exactly \code{n} results back. You will get
fewer results when tweets have been deleted or if you hit a rate limit.
You will get more results if you ask for a number of tweets that's not
a multiple of page size, e.g. if you request \code{n = 150} and the page
size is 200, you'll get 200 results back.}

\item{retryonratelimit}{If \code{TRUE}, and a rate limit is exhausted, will wait
until it refreshes. Most Twitter rate limits refresh every 15 minutes.
If \code{FALSE}, and the rate limit is exceeded, the function will terminate
early with a warning; you'll still get back all results received up to
that point. The default value, \code{NULL}, consults the option
\code{rtweet.retryonratelimit} so that you can globally set it to \code{TRUE},
if desired.

If you expect a query to take hours or days to perform, you should not
rely soley on \code{retryonratelimit} because it does not handle other common
failure modes like temporarily losing your internet connection.}

\item{cursor}{Which page of results to return. The default will return
the first page; you can supply the result from a previous call to
continue pagination from where it left off.}

\item{verbose}{Show progress bars and other messages indicating current
progress?}

\item{token}{Expert use only. Use this to override authentication for
a single API call. In most cases you are better off changing the
default for all calls. See \code{\link[rtweet:auth_as]{auth_as()}} for details.}

\item{cache}{The cache to store information in. Must be a cache, not a
cache name.}
}
\value{
a 2-column tibble edge list from users to their friends
}
\description{
users is a list of COMPLETELY NEW users. This function performs the following:
\enumerate{
\item Fetch the friends of each user (call these main users) listed in users (call these blank friends)
\item MERGE nodes for main users and blank friends (each of these nodes will only contain a id_str field)
\item Create edges between main users and their respective blank friends
\item Set the sampled_friends_at property for nodes that were sampled
}
}
