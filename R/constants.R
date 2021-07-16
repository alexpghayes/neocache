properties <- c(
  "screen_name", "protected", "followers_count", "friends_count",
  "listed_count", "statuses_count", "favourites_count", "account_created_at", "verified", "profile_url",
  "profile_expanded_url", "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url",
  "name", "location", "description", "url"
)


#' Create an empty table of user data
#'
#' @return an empty tibble with columns named after all of the lookup_user
#' properties.  Used when user data is unavailable
#'
#' @keywords internal
#'
empty_user <- function() {
  tibble(
    user_id = character(0),
    screen_name = character(0),
    protected = logical(0),
    followers_count = numeric(0),
    friends_count = numeric(0),
    listed_count = numeric(0),
    statuses_count = numeric(0),
    favourites_count = numeric(0),
    account_created_at = character(0),
    verified = logical(0),
    profile_url = character(0),
    profile_expanded_url = character(0),
    account_lang = logical(0),
    profile_banner_url = character(0),
    profile_background_url = character(0),
    profile_image_url = character(0),
    name = character(0),
    location = character(0),
    description = character(0),
    url = character(0),
    sampled_at = character(0),
    sampled_friends_at = character(0),
    sampled_followers_at = character(0)
  )
}


#' Create an empty table of following information
#'
#' @return an empty 2-column tibble used as a placeholder for
#' when user data is not available
#'
#' @keywords internal
empty_edge_list <- function() {
  tibble(
    from = character(0),
    to = character(0)
  )
}
