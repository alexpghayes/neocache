twitter_properties <- c(
  "id_str", "name", "screen_name", "location", "description",
  "url", "protected", "followers_count", "friends_count", "listed_count",
  "created_at", "favourites_count", "verified", "statuses_count",
  "profile_image_url_https", "profile_banner_url", "default_profile",
  "default_profile_image"
)

all_properties <- c(
  twitter_properties,
  "sampled_at",
  "sampled_friends_at",
  "sampled_followers_at"
)

chr_properties <- c(
  "id_str", "name", "screen_name", "location", "description",
  "url", "profile_image_url_https", "profile_banner_url"
)

lgl_properties <- c(
  "protected", "verified", "default_profile", "default_profile_image"
)

int_properties <- c(
  "followers_count", "friends_count", "listed_count",
  "favourites_count", "statuses_count"
)

dttm_properties <- c(
  "created_at",
  "sampled_at",
  "sampled_friends_at",
  "sampled_followers_at"
)

as_user_data <- function(data) {

  log_trace(glue("data is {nrow(data)} x {ncol(data)} with type signature"))
  log_trace(type_signature(data))

  typecast <- data %>%
    mutate_at(vars(any_of(chr_properties)), as.character) %>%
    mutate_at(vars(any_of(lgl_properties)), as.logical) %>%
    mutate_at(vars(any_of(int_properties)), as.integer) %>%
    mutate_at(vars(any_of(dttm_properties)), as.POSIXct)

  log_trace(glue("typecast is {nrow(typecast)} x {ncol(typecast)} with type signature"))
  log_trace(type_signature(typecast))

  typecast
}

as_edge_list <- function(edges) {

  log_trace(glue("edges is {nrow(edges)} x {ncol(edges)} with type signature"))
  log_trace(type_signature(edges))

  typed_edges <- edges %>%
    mutate_at(vars(from_id, to_id), as.character) %>%
    select(from_id, to_id)

  log_trace(glue("typed_edges is {nrow(typed_edges)} x {ncol(typed_edges)} with type signature"))
  log_trace(type_signature(typed_edges))

  typed_edges
}

#' Create an empty table of user data
#'
#' @return an empty tibble with columns named after all of the lookup_user
#' properties.  Used when user data is unavailable
#'
#' @keywords internal
#'
empty_user <- function() {
  tibble(
    # everything from rtweet::lookup_users except withheld_in_countries<list>
    # and entities<list>
    id_str = character(0),
    name = character(0),
    screen_name = character(0),
    location = character(0),
    description = character(0),
    url = character(0),
    protected = logical(0),
    followers_count = integer(0),
    friends_count = integer(0),
    listed_count = integer(0),
    created_at = lubridate::POSIXct(0),
    favourites_count = integer(0),
    verified = logical(0),
    statuses_count = integer(0),
    profile_image_url_https = character(0),
    profile_banner_url = character(0),
    default_profile = logical(0),
    default_profile_image = logical(0),

    # additional fields for internal neocache use
    sampled_at = lubridate::POSIXct(0),
    sampled_friends_at = lubridate::POSIXct(0),
    sampled_followers_at = lubridate::POSIXct(0)
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
    from_id = character(0),
    to_id = character(0)
  )
}
