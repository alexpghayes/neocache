#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

utils::globalVariables(
  c(
    "created_at",
    "edge_list",
    "from_id",
    "id_str",
    "sampled_at",
    "screen_name",
    "to_id",
    "user_info_raw"
  )
)
