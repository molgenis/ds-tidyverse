#' @title Performs dplyr \code{group_keys}.
#' @param x a grouped tibble.
#' @return A tibble describing the groups is returned to the client.
#' @export
groupKeysDS <- function(x) {
  call <- .make_tidyverse_call(x, "group_keys", tidy_select = NULL, other_args = NULL)
  out <- .execute_with_error_handling("group_keys", call)
  return(out)
}
