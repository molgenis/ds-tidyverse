#'
#' @title Performs dplyr select
#' @description This function is similar to R function \code{select}.
#' @details Performs dplyr select
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param expr Diffused expression of dotdotdot passed to ds.select
#' @return the object specified by the \code{newobj} argument of \code{ds.select} which is written
#' to the serverside.
#' @export
#'
selectDS <- function(expr, .data) {
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  .check_tidy_disclosure(.data, tidy_select)
  call <- .make_tidyverse_call(.data, "select", tidy_select)
  out <- .execute_with_error_handling("select", call)
  return(out)
}
