#' @title Performs dplyr rename
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param tidy_expr Diffused expression of dotdotdot passed to ds.rename
#' @return the object specified by the \code{newobj} argument of \code{ds.rename} which is written
#' to the serverside.
#' @export
#'
renameDS <- function(tidy_expr, .data) {
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(.data, tidy_expr)
  call <- .make_tidyverse_call(.data, "rename", tidy_expr)
  out <- .execute_with_error_handling("rename", call)
  return(out)
}
