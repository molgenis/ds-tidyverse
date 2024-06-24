#'
#' @title Performs dplyr select
#' @description This function is similar to R function \code{select}.
#' @details Performs dplyr select
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param expr Diffused expression of dotdotdot passed to ds.select
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.select} (or as default same name as input object) which is written to the serverside.
#' @export
#'
selectDS <- function(.data, expr) {
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  call <- .make_tidyverse_call(.data, "select", tidy_select)
  out <- .execute_with_error_handling("select", call)
  return(out)
}
