#' @title Performs dplyr rename
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param expr Diffused expression of dotdotdot passed to ds.rename
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.rename} (or as default same name as input object) which is written to the serverside.
#' @export
#'
renameDS <- function(expr, .data) {
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  call <- .make_tidyverse_call(.data, "rename", tidy_select)
  out <- .execute_with_error_handling("rename", call)
  return(out)
}
