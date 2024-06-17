#' @title Performs dplyr mutate
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param expr Diffused expression of dotdotdot passed to ds.rename
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.rename} (or as default same name as input object) which is written to the serverside.
#' @export
#'
mutateDS <- function(.data, expr, .keep, .before, .after) {
  tidy_select_args <- .decode_tidy_eval(expr, .get_encode_dictionary())
  extra_args <- .paste_character_args(.keep, .before, .after)
  out <- .execute_tidyverse_function(.data, "mutate", tidy_select_args, extra_args)
  return(out)
}


