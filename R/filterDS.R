#' @title Performs dplyr filter
#' @description This function is similar to R function \code{filter}.
#' @details Performs dplyr filter
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param expression Diffused expression of dotdotdot passed to ds.filter
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is kept as is.
#' @return the object specified by the \code{newobj} argument of \code{ds.filter} which is written
#' to the serverside.
#' @export
filterDS <- function(expr, .data, .keep = NULL, .before = NULL, .after = NULL) {
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  other_args <- .paste_character_args(.keep, .before, .after)
  call <- .make_tidyverse_call(.data, "mutate", tidy_select, other_args)
  out <- .execute_with_error_handling("mutate", call)
  return(out)
}
