#' @title Performs dplyr filter
#' @description This function is similar to R function \code{filter}.
#' @details Performs dplyr filter
#' @param expr Diffused expression of dotdotdot passed to ds.filter
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .add When FALSE, the default, \code{group_by()} will override existing groups. To add to
#' the existing groups, use .add = TRUE.
#' @param .drop Drop groups formed by factor levels that don't appear in the data? The default is
#' TRUE except when .data has been previously grouped with .drop = FALSE.
#' @return the object specified by the \code{newobj} argument of \code{ds.group_by} which is written
#' to the serverside.
#' @export
groupByDS <- function(expr, .data, .add, .drop) {
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  other_args <- .paste_character_args(.add, .drop)
  call <- .make_tidyverse_call(.data, "group_by", tidy_select, other_args)
  out <- .execute_with_error_handling("group_by", call)
  return(out)
}

#' @title Performs dplyr ungroup
#' @description This function is similar to R function \code{ungroup}.
#' @param x A tibble.
#' @export
ungroupDS <- function(x) {
  call <- .make_tidyverse_call(x, "ungroup", tidy_select = NULL, other_args = NULL)
  out <- .execute_with_error_handling("ungroup", call)
  return(out)
}
