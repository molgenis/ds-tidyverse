#' @title Performs dplyr filter
#' @description This function is similar to R function \code{filter}.
#' @details Performs dplyr filter
#' @param tidy_expr Diffused expression of dotdotdot passed to ds.filter
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as
#' an alternative to \code{group_by}.
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is kept as is.
#' @return the object specified by the \code{newobj} argument of \code{ds.filter} which is written
#' to the serverside.
#' @export
filterDS <- function(tidy_expr, .data, .by, .preserve) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(.data, tidy_expr)
  other_args <- .paste_character_args(.by, .preserve)
  call <- .make_tidyverse_call(.data, "filter", tidy_expr, other_args)
  out <- .execute_with_error_handling("filter", call)
  .check_subset_disclosure_risk(eval(parse(text = .data), envir = parent.frame()), out)
  return(out)
}
