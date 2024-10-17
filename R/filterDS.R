#' @title Performs dplyr filter
#' @description DataSHIELD implentation of \code{dplyr::filter}.
#' @param tidy_expr Diffused expression that return a logical value, and are defined in terms of the
#' variables in \code{df.name}.
#' @param df.name A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as
#' an alternative to \code{group_by}.
#' @param .preserve Relevant when the df.name input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is kept as is.
#' @return An object of the same type as \code{df.name}, typically a data frame or tibble.
#' @export
filterDS <- function(tidy_expr, df.name, .by, .preserve) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(df.name, tidy_expr)
  other_args <- .paste_character_args(.by, .preserve)
  call <- .make_tidyverse_call(df.name, "filter", tidy_expr, other_args)
  out <- .execute_with_error_handling("filter", call)
  .check_subset_disclosure_risk(eval(parse(text = df.name), envir = parent.frame()), out)
  return(out)
}
