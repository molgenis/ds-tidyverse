#' @title Subset rows using their positions
#' @description DataSHIELD implentation of \code{dplyr::slice}.
#' @param tidy_expr Provide either positive values to keep, or negative values to drop. The values
#' provided must be either all positive or all negative. Indices beyond the number of rows in the
#' input are silently ignored.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as
#' an alternative to \code{group_by}.
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is
#' kept as is.
#' @return the object specified by the \code{newobj} argument of \code{ds.slice} which is written
#' to the serverside.
#' @export
sliceDS <- function(tidy_expr, .data, .by, .preserve) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(.data, tidy_expr)
  other_args <- .paste_character_args(.by, .preserve)
  call <- .make_tidyverse_call(.data, "slice", tidy_expr, other_args)
  out <- .execute_with_error_handling("slice", call)
  .check_subset_disclosure_risk(eval(parse(text = .data), envir = parent.frame()), out)
  return(out)
}
