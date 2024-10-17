#' @title Subset rows using their positions
#' @description DataSHIELD implentation of \code{dplyr::slice}.
#' @param tidy_expr Provide either positive values to keep, or negative values to drop. The values
#' provided must be either all positive or all negative. Indices beyond the number of rows in the
#' input are silently ignored.
#' @param df.name A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as
#' an alternative to \code{group_by}.
#' @param .preserve Relevant when the df.name input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is
#' kept as is.
#' @return An object of the same type as \code{df.name}, typically a data frame or tibble.
#' @export
sliceDS <- function(tidy_expr, df.name, .by, .preserve) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(df.name, tidy_expr)
  other_args <- .paste_character_args(.by, .preserve)
  call <- .make_tidyverse_call(df.name, "slice", tidy_expr, other_args)
  out <- .execute_with_error_handling("slice", call)
  .check_subset_disclosure_risk(eval(parse(text = df.name), envir = parent.frame()), out)
  return(out)
}
