#' @title Group by one or more variables
#' @description DataSHIELD implentation of \code{dplyr::group_by}.
#' @param tidy_expr Diffused grouping expression.
#' @param df.name A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .add When FALSE, the default, \code{group_by()} will override existing groups. To add to
#' the existing groups, use .add = TRUE.
#' @param .drop Drop groups formed by factor levels that don't appear in the data? The default is
#' TRUE except when df.name has been previously grouped with .drop = FALSE.
#' @return the object specified by the \code{newobj} argument of \code{ds.group_by} which is written
#' to the serverside.
#' @export
groupByDS <- function(tidy_expr, df.name, .add, .drop) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(NULL, tidy_expr, check_df = F)
  other_args <- .paste_character_args(.add, .drop)
  call <- .make_tidyverse_call(df.name, "group_by", tidy_expr, other_args)
  out <- .execute_with_error_handling("group_by", call)
  return(out)
}

#' @title Remove grouping from a tibble or data frame
#' @description DataSHIELD implentation of \code{dplyr::ungroup}.
#' @param tidy_expr Unused in this function.
#' @param x A tibble.
#' @return the object specified by the \code{newobj} argument of \code{ds.ungroup} which is written
#' to the serverside.
#' @export
ungroupDS <- function(tidy_expr, x) {
  .check_data_name_length(x, listDisclosureSettingsDS())
  call <- .make_tidyverse_call(x, "ungroup", tidy_expr = NULL, other_args = NULL)
  out <- .execute_with_error_handling("ungroup", call)
  return(out)
}
