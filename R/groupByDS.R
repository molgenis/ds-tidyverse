#' @title Performs dplyr group_by
#' @description This function is similar to R function \code{dplyr::group_by}.
#' @details Performs dplyr group_by
#' @param expr Diffused grouping expression passed to \code{ds.group_by}
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
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  .check_tidy_disclosure(NULL, tidy_select, check_df = F)
  other_args <- .paste_character_args(.add, .drop)
  call <- .make_tidyverse_call(.data, "group_by", tidy_select, other_args)
  out <- .execute_with_error_handling("group_by", call)
  return(out)
}

#' @title Performs dplyr ungroup
#' @description This function is similar to R function \code{dplyr::ungroup}.
#' @param tidy_select Unused in this function.
#' @param x A tibble.
#' @return the object specified by the \code{newobj} argument of \code{ds.ungroup} which is written
#' to the serverside.
#' @export
ungroupDS <- function(tidy_select, x) {
  .check_data_name_length(x, listDisclosureSettingsDS())
  call <- .make_tidyverse_call(x, "ungroup", tidy_select = NULL, other_args = NULL)
  out <- .execute_with_error_handling("ungroup", call)
  return(out)
}
