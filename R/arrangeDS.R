#' @title Order the rows of a data frame by the values of selected columns
#' @description DataSHIELD implentation of  \code{dplyr::arrange}.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param expr Variables, or functions of variables. Use desc() to sort a variable in descending
#' order.
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames
#' only.
#' @return the object specified by the \code{newobj} argument of \code{ds.arrange} which is written
#' to the serverside.
#' @export
arrangeDS <- function(expr, .data, .by_group) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  .check_tidy_disclosure(.data, tidy_select)
  call <- .make_tidyverse_call(.data, "arrange", tidy_select, list(.by_group))
  out <- .execute_with_error_handling("arrange", call)
  return(out)
}
