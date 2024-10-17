#' @title Order the rows of a data frame by the values of selected columns
#' @description DataSHIELD implentation of \code{dplyr::arrange}.
#' @param df.name A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param tidy_expr Variables, or functions of variables. Use \code{desc()} to sort a variable in descending
#' order.
#' @param .by_group If TRUE, will sort first by grouping variable. Applies to grouped data frames
#' only.
#' @return An object of the same type as \code{df.name}, typically a data frame or tibble.
#' @export
arrangeDS <- function(tidy_expr, df.name, .by_group) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(df.name, tidy_expr)
  call <- .make_tidyverse_call(df.name, "arrange", tidy_expr, list(.by_group))
  out <- .execute_with_error_handling("arrange", call)
  return(out)
}
