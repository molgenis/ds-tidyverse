#' @title Performs dplyr case_when
#' @description DataSHIELD implentation of \code{dplyr::case_when}.
#' @param tidy_expr A sequence of two-sided formulas. The left hand side (LHS) determines which
#' values match this case. The right hand side (RHS) provides the replacement value.
#' The LHS inputs must evaluate to logical vectors.
#' The RHS inputs will be coerced to their common type.
#' All inputs will be recycled to their common size. That said, we encourage all LHS inputs to be
#' the same size. Recycling is mainly useful for RHS inputs, where you might supply a size 1 input
#' that will be recycled to the size of the LHS inputs.
#' NULL inputs are ignored.
#' @param .default The value used when all of the LHS inputs return either FALSE or NA.
#' @param .ptype An optional prototype declaring the desired output type. If supplied, this overrides the common type of true, false, and missing.
#' @param .size An optional size declaring the desired output size. If supplied, this overrides the size of condition.
#' @return A vector with the same size as the common size computed from the inputs in \code{tidy_expr} and the same type as the common type of the RHS inputs in \code{tidy_expr}.
#' @export
caseWhenDS <- function(tidy_expr = NULL, .default = NULL, .ptype = NULL, .size = NULL) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana', 'avacado'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(NULL, tidy_expr, check_df = F)
  other_args <- .paste_character_args(.default, .ptype, .size)
  call <- .make_tidyverse_call(.data = NULL, "case_when", tidy_expr, other_args, inc_data = F)
  out <- .execute_with_error_handling("case_when", call)
  return(out)
}
