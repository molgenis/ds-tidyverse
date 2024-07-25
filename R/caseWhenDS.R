#' @title Performs dplyr if_else
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param dynamic_dots A sequence of two-sided formulas. The left hand side (LHS) determines which
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
#' @return the object specified by the \code{newobj} argument of \code{ds.case_when}which is
#' written to the serverside.
#' @export
#'
caseWhenDS <- function(dynamic_dots = NULL, .default = NULL, .ptype = NULL, .size = NULL) {
  dynamic_dots <- .decode_tidy_eval(dynamic_dots, .get_encode_dictionary())
  other_args <- .paste_character_args(.default, .ptype, .size)
  call <- .make_tidyverse_call(.data = NULL, "case_when", dynamic_dots, other_args, inc_data = F)
  out <- .execute_with_error_handling("case_when", call)
  return(out)
}
