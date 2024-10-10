#' @title Performs dplyr if_else
#' @description This function is similar to R function \code{if_else}.
#' @param condition A list, specifying a logical vector in `tidy-select` syntax, ie data and column names unquoted.
#' @param true Vector to use for TRUE value of condition.
#' @param false Vector to use for FALSE value of condition.
#' @param missing If not NULL, will be used as the value for NA values of condition. Follows the same size and type rules as true and false.
#' @param ptype An optional prototype declaring the desired output type. If supplied, this overrides the common type of true, false, and missing.
#' @param size An optional size declaring the desired output size. If supplied, this overrides the size of condition.
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.if_else} which is written to the serverside.
#' @export
ifElseDS <- function(condition = NULL, true = NULL, false = NULL, missing = NULL,
                     ptype = NULL, size = NULL) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana', 'avacado'))
  tidyselect <- .decode_tidy_eval(condition, .get_encode_dictionary())
  .check_tidy_disclosure(NULL, tidyselect, check_df = F)
  other_args <- .paste_character_args(true, false, missing, ptype, size)
  call <- .make_tidyverse_call(.data = NULL, "if_else", tidyselect, other_args, inc_data = F)
  out <- .execute_with_error_handling("if_else", call)
  return(out)
}
