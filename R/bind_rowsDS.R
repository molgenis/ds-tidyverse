#' @title Bind multiple data frames by row.
#' @description DataSHIELD implementation of \code{dplyr::bind_rows}.
#' @param to_combine 	Data frames to combine. Each argument can either be a data frame, a list that
#' could be a data frame, or a list of data frames. Columns are matched by name, and any missing
#' columns will be filled with NA.
#' @param .id he name of an optional identifier column. Provide a string to create an output column
#' that identifies each input. The column will use names if available, otherwise it will use
#' positions.
#' @return A serverside data frame with name specified in \code{newobj} and the same type as the
#' first element of `to_combine`.
#' @export
bindRowsDS <- function(to_combine = NULL, .id = NULL) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  to_combine <- .decode_tidy_eval(to_combine, .get_encode_dictionary())
  .check_tidy_disclosure(NULL, to_combine, check_df = F)
  other_args <- .paste_character_args(.id)
  call <- .make_tidyverse_call(.data = NULL, "bind_rows", to_combine, other_args, inc_data = F)
  out <- .execute_with_error_handling("bind_rows", call)
  return(out)
}
