#' @title Bind multiple data frames by column
#' @description DataSHIELD implementation of \code{dplyr::bind_cols}.
#' @param to_combine 	Data frames to combine. Each argument can either be a data frame, a list that
#' could be a data frame, or a list of data frames. Columns are matched by name, and any missing
#' columns will be filled with NA.
#' @param .name_repair One of "unique", "universal", or "check_unique". See
#' \code{vctrs::vec_as_names()} for the meaning of these options.
#' @return A data frame the same type as the first element of \code{to_combine}
#' @export
bindColsDS <- function(to_combine = NULL, .name_repair = NULL) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana'))
  to_combine <- .decode_tidy_eval(to_combine, .get_encode_dictionary())
  .check_tidy_disclosure(NULL, to_combine, check_df = F)
  other_args <- .paste_character_args(.name_repair)
  call <- .make_tidyverse_call(.data = NULL, "bind_cols", to_combine, other_args, inc_data = F)
  out <- .execute_with_error_handling("bind_cols", call)
  return(out)
}
