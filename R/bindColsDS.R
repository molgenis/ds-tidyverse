#' @title Bind multiple data frames by column
#' @description DataSHIELD implementation of \code{dplyr::bind_cols}.
#' @param to_combine 	Data frames to combine. Each argument can either be a data frame, a list that
#' could be a data frame, or a list of data frames. Columns are matched by name, and any missing
#' columns will be filled with NA.
#' @param .name_repair One of "unique", "universal", or "check_unique". See
#' \code{vctrs::vec_as_names()} for the meaning of these options.
#' @return A serverside data frame with name specified in \code{newobj} and the same type as the
#' first element of `to_combine`.
#' @export
bindColsDS <- function(to_combine = NULL, .name_repair = NULL) {
  to_combine <- .decode_tidy_eval(to_combine, .get_encode_dictionary())
  other_args <- .paste_character_args(.name_repair)
  call <- .make_tidyverse_call(.data = NULL, "bind_cols", to_combine, other_args, inc_data = F)
  out <- .execute_with_error_handling("bind_cols", call)
  return(out)
}
