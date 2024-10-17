#' @title Rename columns
#' @description DataSHIELD implentation of\code{dplyr::rename}.
#' @param df.name A data frame or tibble.
#' @param tidy_expr list containing diffused expression.
#' @return An object of the same type as \code{df.name}, typically a data frame or tibble.
#' @export
renameDS <- function(tidy_expr, df.name) {
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(df.name, tidy_expr)
  call <- .make_tidyverse_call(df.name, "rename", tidy_expr)
  out <- .execute_with_error_handling("rename", call)
  return(out)
}
