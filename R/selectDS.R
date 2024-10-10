#' @title Keep or drop columns using their names and types
#' @description DataSHIELD implentation of \code{dplyr::select}.
#' @details Performs dplyr select
#' @param df.name A data frame or tibble.
#' @param tidy_expr One or more unquoted expressions separated by commas.
#' @return the object specified by the \code{newobj} argument of \code{ds.select} which is written
#' to the serverside.
#' @export
selectDS <- function(tidy_expr, df.name) {
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(df.name, tidy_expr)
  call <- .make_tidyverse_call(df.name, "select", tidy_expr)
  out <- .execute_with_error_handling("select", call)
  return(out)
}
