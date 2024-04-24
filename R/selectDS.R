#'
#' @title Performs dplyr select
#' @description This function is similar to R function \code{select}.
#' @details Performs dplyr select
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param expr Diffused expression of dotdotdot passed to ds.select
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.select} (or as default same name as input object) which is written to the serverside.
#' @importFrom tidyselect eval_select
#' @importFrom rlang set_names
#' @importFrom dplyr
#' @export
#'
selectDS <- function(.data, expr) {
  .check_data(.data)
  tidy_select_args <- .decode_tidy_eval(expr, .getEncodeKey())
  out <- .execute_tidyverse_function(.data, "select", tidy_select_args)
  return(out)
}
