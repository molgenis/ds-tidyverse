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
selectDS <- function(.data, expr){
  ds_data <- eval(parse(text=.data), envir = parent.frame())
  out <- .decode_tidy_eval(expr, .getEncodeKey())
  string_to_eval <- paste0(.data, " %>% dplyr::select(", out, ")")
  string_as_expr <- rlang::parse_expr( string_to_eval )
  out <- eval_tidy(string_as_expr)
  return(out)
}
