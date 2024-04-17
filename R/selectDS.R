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
  expr_replaced <- str_replace_all(expr, fixed("$LB$"), "(")
  expr_replaced <- str_replace_all(expr_replaced, fixed("$RB$"), ")")
  expr_replaced <- str_replace_all(expr_replaced, fixed("$QUOTE$"), "'")
  expr_replaced <- str_replace_all(expr_replaced, fixed("$COMMA$"), ",")
  expr_replaced <- str_replace_all(expr_replaced, fixed("$SPACE$"), " ")
  expr_replaced <- str_replace_all(expr_replaced, fixed("c("), "") # Better regex
  expr_replaced <- str_sub(expr_replaced, end = -2)

  expr_split <- str_split(expr_replaced, fixed(","))

  out <- expr_split[[1]] %>%
    map(~dplyr::select(ds_data, eval(str2expression(.x)))) %>%
    bind_cols() ## Here we loop over each element of the select_expression and stick all the subsets together

  return(out)

}
