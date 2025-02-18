#' @title Keep distinct/unique rows
#' @description DataSHIELD implentation of \code{dplyr::distinct}.
#' @param tidy_expr Optional variables to use when determining uniqueness. If there are multiple rows for
#' a given combination of inputs, only the first row will be preserved. If omitted, will use all
#' variables in the data frame.
#' @param df.name A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .keep_all If TRUE, keep all variables in df.name If a combination of expr is not distinct,
#' this keeps the first row of values.
#' @return An object of the same type as \code{df.name}, typically a data frame or tibble.
#' @export
distinctDS <- function(tidy_expr, df.name, .keep_all) {
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  other_args <- .paste_character_args(.keep_all)
  call <- .make_tidyverse_call(df.name, "distinct", tidy_expr, other_args)
  out <- .execute_with_error_handling("distinct", call)
  .check_subset_disclosure_risk(eval(parse(text = df.name)), out)
  return(out)
}
