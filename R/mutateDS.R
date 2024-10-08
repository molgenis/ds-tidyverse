#' @title Create, modify, and delete columns
#' @description DataSHIELD implentation of \code{mutate}.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param tidy_expr Name-value pairs. The name gives the name of the column in the output.
#' @param .keep Control which columns from .data are retained in the output. Grouping columns and
#' columns created by ... are always kept. "all" retains all columns from .data. This is the default.
#' "used" retains only the columns used in \code{tidy_expr} to create new columns. "unused" retains
#' only the columns not used in \code{tidy_expr} to create new columns. This is useful if you generate
#' new columns, but no longer need the columns used to generate them. "none" doesn't retain any
#' extra columns from \code{df.name}. Only the grouping variables and columns created by \code{tidy_expr}
#' are kept.
#' @param .before <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See \code{relocate} for more details.
#' @param .after <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See \code{relocate} for more details.
#' @return the object specified by the \code{newobj} argument of \code{ds.mutate} which is written
#' to the serverside.
#' @export
mutateDS <- function(tidy_expr, .data, .keep = NULL, .before = NULL, .after = NULL) {
  checkPermissivePrivacyControlLevel(c('permissive', 'banana', 'avacado'))
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(.data, tidy_expr)
  other_args <- .paste_character_args(.keep, .before, .after)
  call <- .make_tidyverse_call(.data, "mutate", tidy_expr, other_args)
  out <- .execute_with_error_handling("mutate", call)
  return(out)
}
