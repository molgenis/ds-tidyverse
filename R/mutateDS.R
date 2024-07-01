#' @title Performs dplyr mutate
#' @description This function is similar to R function \code{rename}.
#' @details Performs dplyr rename
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param expr Diffused expression of dotdotdot passed to ds.rename
#' @param .keep Control which columns from .data are retained in the output. Grouping columns and
#' columns created by ... are always kept. "all" retains all columns from .data. This is the default.
#' "used" retains only the columns used in `tidy_select` to create new columns. "unused" retains
#' only the columns not used in `tidy_select` to create new columns. This is useful if you generate
#' new columns, but no longer need the columns used to generate them. "none" doesn't retain any
#' extra columns from `df.name`. Only the grouping variables and columns created by `tidy_select`
#' are kept.
#' @param .before <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See `relocate` for more details.
#' @param .after <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See `relocate` for more details.
#' @return the object specified by the \code{newobj} argument
#' of \code{ds.rename} (or as default same name as input object) which is written to the serverside.
#' @export
#'
mutateDS <- function(.data, expr, .keep = NULL, .before = NULL, .after = NULL) {
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  other_args <- .paste_character_args(.keep, .before, .after)
  call <- .make_tidyverse_call(.data, "mutate", tidy_select, other_args)
  out <- .execute_with_error_handling("mutate", call)
  return(out)
}
