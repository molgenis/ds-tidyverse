#' @title Create, modify, and delete columns
#' @description DataSHIELD implentation of \code{mutate}.
#' @param df.name A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param tidy_expr Name-value pairs. The name gives the name of the column in the output.
#' @param .keep .keep Control which columns from \code{df.name} are retained in the output. Grouping
#' columns and columns created by \code{tidy_expr} are always kept.
#' \itemize{
#'   \item \code{"all"}: Retains all columns from \code{df.name}. This is the default.
#'   \item \code{"used"}: Retains only the columns used in \code{tidy_expr} to create new columns.
#'   \item \code{"unused"}: Retains only the columns not used in \code{tidy_expr} to create new columns. This is useful if you generate new columns but no longer need the columns used to generate them.
#'   \item \code{"none"}: Doesn't retain any extra columns from \code{df.name}. Only the grouping variables and columns created by \code{tidy_expr} are kept.
#' }
#' Grouping columns and columns created by \code{tidy_expr} are always kept.
#' @param .before <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See \code{relocate} for more details.
#' @param .after <tidy-select> Optionally, control where new columns should appear (the default is
#' to add to the right hand side). See \code{relocate} for more details.
#' @return An object of the same type as \code{df.name}, typically a data frame or tibble.
#' @export
mutateDS <- function(tidy_expr, df.name, .keep = NULL, .before = NULL, .after = NULL) {
  checkPermissivePrivacyControlLevel('permissive')
  tidy_expr <- .decode_tidy_eval(tidy_expr, .get_encode_dictionary())
  .check_tidy_disclosure(df.name, tidy_expr)
  other_args <- .paste_character_args(.keep, .before, .after)
  call <- .make_tidyverse_call(df.name, "mutate", tidy_expr, other_args)
  out <- .execute_with_error_handling("mutate", call)
  return(out)
}
