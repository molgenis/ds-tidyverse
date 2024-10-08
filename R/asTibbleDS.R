#' @title Coerce a data frame or matrix to a tibble. Currently not implemented for lists or other
#' object types.
#' @description DataSHIELD implementation of \code{tibble::as_tibble}.
#' @param tidy_select Unused in present function.
#' @param x A data frame or matrix.
#' @param .rows The number of rows, useful to create a 0-column tibble or just as an additional
#' check.
#' @param .name_repair Treatment of problematic column names:
#'   \itemize{
#'     \item "minimal": No name repair or checks, beyond basic existence
#'     \item "unique": Make sure names are unique and not empty
#'     \item "check_unique": (default value), no name repair, but check they are unique
#'     \item "universal": Make the names unique and syntactic
#'   }
#' @param rownames How to treat existing row names of a data frame or matrix:
#'   \itemize{
#'     \item `NULL`: remove row names. This is the default.
#'     \item `NA`: keep row names.
#'     \item A string: the name of a new column. Existing rownames are transferred
#'     into this column and the `row.names` attribute is deleted. No name repair is applied
#'     to the new column name, even if `x` already contains a column of that name.
#'   }
#' @return the object specified by the \code{newobj} argument of \code{ds.as_tibble} which is
#' written to the serverside.
#' @export
asTibbleDS <- function(tidy_select, x, .rows, .name_repair, rownames) {
  other_args <- .paste_character_args(.rows, .name_repair, rownames)
  call <- .make_tidyverse_call(x, "as_tibble", other_args)
  out <- .execute_with_error_handling("as_tibble", call)
  .check_subset_disclosure_risk(eval(parse(text = x), envir = parent.frame()), out)
  return(out)
}
