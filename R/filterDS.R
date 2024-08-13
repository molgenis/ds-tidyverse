#' @title Performs dplyr filter
#' @description This function is similar to R function \code{filter}.
#' @details Performs dplyr filter
#' @param expr Diffused expression of dotdotdot passed to ds.filter
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as
#' an alternative to \code{group_by}.
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is kept as is.
#' @return the object specified by the \code{newobj} argument of \code{ds.filter} which is written
#' to the serverside.
#' @export
filterDS <- function(expr, .data, .by, .preserve) {
  tidy_select <- .decode_tidy_eval(expr, .get_encode_dictionary())
  other_args <- .paste_character_args(.by, .preserve)
  call <- .make_tidyverse_call(.data, "filter", tidy_select, other_args)
  out <- .execute_with_error_handling("filter", call)
  .check_filter_disclosure_risk(.data, out)
  return(out)
}

#' Check Filter Disclosure Risk
#'
#' This function checks the disclosure risk when applying a filter on a dataset.
#' It evaluates the subset size and the difference in rows between the original
#' and subsetted data to ensure they meet the minimum threshold specified by
#' `nfilter.subset`.
#'
#' @param .data A string representing the name of the original dataset.
#' @param out The filtered dataset object.
#' @keywords internal
#' @return None. The function will throw an error if disclosure risk is detected.
#' @noRd
.check_filter_disclosure_risk <- function(.data, out) {
  nfilter.subset <- .get_nfilter_subset_value()
  dims <- .get_dimensions(.data, out)
  .check_subset_size(dims$subset, nfilter.subset)
  .check_rows_compared_with_original(dims$original, dims$subset, nfilter.subset)
}

#' Get `nfilter.subset` Value
#'
#' This function retrieves the value of `nfilter.subset` from the disclosure
#' settings.
#'
#' @keywords internal
#' @return The value of `nfilter.subset` from the disclosure settings.
#' @noRd
.get_nfilter_subset_value <- function() {
  return(
    listDisclosureSettingsDS()$nfilter.subset
  )
}

#' Get Dimensions of Original and Subset Data
#'
#' This function calculates the number of rows in the original and subsetted datasets.
#'
#' @param .data A string representing the name of the original dataset.
#' @param out The filtered dataset object.
#' @keywords internal
#' @return A list containing the number of rows in the original and subsetted datasets.
#' @noRd
.get_dimensions <- function(.data, out) {
  return(
    list(
      original = dim(eval(parse(text = .data), parent.frame()))[[1]],
      subset = dim(out)[[1]]
    )
  )
}

#' Check Subset Size
#'
#' This function checks if the number of rows in the subsetted data is below the
#' threshold defined by `nfilter.subset`. If it is, the function throws an error.
#'
#' @param subset_rows The number of rows in the subsetted data.
#' @param nfilter.subset The minimum allowed size for a subset as defined in the
#' disclosure settings.
#' @keywords internal
#' @return None. The function will throw an error if the subset size is too small.
#' @noRd
.check_subset_size <- function(subset_rows, nfilter.subset) {
  if(subset_rows < nfilter.subset){
    cli_abort(
      "Subset to be created is too small (< nfilter.subset)",
      call = NULL
    )
  }
}

#' Check Rows Compared with Original
#'
#' This function checks the difference in the number of rows between the original
#' and subsetted data. If the difference is smaller than `nfilter.subset` but
#' greater than zero, it raises an error, indicating a potential disclosure risk.
#'
#' @param original_rows The number of rows in the original dataset.
#' @param subset_rows The number of rows in the subsetted dataset.
#' @param nfilter.subset The minimum allowed difference between the original and
#' subsetted data, as defined in the disclosure settings.
#' @keywords internal
#' @return None. The function will throw an error if a potential disclosure risk
#' is detected.
#' @noRd
.check_rows_compared_with_original <- function(original_rows, subset_rows, nfilter.subset) {
  diff <- original_rows - subset_rows
  if((diff < nfilter.subset) & (diff > 0)) {
    cli_abort(
      "The difference in row length between the original dataframe and the new dataframe is {diff},
      which is lower than the value of nfilter.subset ({nfilter.subset}). This could indicate a potential subsetting
      attack which will be recorded in the serverside logs. Please review the filter expression.",
      call = NULL
    )
  }
}
