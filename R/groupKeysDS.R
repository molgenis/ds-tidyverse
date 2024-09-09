#' @title Performs dplyr \code{group_keys}.
#' @param x a grouped tibble.
#' @return A tibble describing the groups is returned to the client.
#' @export
groupKeysDS <- function(x) {
  call <- .make_tidyverse_call(x, "group_keys", tidy_select = NULL, other_args = NULL)
  out <- .execute_with_error_handling("group_keys", call)
  .check_group_keys_disclosure_risk(eval(parse(text = x), envir = parent.frame()), out)
  return(out)
}

#' Check Group Keys Disclosure Risk
#'
#' This internal function checks the disclosure risk by comparing the number of groups in the
#' original dataset with the number of groups in the output dataset. If the number of groups in
#' the output dataset is too close to the number of groups in the original dataset, a disclosure
#' risk is detected and an error is thrown.
#'
#' @param original A data frame representing the original data
#' @param out A data frame representing a summary of the groups
#' @return Silent if no disclosure risk, else error is thrown.
#' @noRd
.check_group_keys_disclosure_risk <- function(original, out) {
  nfilter.levels.density <- .get_disclosure_value("nfilter.levels.density")
  dims <- .get_dimensions(original, out)
  .check_n_groups_compared_with_original(dims, nfilter.levels.density)
}

#' Check Number of Groups Compared with Original Dataset
#'
#' This internal function evaluates the number of groups in a subset against a calculated disclosure
#' threshold. If the number of groups in the subset exceeds the threshold, a disclosure risk is
#' detected and an error is raised to prevent the return of group keys that might compromise data
#' privacy.
#'
#' @param dims A list containing the dimensions of the original and subset datasets. It should
#' include `original` and `subset` elements representing the number of groups in the original and
#' subset datasets, respectively.
#' @param disclosure_value A numeric value representing the disclosure threshold
#' @return Silent if no disclosure risk, else error is thrown.
#' @noRd
.check_n_groups_compared_with_original <- function(dims, disclosure_value) {
  threshold <- dims$original * disclosure_value
  if (threshold < dims$subset) {
    cli_abort(
      c("The group keys cannot be returned due to a disclosure risk",
        "i" = "The length of the dataset is {dims$original} and the number of groups is {dims$subset}",
        "i" = "Based on current disclosure settings the minimum number of groups required to return
      these values must be >= {threshold}",
        call = NULL
      )
    )
  }
}
