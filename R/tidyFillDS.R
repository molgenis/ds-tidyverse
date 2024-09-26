#' @title Subset rows using their positions
#' @description This function is similar to R function \code{dplyr::slice}.
#' @param expr Provide either positive values to keep, or negative values to drop. The values
#' provided must be either all positive or all negative. Indices beyond the number of rows in the
#' input are silently ignored.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame
#' (e.g. from dbplyr or dtplyr).
#' @param .by Optionally, a selection of columns to group by for just this operation, functioning as
#' an alternative to \code{group_by}.
#' @param .preserve Relevant when the .data input is grouped. If .preserve = FALSE (the default),
#' the grouping structure is recalculated based on the resulting data, otherwise the grouping is
#' kept as is.
#' @return the object specified by the \code{newobj} argument of \code{ds.slice} which is written
#' to the serverside.
#' @export

setAllLevelsDS <- function(df.name, vars, levels) {

  df.name <- eval(parse(text = df.name), envir = parent.frame())
  out <- df.name %>%
    mutate(across(all_of(vars), ~factor(., levels = levels[[cur_column()]])))

}

getAllLevelsDS <- function(df.name, factor_vars) {
  df <- eval(parse(text = df.name), envir = parent.frame())
  return(df %>% dplyr::select(all_of(factor_vars)) %>% map(levels))
}


fixClassDS <- function(df.name, target_vars, target_class) {

  df <- eval(parse(text = df.name), envir = parent.frame())
  df_transformed <- df %>%
    mutate(
      across(all_of(target_vars),
      ~ convert_class(., target_class[which(target_vars == cur_column())])))

  return(df_transformed)

}

convert_class <- function(x, class_name) {
  switch(class_name,
         "numeric" = as.numeric(x),
         "factor" = as.factor(x),
         "character" = as.character(x),
         x)
}

classAllColsDS <- function(df.name){
  df.name <- eval(parse(text = df.name), envir = parent.frame())
  all_classes <- map(df.name, class) %>% as_tibble()
  return(all_classes)
}

makeColsSameDS <- function(.data, cols) {
  .data <- eval(parse(text = .data), envir = parent.frame())
  missing <- setdiff(cols, colnames(.data))
  out <- .data %>%
    mutate(!!!setNames(rep(list(NA), length(missing)), missing))
  return(out)
}

