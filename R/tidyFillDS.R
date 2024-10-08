#' Get the Class of All Columns in a Data Frame
#' @param df.name A string representing the name of the data frame.
#' @return A tibble with the class of each column in the data frame.
#' @importFrom dplyr %>%
#' @importFrom tibble as_tibble
#' @export
classAllColsDS <- function(df.name){
  df.name <- eval(parse(text = df.name), envir = parent.frame())
  all_classes <- map(df.name, class) %>% as_tibble()
  return(all_classes)
}

#' Change Class of Target Variables in a Data Frame
#' @param df.name A string representing the name of the data frame.
#' @param target_vars A character vector specifying the columns to be modified.
#' @param target_class A character vector specifying the new classes for each column (1 = factor,
#' 2 = integer, 3 = numeric, 4 = character, 5 = logical).
#' @return A modified data frame with the specified columns converted to the target classes.
#' @export
fixClassDS <- function(df.name, target_vars, target_class) {
  df <- eval(parse(text = df.name), envir = parent.frame())
  df_transformed <- df %>%
    mutate(
      across(all_of(target_vars),
             ~ convert_class(., target_class[which(target_vars == cur_column())])))
  return(df_transformed)
}

#' Convert a Vector to a Specified Class
#' @param x The vector to be converted.
#' @param class_name A string indicating the target class (1 = factor, 2 = integer, 3 = numeric,
#' 4 = character, 5 = logical).
#'
#' @return The converted vector.
#' @export
convert_class <- function(x, class_name) {
  switch(class_name,
         "1" = as.factor(x),
         "2" = as.integer(x),
         "3" = as.numeric(x),
         "4" = as.character(x),
         "5" = as.logical(x)
  )
}

#' Add Missing Columns with NA Values
#' @param .data A string representing the name of the data frame.
#' @param cols A character vector specifying the columns to be added if missing.
#' @return A modified data frame with missing columns added and filled with NA.
#' @export
makeColsSameDS <- function(.data, cols) {
  .data <- eval(parse(text = .data), envir = parent.frame())
  missing <- setdiff(cols, colnames(.data))
  out <- .data %>%
    mutate(!!!setNames(rep(list(NA), length(missing)), missing)) %>%
    select(sort(peek_vars()))
  return(out)
}

#' Retrieve Factor Levels for Specific Columns
#' @param df.name A string representing the name of the data frame.
#' @param factor_vars A character vector specifying the factor columns.
#' @return A list of factor levels for the specified columns.
#' @export
getAllLevelsDS <- function(df.name, factor_vars) {
  df <- eval(parse(text = df.name), envir = parent.frame())
  return(df %>% dplyr::select(all_of(factor_vars)) %>% map(levels))
}


#' Set Factor Levels for Specific Columns in a Data Frame
#' @param df.name A string representing the name of the data frame to modify.
#' @param vars A character vector specifying the columns to be modified.
#' @param levels A named list where each element contains the levels for the corresponding factor variable.
#' @return A modified data frame with the specified columns converted to factors with the provided levels.
#' @export
setAllLevelsDS <- function(df.name, vars, levels) {
  df.name <- eval(parse(text = df.name), envir = parent.frame())
  out <- df.name %>%
    mutate(across(all_of(vars), ~factor(., levels = levels[[cur_column()]])))
}
