library(dplyr)
library(purrr)
library(cli)

test_that(".make_tidyselect_arg creates argument to pass to `eval_tidy", {
  input_string <- "asd, qwe, starts_with('test')"
  expected_string <- "test %>% dplyr::select(asd, qwe, starts_with('test'))"
  observed_string <- .make_tidyselect_arg(.data = "test", fun = "select", tidy_select_args = input_string)
  expect_equal(expected_string, observed_string)
})

test_that_cli(configs = "ansi", ".make_tidyselect_arg fails with correct message if attempt to use non permitted tidyverse serverside command", {
  expect_snapshot(
    .execute_tidyverse_function("mtcars", "filter", mtcars_random_arg),
    error = TRUE)
})

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_good_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_good_arg)
mtcars_good_expr <- rlang::parse_expr(mtcars_good_str)

test_that(".tidy_eval_handle_errors works where data and object exists", {
  observed <- .tidy_eval_handle_errors(mtcars_good_expr, "mtcars")
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))
})

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_wrong_data_str <- .make_tidyselect_arg(.data = "data_not_here", fun = "select", tidy_select_args = mtcars_good_arg)
mtcars_wrong_data_expr <- rlang::parse_expr(mtcars_wrong_data_str)

test_that(".tidy_eval_handle_errors fails with correct message if object doesn't exist", {
  expect_snapshot(
    .tidy_eval_handle_errors(mtcars_wrong_data_expr, "data_not_here"),
    error = TRUE
  )
})

mtcars_missing_col_arg <- "all_of('test_col'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_missing_col_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_missing_col_arg)
mtcars_missing_col_expr <- rlang::parse_expr(mtcars_missing_col_str)

test_that(".tidy_eval_handle_errors fails with correct message if column doesn't exist", {
  expect_snapshot(
    .tidy_eval_handle_errors(mtcars_missing_col_expr, "mtcars"),
    error = TRUE
  )
})

mtcars_random_arg <- "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_random_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_random_arg)
mtcars_random_expr <- rlang::parse_expr(mtcars_random_str)

test_that(".tidy_eval_handle_errors fails with correct message when unrecognised function passed", {
  expect_snapshot(
    .tidy_eval_handle_errors(mtcars_random_expr, "mtcars"),
    error = TRUE
  )
})

test_that(".execute_tidyverse_function passes where data and column exist", {
  observed <- .execute_tidyverse_function("mtcars", "select", mtcars_good_arg)
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))
})

test_that(".execute_tidyverse_function fails with correct message when data doesn't exist", {
  expect_snapshot(
    .execute_tidyverse_function("data_not_there", "select", mtcars_good_arg),
    error = TRUE
  )
})

test_that(".execute_tidyverse_function fails with correct message when unrecognised function passed", {
  expect_snapshot(
    .execute_tidyverse_function("mtcars", "select", mtcars_random_arg),
    error = TRUE
  )
})
