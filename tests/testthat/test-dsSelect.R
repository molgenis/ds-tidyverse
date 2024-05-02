library(dplyr)
library(DSLite)
library(dsBaseClient)

test_that(".make_tidyselect_arg creates argument to pass to `eval_tidy", {
  input_string <- "asd, qwe, starts_with('test')"
  expected_string <- "test %>% dplyr::select(asd, qwe, starts_with('test'))"
  observed_string <- .make_tidyselect_arg(.data = "test", fun = "select", tidy_select_args = input_string)
  expect_equal(expected_string, observed_string)
})

test_that(".make_tidyselect_arg fails with correct message if attempt to use non permitted tidyverse serverside command", {
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
    error = TRUE)
})

mtcars_missing_col_arg <-  "all_of('test_col'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_missing_col_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_missing_col_arg)
mtcars_missing_col_expr <- rlang::parse_expr(mtcars_missing_col_str)

test_that(".tidy_eval_handle_errors fails with correct message if column doesn't exist", {
  expect_snapshot(
    .tidy_eval_handle_errors(mtcars_missing_col_expr, "mtcars"),
    error = TRUE)
})

mtcars_random_arg <-  "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_random_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_random_arg)
mtcars_random_expr <- rlang::parse_expr(mtcars_random_str)

test_that(".tidy_eval_handle_errors fails with correct message when unrecognised function passed", {
  expect_snapshot(
    .tidy_eval_handle_errors(mtcars_random_expr, "mtcars"),
    error = TRUE)
})

test_that(".execute_tidyverse_function passes where data and column exist", {
  observed <- .execute_tidyverse_function("mtcars", "select", mtcars_good_arg)
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))
})

test_that(".execute_tidyverse_function fails with correct message when data doesn't exist", {
  expect_snapshot(
    .execute_tidyverse_function("data_not_there", "select", mtcars_good_arg),
    error = TRUE)
})

test_that(".execute_tidyverse_function fails with correct message when unrecognised function passed", {
  expect_snapshot(
    .execute_tidyverse_function("mtcars", "select", mtcars_random_arg),
    error = TRUE)
})

.encode_tidy_eval <- function(input_string, encode_key) {
  encode_vec <- set_names(encode_key$output, encode_key$input)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
}

.wrap_assign_call <- function(tidy_select_as_string, newobj){
  args_encoded <- .encode_tidy_eval(tidy_select_as_string, .getEncodeKey())
  cally <- call("selectDS", "mtcars", args_encoded)
  datashield.assign(conns, newobj, cally)
}

.check_cols_as_expected <- function(expected, newobj){
  observed <- ds.colnames(newobj)[[1]]
  expected <- expected
  expect_equal(observed, expected)
}

.wrap_assign_call_no_data <- function(tidy_select_as_string){
  args_encoded <- .encode_tidy_eval(tidy_select_as_string, .getEncodeKey())
  cally <- call("selectDS", "asdasd", args_encoded)
  datashield.assign(conns, "test", cally)
}

## ---- Log in -------------------------------------------------------------------------------------
# data("mtcars")
# dslite.server <- newDSLiteServer(tables=list(mtcars=mtcars))
# data("logindata.dslite.cnsim")
# logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
#   mutate(table = "mtcars")
# dslite.server$config(defaultDSConfiguration(include=c("dsBase", "dsTidyverse", "dsDanger")))
# dslite.server$assignMethod("selectDS", "selectDS")
# conns <- datashield.login(logindata.dslite.cnsim, assign=TRUE)

## ---- Tests --------------------------------------------------------------------------------------
# test_that("selectDS fails with correct error message ", {
#   expect_snapshot(
#     .wrap_assign_call_no_data("mpg:drat")
#     error = TRUE)
# })

test_that("selectDS correctly passes : ", {
  data("mtcars")
  dslite.server <- newDSLiteServer(tables=list(mtcars=mtcars))
  data("logindata.dslite.cnsim")
  logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
    mutate(table = "mtcars")
  dslite.server$config(defaultDSConfiguration(include=c("dsBase", "dsTidyverse", "dsDanger")))
  dslite.server$assignMethod("selectDS", "selectDS")
  conns <- datashield.login(logindata.dslite.cnsim, assign=TRUE)
    select_arg = "mpg:drat"
    args_encoded <- .encode_tidy_eval(select_arg, .getEncodeKey())
    cally <- call("selectDS", "mtcars", args_encoded)
    datashield.assign(conns, "test2", cally)
    observed <- ds.colnames("test2")[[1]]
    expected <- c("mpg", "cyl", "disp", "hp", "drat")
    expect_equal(observed, expected)
})

# test_that(".execute_tidyverse_function correctly passes `starts_with`", {
#   select_arg <- "starts_with('m')"
#   args_encoded <- .encode_tidy_eval(select_arg, .getEncodeKey())
#   cally <- call("selectDS", "mtcars", args_encoded)
#   datashield.assign(conns, "starts", cally)
#   observed <- ds.colnames("starts")[[1]]
#   expected <- "mpg"
#   expect_equal(observed, expected)
# })
#
# test_that(".execute_tidyverse_function correctly passes `ends_with`", {
#   select_arg <- "ends_with('m')"
#   .wrap_assign_call(select_arg, "ends")
#   .check_cols_as_expected("am", "ends")
# })
#
# test_that(".execute_tidyverse_function correctly passes `matches`", {
#   select_arg <- "matches('[aeiou]')"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(c("disp", "drat", "qsec", "am", "gear", "carb"))
# })
#
# test_that(".execute_tidyverse_function correctly passes `everything`", {
#   select_arg <- "everything()"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(colnames(mtcars))
# })
#
# test_that(".execute_tidyverse_function correctly passes `last_col`", {
#   select_arg <- "last_col()"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected("carb")
# })
#
# test_that(".execute_tidyverse_function correctly passes `group_cols`", {
#   select_arg <- "group_cols()"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(character(0))
# })
#
# test_that(".execute_tidyverse_function correctly passes strings with '&'", {
#   select_arg <- "starts_with('c') & ends_with('b')"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected("carb")
# })
#
# test_that(".execute_tidyverse_function correctly passes strings with '!'", {
#   select_arg <- "!mpg"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
# })
#
# test_that(".execute_tidyverse_function correctly passes strings with '|'", {
#   select_arg <- "starts_with('c') | ends_with('b')"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(c("cyl", "carb"))
# })
#
# test_that(".execute_tidyverse_function correctly passes `strings with `all_of`", {
#   select_arg <- "all_of(c('mpg','cyl'))"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(c("mpg", "cyl"))
# })
#
# test_that(".execute_tidyverse_function correctly passes strings with `any_of`", {
#   select_arg <- "any_of(c('mpg','cyl'))"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(c("mpg", "cyl"))
# })
#
# test_that(".execute_tidyverse_function correctly passes complex strings", {
#   select_arg <- "(starts_with('c') & ends_with('b')) | contains('ra') | gear:carb"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(c("carb", "drat", "gear"))
# })
#
# test_that(".execute_tidyverse_function correctly passes strings with `where`", {
#   select_arg <- "where(is.numeric)"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected(colnames(mtcars))
# })
#
# test_that(".execute_tidyverse_function correctly passes strings with '='", {
#   select_arg <- "test = mpg"
#   .wrap_assign_call(select_arg)
#   .check_cols_as_expected("test")
# })

