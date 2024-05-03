library(dplyr)
library(DSLite)
library(dsBaseClient)
library(purrr)
library(cli)
library(DSI)
library(dsTidyverseClient)

options(datashield.env=environment())
data("mtcars")
dslite.server <- newDSLiteServer(tables=list(mtcars=mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include=c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "selectDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign=TRUE)

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

.check_cols_as_expected <- function(expected, df){
  observed <- ds.colnames(df)[[1]]
  expected <- expected
  expect_equal(observed, expected)
}

.wrap_assign_call_no_data <- function(tidy_select_as_string){
  args_encoded <- .encode_tidy_eval(tidy_select_as_string, .getEncodeKey())
  cally <- call("selectDS", "asdasd", args_encoded)
  datashield.assign(conns, "test", cally)
}

# test_that("ds.select fails with correct error message if data not present ", {
#   expect_snapshot(
#     ds.select(
#       .data = "datanotthere",
#       tidy_select = list(mpg:drat),
#       newobj = "nodata"))
#   })## Wait until DSI updated

test_that("selectDS correctly passes : ", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(mpg:drat),
    newobj = "mpg_drat")
  expected <- c("mpg", "cyl", "disp", "hp", "drat")
  .check_cols_as_expected(expected, "mpg_drat")
})

test_that(".execute_tidyverse_function correctly passes `starts_with`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(starts_with("m")),
    newobj = "starts")
  expected <- "mpg"
  .check_cols_as_expected(expected, "starts")
})

test_that(".execute_tidyverse_function correctly passes `ends_with`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(ends_with('m')),
    newobj = "ends")
  expected <- "am"
  .check_cols_as_expected(expected, "ends")
})

test_that(".execute_tidyverse_function correctly passes `matches`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(matches('[aeiou]')),
    newobj = "matches")
  expected <- c("disp", "drat", "qsec", "am", "gear", "carb")
  .check_cols_as_expected(expected, "matches")
})

test_that(".execute_tidyverse_function correctly passes `everything`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(everything()),
    newobj = "everything")
  expected <- colnames(mtcars)
  .check_cols_as_expected(expected, "everything")
})

test_that(".execute_tidyverse_function correctly passes `last_col`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(last_col()),
    newobj = "last")
  expected <- "carb"
  .check_cols_as_expected(expected, "last")
})

test_that(".execute_tidyverse_function correctly passes `group_cols`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(group_cols()),
    newobj = "group")
  expected <- character(0)
  .check_cols_as_expected(expected, "group")
})

test_that(".execute_tidyverse_function correctly passes strings with '&'", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(starts_with('c') & ends_with('b')),
    newobj = "and")
  expected <- "carb"
  .check_cols_as_expected(expected, "and")
})

test_that(".execute_tidyverse_function correctly passes strings with '!'", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(!mpg),
    newobj = "not")
  expected <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  .check_cols_as_expected(expected, "not")
})

test_that(".execute_tidyverse_function correctly passes strings with '|'", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(starts_with('c') | ends_with('b')),
    newobj = "or")
  expected <- c("cyl", "carb")
  .check_cols_as_expected(expected, "or")
})

test_that(".execute_tidyverse_function correctly passes `strings with `all_of`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(all_of(c('mpg','cyl'))),
    newobj = "all_of")
  expected <- c("mpg", "cyl")
  .check_cols_as_expected(expected, "all_of")
})

test_that(".execute_tidyverse_function correctly passes strings with `any_of`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(any_of(c('mpg','cyl'))),
    newobj = "any_of")
  expected <- c("mpg", "cyl")
  .check_cols_as_expected(expected, "any_of")
})

test_that(".execute_tidyverse_function correctly passes complex strings", {
  ds.select(
    .data = "mtcars",
    tidy_select = list((starts_with('c') & ends_with('b')) | contains('ra') | gear:carb),
    newobj = "complex")
  expected <- c("carb", "drat", "gear")
  .check_cols_as_expected(expected, "complex")
})

test_that(".execute_tidyverse_function correctly passes strings with `where`", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(where(is.numeric)),
    newobj = "where")
  expected <- colnames(mtcars)
  .check_cols_as_expected(expected, "where")
})

test_that(".execute_tidyverse_function correctly passes strings with '='", {
  ds.select(
    .data = "mtcars",
    tidy_select = list(test = mpg, cyl, gear),
    newobj = "equals")
  expected <- c("test", "cyl", "gear")
  .check_cols_as_expected(expected, "equals")
})
