require(DSI)
require(DSLite)
require(dplyr)
require(dsBase)
require(dsBaseClient)

data("mtcars")
mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))
login_data <- .prepare_dslite("groupByDS", NULL, list(mtcars = mtcars, mtcars_group = mtcars_group))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "mtcars_group", "mtcars_group")

test_that("groupByDS correctly groups data where data and columns exist", {
  good_group_expr <- "cyl"
  group_call <- .make_tidyverse_call("mtcars", "group_by", good_group_expr)

  expect_equal(
    group_keys(eval(group_call)),
    tibble(cyl = c(4, 6, 8))
  )

  mult_group_expr <- "cyl, mpg"
  mult_call <- .make_tidyverse_call("mtcars", "group_by", mult_group_expr)

  expect_equal(
    group_keys(eval(mult_call)),
    tibble(
      cyl = c(rep(4, 9), rep(6, 6), rep(8, 12)),
      mpg = c(
        21.4, 21.5, 22.8, 24.4, 26, 27.3, 30.4, 32.4, 33.9,
        17.8, 18.1, 19.2, 19.7, 21, 21.4,
        10.4, 13.3, 14.3, 14.7, 15, 15.2, 15.5, 15.8, 16.4, 17.3, 18.7, 19.2
      )
    )
  )
})

test_that("groupByDS works with .add argument", {
  add_true_call <- .make_tidyverse_call("mtcars_group", "group_by", "gear", ".add = TRUE")
  grouped_add_true <- eval(add_true_call)
  expect_equal(
    group_keys(grouped_add_true),
    tibble(cyl = c(4, 4, 4, 6, 6, 6, 8, 8), gear = c(3, 4, 5, 3, 4, 5, 3, 5))
  )

  add_false_call <- .make_tidyverse_call("mtcars_group", "group_by", "cyl", ".add = FALSE")
  grouped_add_false <- eval(add_false_call)
  expect_equal(
    group_keys(grouped_add_false),
    tibble(cyl = c(4, 6, 8))
  )
})

test_that("groupByDS works with .drop argument", {
  drop_true_call <- .make_tidyverse_call("mtcars_group", "group_by", "drop_test", ".drop = TRUE")
  grouped_drop_true <- eval(drop_true_call)
  expect_equal(
    group_keys(grouped_drop_true)$drop_test,
    factor("a", levels = c("a", "b"))
  )

  drop_false_call <- .make_tidyverse_call("mtcars_group", "group_by", "drop_test", ".drop = FALSE")
  grouped_drop_false <- eval(drop_false_call)
  expect_equal(
    group_keys(grouped_drop_false)$drop_test,
    factor(c("a", "b"), levels = c("a", "b"))
  )
})

test_that("groupByDS fails when data doesn't exist", {
  no_data_call <- .make_tidyverse_call("doesntexist", "group_by", "cyl")
  expect_error(
    eval(no_data_call),
    "object 'doesntexist' not found"
  )
})

test_that("groupByDS fails with bad argument", {
  bad_group_expr <- "wrong_column"
  bad_call <- .make_tidyverse_call("mtcars", "group_by", bad_group_expr)
  expect_error(
    eval(bad_call),
    "Must group by variables found in `.data`"
  )
})

test_that("groupByDS passes when called directly", {
  skip_if_not_installed("dsBaseClient")
  call_direct <- call("groupByDS", "cyl", "mtcars", FALSE, TRUE)
  datashield.assign(conns, "test_group", call_direct)

  expect_equal(
    ds.class("test_group", datasources = conns)[[1]],
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
})


add_true_call <- .make_tidyverse_call("mtcars_group", "group_by", "gear", ".add = TRUE")
grouped_add_true <- eval(add_true_call)


test_that("ungroupDS correctly ungroups data", {
  ungroup_call <- .make_tidyverse_call("mtcars_group", "ungroup", tidy_expr = NULL, other_args = NULL)
  ungrouped_data <- eval(ungroup_call)

  expect_equal(
    class(ungrouped_data),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("ungroupDS works with already ungrouped data", {
  ungroup_call <- .make_tidyverse_call("mtcars", "ungroup", tidy_expr = NULL, other_args = NULL)
  ungrouped_data <- eval(ungroup_call)

  expect_equal(
    class(ungrouped_data),
    "data.frame"
  )
})

test_that("ungroupDS fails when data doesn't exist", {
  no_data_call <- .make_tidyverse_call("doesntexist", "ungroup", tidy_expr = NULL, other_args = NULL)
  expect_error(
    eval(no_data_call),
    "object 'doesntexist' not found"
  )
})

data("mtcars")
mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))
login_data <- .prepare_dslite("ungroupDS", NULL, list(mtcars_group = mtcars_group))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars_group", "mtcars_group")

print(datashield.tables(conns))
print(ds.ls(datasources = conns))

test_that("ungroupDS works correctly when called directly", {
  skip_if_not_installed("dsBaseClient")
  ungroup_call <- call("ungroupDS", NULL, "mtcars_group")
  datashield.assign(conns, "ungrouped_data", ungroup_call)

  expect_equal(
    ds.class("ungrouped_data", datasources = conns)[[1]],
    c("tbl_df", "tbl", "data.frame")
  )
})
