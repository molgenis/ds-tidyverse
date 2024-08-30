library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(DSI)

options(datashield.env = environment())
data("mtcars")
mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

mtcars_bad_group <- mtcars %>% group_by(qsec)
dslite.server <- newDSLiteServer(
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group,
    mtcars_bad_group = mtcars_bad_group
  )
)

dslite.server$config(defaultDSConfiguration(include=c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("groupByDS", "groupByDS")
dslite.server$assignMethod("ungroupDS", "ungroupDS")
dslite.server$aggregateMethod("groupKeysDS", "groupKeysDS")
dslite.server$aggregateMethod("listDisclosureSettingsDS", "listDisclosureSettingsDS")

builder <- DSI::newDSLoginBuilder()

builder$append(
  server="server_1",
  url="dslite.server",
  table = "mtcars",
  driver = "DSLiteDriver")

logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata, assign = TRUE)

test_that("groupByDS correctly groups data where data and columns exist", {
  good_group_expr <- "cyl"
  group_call <- .make_tidyverse_call("mtcars", "group_by", good_group_expr)

  expect_equal(
    group_keys(eval(group_call)),
    tibble(cyl = c(4, 6, 8))
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
  call_direct <- call("groupByDS", "cyl", "mtcars", FALSE, TRUE)
  datashield.assign(conns, "test_group", call_direct)

  expect_equal(
    ds.class("test_group")[[1]],
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
})
