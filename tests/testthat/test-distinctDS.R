require(DSI)
require(DSLite)
require(dplyr)
require(dsBase)
require(dsBaseClient)

options(datashield.errors.print = TRUE)
data("mtcars")
login_data <- .prepare_dslite("distinctDS", NULL, list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

empty_distinct_arg <- character(0)

test_that("distinctDS correctly works where no `expr` argument is provided", {
  empty_distinct_cally <- .make_tidyverse_call("mtcars", "distinct", empty_distinct_arg)

  out_empty <- eval(empty_distinct_cally)
  expect_equal(
    class(out_empty),
    "data.frame"
  )

  expect_equal(
    dim(out_empty),
    c(32, 11)
  )
})

test_that("distinctDS correctly works where variables specified in `expr` argument", {
  expr_distinct_cally <- .make_tidyverse_call("mtcars", "distinct", "cyl, drat")

  out_expr <- eval(expr_distinct_cally)

  expect_equal(
    class(out_expr),
    "data.frame"
  )

  expect_equal(
    dim(out_expr),
    c(26, 2)
  )
})

test_that("distinctDS correctly works with `.keep_all` argument", {
  keep_all_y_cally <- .make_tidyverse_call("mtcars", "distinct", "cyl, drat", ".keep_all = TRUE")
  keep_all_y_out <- eval(keep_all_y_cally)

  expect_equal(
    class(keep_all_y_out),
    "data.frame"
  )

  expect_equal(
    dim(keep_all_y_out),
    c(26, 11)
  )

  expect_equal(
    colnames(keep_all_y_out),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

  keep_all_n_cally <- .make_tidyverse_call("mtcars", "distinct", "cyl, drat", ".keep_all = FALSE")
  keep_all_n_out <- eval(keep_all_n_cally)

  expect_equal(
    class(keep_all_n_out),
    "data.frame"
  )

  expect_equal(
    dim(keep_all_n_out),
    c(26, 2)
  )

  expect_equal(
    colnames(keep_all_n_out),
    c("cyl", "drat")
  )
})

test_that("distinctDS fails when data doesn't exist", {
  no_data <- .make_tidyverse_call("doesntexist", "distinct", empty_distinct_arg)
  expect_error(
    eval(no_data),
    "object 'doesntexist' not found"
  )
})

test_that("distinctDS passes when called directly", {
  skip_if_not_installed("dsBaseClient")
  cally <- call("distinctDS", "mpg$COMMA$$SPACE$cyl", "mtcars", FALSE)
  datashield.assign(conns, "test_distinct", cally)

  expect_equal(
    ds.class("test_distinct", datasources = conns)[[1]],
    "data.frame"
  )

  expect_equal(
    ds.dim("test_distinct", datasources = conns)[[1]],
    c(27, 2)
  )
})

test_that("distinctDS fails if subset is too small", {
  cally <- call("distinctDS", "mpg$COMMA$$SPACE$cyl$COMMA$$SPACE$drat", "mtcars", FALSE)
  expect_error(
    datashield.assign(conns, "test_distinct", cally)
  )
})
