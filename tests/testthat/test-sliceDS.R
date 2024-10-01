library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

data("mtcars")
mtcars_group <- mtcars %>% group_by(cyl)
login_data <- .prepare_dslite("sliceDS", list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

mtcars_group <- mtcars %>% group_by(carb)

good_slice_arg <- "1:5"

test_that("sliceDS correctly filters where data and columns exist", {
  good_slice_cally <- .make_tidyverse_call("mtcars", "slice", good_slice_arg)
    expect_equal(
    dim(eval(good_slice_cally)),
    c(5, 11)
  )
})

test_that("sliceDS works with .by argument", {

  by_cally <- .make_tidyverse_call("mtcars", "slice", "1:5", ".by = 'cyl'")
  no_by_cally <- .make_tidyverse_call("mtcars", "slice", "1:5")

  slice_with_by <- eval(by_cally)
  slice_without_by <- eval(no_by_cally)

  expect_false(
    identical(
      rownames(slice_with_by),
      rownames(slice_without_by)
      )
  )

  expect_equal(
    dim(slice_with_by),
    c(15, 11))

  expect_equal(
    dim(slice_without_by),
    c(5, 11))

})

test_that("sliceDS works with .preserve argument", {

  preserve_cally_true <- .make_tidyverse_call("mtcars_group", "slice", "9:12", ".preserve = TRUE")
  expect_equal(
    group_keys(eval(preserve_cally_true)),
    tibble(carb = c(1, 2, 3, 4, 6, 8))
  )

  preserve_cally_false <- .make_tidyverse_call("mtcars_group", "slice", "9:12", ".preserve = FALSE")
  expect_equal(
    group_keys(eval(preserve_cally_false)),
    tibble(carb = c(2, 4))
  )
})

test_that("sliceDS fails when data doesn't exist", {
  no_data <- .make_tidyverse_call("doesntexist", "slice", good_slice_arg)
  expect_error(
    eval(no_data),
    "object 'doesntexist' not found"
  )
})

test_that("sliceDS passes when called directly", {
  cally <- call("sliceDS", "1:5", "mtcars", NULL, FALSE)

  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "data.frame")

  expect_equal(
    ds.dim("test", datasources = conns)[[1]],
    c(5, 11)
  )
})
