library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("filterDS", "dsTidyverse::filterDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

good_filter_arg <- "mpg > 20 & gear > 2"

test_that("filterDS correctly filters where data and columns exist", {
  good_filter_cally <- .make_tidyverse_call("mtcars", "filter", good_filter_arg)
    expect_equal(
    dim(eval(good_filter_cally))[[1]],
    14
  )
})

test_that("filterDS works with .preserve argument", {
  mtcars_group <- mtcars %>% group_by(cyl)

  preserve_cally_true <- .make_tidyverse_call("mtcars_group", "filter", good_filter_arg, ".preserve = TRUE")
  expect_equal(
    group_keys(eval(preserve_cally_true)),
    tibble(cyl = c(4, 6, 8))
  )

  preserve_cally_false <- .make_tidyverse_call("mtcars_group", "filter", good_filter_arg, ".preserve = FALSE")
  expect_equal(
    group_keys(eval(preserve_cally_false)),
    tibble(cyl = c(4, 6))
  )
})

test_that("filterDS fails when data doesn't exist", {
  no_data <- .make_tidyverse_call("doesntexist", "filter", good_filter_arg)
  expect_error(
    eval(no_data),
    "`filterDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'doesntexist'\\s+not\\s+found"
  )
})

test_that("filterDS fails with bad argument", {
  bad_filter_arg <- "test_1 = mpg, mutate/asd"
  bad_call <- .make_tidyverse_call("mtcars", "filter", bad_filter_arg)
  expect_error(
    eval(bad_call),
    "We detected a named input\\."
  )
})

test_that("filterDS passes when called directly", {
  cally <- call("filterDS", "carb$SPACE$$EQU$$EQU$$SPACE$4", "mtcars", NULL)

  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test")[[1]],
    "data.frame")

  expect_equal(
    ds.dim("test")[[1]],
    c(10, 11)
  )
})

