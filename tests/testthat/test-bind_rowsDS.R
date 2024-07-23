library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(purrr)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("bindRowsDS", "dsTidyverse::bindRowsDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)


test_that("bindRowsDS passes", {
  bind_rows_arg <- "mtcars, mtcars"
  other_args <- ".id = NULL"
  bind_rows_cally <- .make_tidyverse_call(NULL, "bind_rows", bind_rows_arg, other_args, inc_data = F)
  result <- eval(bind_rows_cally)

  expect_equal(
    class(result),
    "data.frame"
    )

  expect_equal(
    colnames(result),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

  expect_equal(
    dim(result),
    c(64, 11)
  )

  expect_equal(
    result %>% map_dbl(mean) %>% round(2),
    c(mpg = 20.09, cyl = 6.19, disp = 230.72, hp = 146.69, drat = 3.60, wt = 3.22, qsec = 17.85, vs = 0.44, am = 0.41, gear = 3.69, carb = 2.81)
  )

})

  test_that("bindRowsDS passes with .id argument", {
    bind_rows_arg <- "mtcars, mtcars"
    other_args <- ".id = 'test_col_name'"
    bind_rows_cally <- .make_tidyverse_call(NULL, "bind_rows", bind_rows_arg, other_args, inc_data = F)
    result <- eval(bind_rows_cally)

    expect_equal(
      colnames(result),
      c("test_col_name", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
    )

  })

test_that("bindRowsDS passes when called directly", {
  cally <- call("bindRowsDS", "mtcars$COMMA$$SPACE$mtcars", NULL)
  datashield.assign(conns, "test", cally)


  expect_equal(
    ds.class("test")[[1]],
    "data.frame"
  )

  expect_equal(
    ds.colnames("test")[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )

  expect_equal(
    ds.dim("test")[[1]],
    c(64, 11)
  )

})
