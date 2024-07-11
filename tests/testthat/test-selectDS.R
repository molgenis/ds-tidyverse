library(dplyr)
library(purrr)
library(cli)
library(DSLite)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
mtcars <- mtcars %>% mutate(cat_var = factor(ifelse(mpg > 20, "high", "low")))
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
dslite.server$assignMethod("selectDS", "dsTidyverse::selectDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

good_select_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"

test_that("selectDS passes where data and column exist", {
  good_select_cally <- .make_tidyverse_call("mtcars", "select", good_select_arg)
  expected <- c("mpg", "cyl", "gear", "carb")
  expect_equal(
    colnames(eval(good_select_cally)),
    expected
  )
})

call_no_data <- .make_tidyverse_call("data_not_there", "select", good_select_arg)

test_that("selectDS fails with correct message when data doesn't exist", {
  select_cally_no_data <- .make_tidyverse_call("doesntexist", "select", good_select_arg)
  expect_error(
    eval(select_cally_no_data),
    "object 'doesntexist' not found"
  )
})

test_that(".execute_tidyverse_function fails with correct message when unrecognised function passed", {
  select_random_arg <- "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
  random_select_cally <- .make_tidyverse_call("mtcars", "select", select_random_arg)
  expect_error(
    eval(random_select_cally),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|no\\s+applicable\\s+method\\s+for\\s+'filter'"
  )
})

test_that("select passes when called directly", {

  cally <- call("selectDS", "mpg$COMMA$$SPACE$starts_with$LB$$QUOTE$m$QUOTE$$RB$$COMMA$$SPACE$ends_with$LB$$QUOTE$t$QUOTE$$RB$",
                "mtcars")
  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test")[[1]],
    "data.frame")

  expect_equal(
    ds.colnames("test")[[1]],
    c("mpg", "drat", "wt")
  )
})
