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
dslite.server$assignMethod("bindColsDS", "dsTidyverse::bindColsDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)


test_that("bindColsDS passes", {

  bind_cols_arg <- "mtcars, mtcars"
  other_args <- '.name_repair = c("unique", "universal", "check_unique", "minimal")'
  bind_cols_cally <- .make_tidyverse_call(NULL, "bind_cols", bind_cols_arg, other_args, inc_data = F)
  result <- eval(bind_cols_cally)

  expect_equal(
    class(result),
    "data.frame"
    )

  expect_equal(
    colnames(result),
    c("mpg...1", "cyl...2", "disp...3", "hp...4", "drat...5", "wt...6", "qsec...7", "vs...8",
      "am...9", "gear...10", "carb...11", "mpg...12", "cyl...13", "disp...14", "hp...15",
      "drat...16", "wt...17", "qsec...18", "vs...19", "am...20", "gear...21", "carb...22")
  )

  expect_equal(
    dim(result),
    c(32, 22)
  )

})

  test_that("bind_rowsDS passes with .name_repair argument", {

    bind_cols_arg <- "mtcars, mtcars"
    other_args <- '.name_repair = "unique"'
    bind_cols_cally <- .make_tidyverse_call(NULL, "bind_cols", bind_cols_arg, other_args, inc_data = F)
    result <- eval(bind_cols_cally)

    expect_equal(
      colnames(result),
      c("mpg...1", "cyl...2", "disp...3", "hp...4", "drat...5", "wt...6", "qsec...7", "vs...8",
        "am...9", "gear...10", "carb...11", "mpg...12", "cyl...13", "disp...14", "hp...15",
        "drat...16", "wt...17", "qsec...18", "vs...19", "am...20", "gear...21", "carb...22")
    )

    other_args <- '.name_repair = "universal"'
    bind_cols_cally <- .make_tidyverse_call(NULL, "bind_cols", bind_cols_arg, other_args, inc_data = F)
    result <- eval(bind_cols_cally)

    expect_equal(
      colnames(result),
      c("mpg...1", "cyl...2", "disp...3", "hp...4", "drat...5", "wt...6", "qsec...7", "vs...8",
        "am...9", "gear...10", "carb...11", "mpg...12", "cyl...13", "disp...14", "hp...15",
        "drat...16", "wt...17", "qsec...18", "vs...19", "am...20", "gear...21", "carb...22")
    )

    other_args <- '.name_repair = "check_unique"'
    bind_cols_cally <- .make_tidyverse_call(NULL, "bind_cols", bind_cols_arg, other_args, inc_data = F)

    expect_error(
      eval(bind_cols_cally),
      "Names must be unique."
    )

    other_args <- '.name_repair = "minimal"'
    bind_cols_cally <- .make_tidyverse_call(NULL, "bind_cols", bind_cols_arg, other_args, inc_data = F)
    result <- eval(bind_cols_cally)

    expect_equal(
      colnames(result),
      c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb",
        "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
    )

})

test_that("bindColsDS passes when called directly", {
  cally <- call("bindColsDS", "mtcars$COMMA$$SPACE$mtcars", c("unique", "universal", "check_unique", "minimal"))
  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test")[[1]],
    "data.frame"
  )

  expect_equal(
    ds.colnames("test")[[1]],
    c("mpg...1", "cyl...2", "disp...3", "hp...4", "drat...5", "wt...6", "qsec...7", "vs...8",
      "am...9", "gear...10", "carb...11", "mpg...12", "cyl...13", "disp...14", "hp...15",
      "drat...16", "wt...17", "qsec...18", "vs...19", "am...20", "gear...21", "carb...22")
  )

  expect_equal(
    ds.dim("test")[[1]],
    c(32, 22)
  )

})
