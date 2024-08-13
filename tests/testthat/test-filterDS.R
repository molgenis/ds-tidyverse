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
mtcars_group <- mtcars %>% group_by(cyl)

test_that("filterDS correctly filters where data and columns exist", {
  good_filter_cally <- .make_tidyverse_call("mtcars", "filter", good_filter_arg)
    expect_equal(
    dim(eval(good_filter_cally))[[1]],
    14
  )
})

test_that("filterDS works with .by argument", {

  by_cally <- .make_tidyverse_call("mtcars", "filter", "mpg > median(mpg)", ".by = 'cyl'")
  no_by_cally <- .make_tidyverse_call("mtcars", "filter", "mpg > median(mpg)")

  filter_with_by <- eval(by_cally)
  filter_without_by <- eval(no_by_cally)

  expect_false(
    identical(
      rownames(filter_with_by),
      rownames(filter_without_by)
      )
  )

  expect_equal(
    dim(filter_with_by),
    c(14, 11))

})

test_that("filterDS works with .preserve argument", {
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

test_that("filterDS passes with valid combinations of extra arguments", {
  just_by <- .make_tidyverse_call("mtcars", "filter", good_filter_arg, ".by = 'carb'")
  just_preserve <- .make_tidyverse_call("mtcars_group", "filter", good_filter_arg, ".preserve = T")
  by_preserve_f <- .make_tidyverse_call("mtcars", "filter", good_filter_arg, c(".by = 'carb', .preserve = FALSE"))

  expect_silent(eval(just_by))
  expect_silent(eval(just_preserve))
  expect_silent(eval(by_preserve_f))

})

test_that("filterDS fails if supplied with .by and .preserve = T", {
  by_preserve_t <- .make_tidyverse_call("mtcars", "filter", good_filter_arg, c(".by = 'carb', .preserve = T"))
  expect_error(
    eval(by_preserve_t),
    "Can't supply both `.by` and `.preserve`"
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
  cally <- call("filterDS", "carb$SPACE$$EQU$$EQU$$SPACE$4", "mtcars", NULL, FALSE)

  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test")[[1]],
    "data.frame")

  expect_equal(
    ds.dim("test")[[1]],
    c(10, 11)
  )
})

test_that(".get_nfilter_subset_value retrieves value correctly", {
  expect_equal(
    .get_nfilter_subset_value(),
    3)
})

test_that(".get_dimensions correctly returns dimensions", {
  original <- tibble(a = 1:3, b = 4:6)
  out <- tibble(a = 1:2, b = 1:2)

  returned <- .get_dimensions(original, out)

  expect_equal(
    class(returned),
    "list")

  expect_equal(
    returned,
    list(original = 3, subset = 2)
  )

})

test_that(".check_subset_size returns an error if subset is too small", {
  expect_error(
    .check_subset_size(subset_rows = 3, nfilter.subset = 10),
    ".*Subset to be created is too small \\(< nfilter\\.subset\\).*"
  )
})

test_that(".check_subset_size doesn't return an error if subset is large enough", {
  expect_silent(
    .check_subset_size(subset_rows = 20, nfilter.subset = 2)
    )
})

test_that(".check_rows_compared_with_original returns an error correctly", {
  expect_error(
    .check_rows_compared_with_original(10, 9, 3),
    "The difference in row length between the original dataframe and the new dataframe"
  )
})

test_that(".check_rows_compared_with_original doesn't return an error if different in dimensions is large enough", {
  expect_silent(
    .check_rows_compared_with_original(100, 80, 3)
  )
})

test_that(".check_filter_disclosure_risk returns an error correctly", {
  original <- tibble(a = 1:3, b = 4:6)
  out <- tibble(a = 1:2, b = 1:2)

  expect_error(
    .check_filter_disclosure_risk(original, out),
    ".*Subset to be created is too small \\(< nfilter\\.subset\\).*")

  original <- tibble(a = 1:10, b = 1:10)
  out <- tibble(a = 1:9, b = 1:9)

  expect_error(
    .check_filter_disclosure_risk(original, out),
    "The difference in row length between the original dataframe and the new dataframe"
  )

})

test_that(".check_filter_disclosure_risk doesn't return errors if subset sizes are ok", {
  original <- tibble(a = 1:10, b = 1:10)
  out <- tibble(a = 1:4, b = 1:4)

  expect_silent(
    .check_filter_disclosure_risk(original, out)
  )

  original <- tibble(a = 1:10, b = 1:10)
  out <- tibble(a = 1:5, b = 1:5)

  expect_silent(
    .check_filter_disclosure_risk(original, out)
  )

})
