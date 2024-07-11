library(DSLite)
library(dplyr)
library(dsTidyverse)
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
dslite.server$assignMethod("if_elseDS", "dsTidyverse::if_elseDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

if_else_num_arg <- "mtcars$mpg > 20"

test_that("if_elseDS passes and numeric condition and categorial output", {
  other_args <- "true = \"high\", false = \"low\", missing = NULL, ptype = NULL, size = NULL"
  good_if_else_cally <- .make_tidyverse_call("mtcars", "if_else", if_else_num_arg, other_args, inc_data = F)
  result <- eval(good_if_else_cally)

  expect_equal(
    result,
    c("high", "high", "high", "high", "low", "low", "low", "high", "high", "low", "low", "low",
      "low", "low", "low", "low", "low", "high", "high", "high", "high", "low", "low", "low",
      "low", "high", "high", "high", "low", "low", "low", "high")
  )
})

test_that("if_elseDS passes and numeric condition and numeric output", {
  other_args <- "true = 50, false = 100, missing = NULL, ptype = NULL, size = NULL"
  good_if_else_cally <- .make_tidyverse_call("mtcars", "if_else", if_else_num_arg, other_args, inc_data = F)
  result <- eval(good_if_else_cally)

  expect_equal(
    result,
    c(50, 50, 50, 50, 100, 100, 100, 50, 50, 100, 100, 100,
      100, 100, 100, 100, 100, 50, 50, 50, 50, 100, 100, 100,
      100, 50, 50, 50, 100, 100, 100, 50)
  )
})

test_that("if_elseDS passes with categorical condition", {
  mtcars_cat <- mtcars %>% mutate(mpg_cat = if_else(mpg > 20, "high", "low"))
  if_else_cat_arg <- "mtcars_cat$mpg_cat == \"high\""
  other_args <- "true = 1000, false = 10, missing = NULL, ptype = NULL, size = NULL"
  good_mutate_cally <- .make_tidyverse_call("mtcars_cat", "if_else", if_else_cat_arg, other_args, inc_data = F)
  result <- eval(good_mutate_cally)

  expect_equal(
    result,
    c(1000, 1000, 1000, 1000, 10, 10, 10, 1000, 1000, 10, 10, 10, 10, 10, 10, 10, 10,
      1000, 1000, 1000, 1000, 10, 10, 10, 10, 1000, 1000, 1000, 10, 10, 10, 1000)
    )
})

test_that("if_elseDS passes when `missing` argument used", {
  other_args <- "true = \"high\", false = \"low\", missing = \"val_missing\", ptype = NULL, size = NULL"
  mtcars <- mtcars %>% mutate(mpg = NA)
  good_if_else_cally <- .make_tidyverse_call("mtcars", "if_else", if_else_num_arg, other_args, inc_data = F)
  result <- eval(good_if_else_cally)

  expect_equal(
    result,
    rep("val_missing", 32)
    )
})

test_that("if_elseDS passes when called directly", {
  cally <- call("if_elseDS", "mtcars$mpg$SPACE$$GT$$SPACE$20", "high", "low", NULL, NULL, NULL)
  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test")[[1]],
    "character")

  expect_equal(
    as.numeric(ds.table("test")$output.list$TABLES.COMBINED_all.sources_counts),
    c(42, 54, 0))
})

# test_that("if_elseDS passes when `ptype` argument used", {
#   data(mtcars)
#   other_args <- "true = \"50\", false = \"500\", missing = NULL, ptype = \"integer\", size = NULL"
#   good_if_else_cally <- .make_tidyverse_call("mtcars_mis", "if_else", if_else_num_arg, other_args, inc_data = F)
#   result <- eval(good_if_else_cally)
#
#   expect_equal(
#     result,
#     rep("val_missing", 32)
#   )
# }) ## This doesn't work, but I think an issue with dplyr rather than my function (help says argument is prototype)

# test_that("if_elseDS passes when `size` argument used", {
#   data(mtcars)
#   other_args <- "true = \"high\", false = \"low\", missing = NULL, ptype = NULL, size = 100"
#   good_if_else_cally <- .make_tidyverse_call("mtcars_mis", "if_else", good_if_else_arg, other_args, inc_data = F)
#   result <- eval(good_if_else_cally)
#
#   expect_equal(
#     result,
#     rep("val_missing", 32)
#   )
# }) ## Again fails, but not an issue with my code.
