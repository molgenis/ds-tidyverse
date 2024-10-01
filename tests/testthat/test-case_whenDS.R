library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

data("mtcars")
login_data <- .prepare_dslite("caseWhenDS", list(mtcars = mtcars))
conns <- datashield.login(login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

test_that("caseWhenDS passes and numeric condition and categorical output", {
  case_when_arg <-
    'mtcars$mpg < 10 ~ "low",
  mtcars$mpg >= 10 & mtcars$mpg < 20 ~ "medium",
  mtcars$mpg >= 20 ~ "high"'

  other_args <- ".default = NULL, .ptype = NULL, .size = NULL"
  case_when_cally <- .make_tidyverse_call("mtcars", "case_when", case_when_arg, other_args, inc_data = F)
  result <- eval(case_when_cally)

  expect_equal(
    result,
    c(
      "high", "high", "high", "high", "medium", "medium", "medium", "high",
      "high", "medium", "medium", "medium", "medium", "medium", "medium",
      "medium", "medium", "high", "high", "high", "high", "medium", "medium",
      "medium", "medium", "high", "high", "high", "medium", "medium", "medium", "high"
    )
  )
})

test_that("caseWhenDS passes and numeric condition and numeric output", {
  case_when_arg <-
    "mtcars$mpg < 10 ~ 10,
  mtcars$mpg >= 10 & mtcars$mpg < 20 ~ 20,
  mtcars$mpg >= 20 ~ 30"

  other_args <- ".default = NULL, .ptype = NULL, .size = NULL"
  case_when_cally <- .make_tidyverse_call("mtcars", "case_when", case_when_arg, other_args, inc_data = F)
  result <- eval(case_when_cally)

  expect_equal(
    result,
    c(
      30, 30, 30, 30, 20, 20, 20, 30, 30, 20, 20, 20, 20, 20, 20, 20, 20, 30, 30, 30, 30, 20, 20,
      20, 20, 30, 30, 30, 20, 20, 20, 30
    )
  )
})

test_that("caseWhenDS passes with categorical condition and categorical output", {
  case_when_arg <-
    'mtcars$gear == 1 ~ "low",
     mtcars$gear == 2 ~ "medium",
     mtcars$gear == 3 ~ "high",
     mtcars$gear == 4 ~ "very_high"'

  other_args <- ".default = NULL, .ptype = NULL, .size = NULL"
  case_when_cally <- .make_tidyverse_call("mtcars", "case_when", case_when_arg, other_args, inc_data = F)
  result <- eval(case_when_cally)

  expect_equal(
    result,
    c(
      "very_high", "very_high", "very_high", "high", "high", "high", "high", "very_high", "very_high", "very_high",
      "very_high", "high", "high", "high", "high", "high", "high", "very_high", "very_high", "very_high",
      "high", "high", "high", "high", "high", "very_high", NA, NA, NA, NA,
      NA, "very_high"
    )
  )
})

test_that("caseWhenDS passes with .default argument", {
  case_when_arg <-
    'mtcars$gear == 1 ~ "low",
     mtcars$gear == 2 ~ "medium",
     mtcars$gear == 3 ~ "high",
     mtcars$gear == 4 ~ "very_high"'

  other_args <- ".default = 'something_missing', .ptype = NULL, .size = NULL"
  case_when_cally <- .make_tidyverse_call("mtcars", "case_when", case_when_arg, other_args, inc_data = F)
  result <- eval(case_when_cally)

  expect_equal(
    result,
    c(
      "very_high", "very_high", "very_high", "high", "high", "high",
      "high", "very_high", "very_high", "very_high", "very_high", "high",
      "high", "high", "high", "high", "high", "very_high",
      "very_high", "very_high", "high", "high", "high",
      "high", "high", "very_high", "something_missing",
      "something_missing", "something_missing", "something_missing",
      "something_missing", "very_high"
    )
  )
})

test_that("caseWhenDS passes when called directly", {
  cally <- call("caseWhenDS", "mtcars$mpg$SPACE$$LT$$SPACE$20$SPACE$$TILDE$$SPACE$$QUOTE$low$QUOTE$$COMMA$$SPACE$mtcars$mpg$SPACE$$GT$$EQU$$SPACE$20$SPACE$$AND$$SPACE$mtcars$mpg$SPACE$$LT$$SPACE$$LINE$$SPACE$$SPACE$$SPACE$$SPACE$30$SPACE$$TILDE$$SPACE$$QUOTE$medium$QUOTE$$COMMA$$SPACE$mtcars$mpg$SPACE$$GT$$EQU$$SPACE$30$SPACE$$TILDE$$SPACE$$QUOTE$high$QUOTE$", NULL, NULL, NULL)
  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "character"
  )

  expect_equal(
    as.numeric(ds.table("test", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts),
    c(12, 54, 30, 0)
  )
})
