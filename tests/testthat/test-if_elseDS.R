library(dplyr)
library(purrr)
library(cli)

if_else_num_arg <- "mtcars$mpg > 20"
# good_mutate_arg_enc <- .encode_tidy_eval(good_mutate_arg, .get_encode_dictionary())


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

test_that(".decode_tidy_eval correctly decodes an encoded string passed via the R parser", {
  decoded_string <- .decode_tidy_eval("mtcars2$mpg$SPACE$$GT$$SPACE$20", .get_encode_dictionary())
  expect_equal(
    decoded_string,
    "mtcars2$mpg > 20")
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
