library(dplyr)
library(purrr)
library(cli)

.encode_tidy_eval <- function(input_string, encode_key) {
  encode_vec <- set_names(encode_key$output, encode_key$input)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
}

good_mutate_arg <- "mpg_trans = cyl*1000, new_var = (hp-drat)/qsec"
good_mutate_arg_enc <- .encode_tidy_eval(good_mutate_arg, .get_encode_dictionary())

test_that("mutateDS passes where data and column exist", {
  good_mutate_cally <- call("mutateDS", "mtcars", good_mutate_arg_enc, "all", NULL, NULL)
  result <- eval(good_mutate_cally)

  expect_equal(
    mean(result$mpg_trans),
    6187.5)

  expect_equal(
    colnames(result),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "mpg_trans", "new_var")
  )
})

test_that("mutateDS passes with different .keep argument", {
  good_mutate_cally <- call("mutateDS", "mtcars", good_mutate_arg_enc, "none", NULL, NULL)
  result <- eval(good_mutate_cally)

  expect_equal(
    colnames(result),
    c("mpg_trans", "new_var")
  )
})

test_that("mutateDS passes with different .before argument", {
  good_mutate_cally <- call("mutateDS", "mtcars", good_mutate_arg_enc, "all", "disp", NULL)
  result <- eval(good_mutate_cally)

  expect_equal(
    colnames(result),
    c("mpg", "cyl", "mpg_trans", "new_var", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})

test_that("mutateDS passes with different .afterb argument", {
  good_mutate_cally <- call("mutateDS", "mtcars", good_mutate_arg_enc, "all", NULL, "qsec")
  result <- eval(good_mutate_cally)

  expect_equal(
    colnames(result),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "mpg_trans", "new_var", "vs", "am", "gear", "carb")
  )
})
