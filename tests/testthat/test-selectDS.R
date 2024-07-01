library(dplyr)
library(purrr)
library(cli)

.encode_tidy_eval <- function(input_string, encode_key) {
  encode_vec <- set_names(encode_key$output, encode_key$input)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
}

good_select_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
good_select_arg_enc <- .encode_tidy_eval(good_select_arg, .get_encode_dictionary())

test_that("selectDS passes where data and column exist", {
  good_select_cally <- call("selectDS", "mtcars", good_select_arg_enc)
  expected <- c("mpg", "cyl", "gear", "carb")
  expect_equal(
    colnames(eval(good_select_cally)),
    expected
  )
})

call_no_data <- .make_tidyverse_call("data_not_there", "select", good_select_arg)

test_that("selectDS fails with correct message when data doesn't exist", {
  select_cally_no_data <- call("selectDS", "doesntexist", good_select_arg_enc)
  expect_error(
    eval(select_cally_no_data),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'data_not_there'\\s+not\\s+found"
  )
})

select_random_arg <- "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
random_select_arg_enc <- .encode_tidy_eval(select_random_arg, .get_encode_dictionary())
random_select_cally <- call("selectDS", "mtcars", random_select_arg_enc)

test_that(".execute_tidyverse_function fails with correct message when unrecognised function passed", {
  expect_error(
    eval(random_select_cally),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|no\\s+applicable\\s+method\\s+for\\s+'filter'"
  )
})
