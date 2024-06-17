library(dplyr)
library(purrr)
library(cli)

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
test_that(".execute_tidyverse_function passes where data and column exist", {
  observed <- .execute_tidyverse_function("mtcars", "select", mtcars_good_arg)
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))
})

test_that(".execute_tidyverse_function fails with correct message when data doesn't exist", {
  expect_error(
    .execute_tidyverse_function("data_not_there", "select", mtcars_good_arg),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'data_not_there'\\s+not\\s+found"
  )
})

mtcars_random_arg <- "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
test_that(".execute_tidyverse_function fails with correct message when unrecognised function passed", {
  expect_error(
    .execute_tidyverse_function("mtcars", "select", mtcars_random_arg),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|no\\s+applicable\\s+method\\s+for\\s+'filter'"
  )
})
