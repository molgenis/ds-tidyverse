library(dplyr)
library(purrr)
library(cli)

rename_good_arg <- "new_name_1 = mpg, new_name_2 = drat"

test_that(".execute_tidyverse_function passes where data and column exist", {
  observed <- .execute_tidyverse_function("mtcars", "rename", rename_good_arg)
  expected <- c("new_name_1", "cyl", "disp", "hp", "new_name_2", "wt", "qsec", "vs", "am", "gear", "carb")
  expect_equal(colnames(observed), expected)
})

test_that(".execute_tidyverse_function fails with correct message when data doesn't exist", {
  expect_error(
    .execute_tidyverse_function("data_not_there", "rename", rename_good_arg),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'data_not_there'\\s+not\\s+found"
  )
})
