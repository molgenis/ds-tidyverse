library(dplyr)
library(purrr)
library(cli)

good_rename_arg <- "test_1 = mpg, test_2 = drat"
test_that(".execute_tidyverse_function passes for rename where data and column exist", {
  observed <- .execute_tidyverse_function("mtcars", "rename", good_rename_arg)
  expected <- c("test_1",  "cyl", "disp", "hp", "test_2", "wt", "qsec", "vs", "am", "gear", "carb")
  expect_equal(colnames(observed),  expected)
})

test_that(".execute_tidyverse_function fails with correct message when data doesn't exist", {
  expect_error(
    .execute_tidyverse_function("data_not_there", "rename", good_rename_arg),
    "`renameDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'data_not_there'\\s+not\\s+found"
  )
})

bad_rename_arg <- "test_1 = mutate(mpg)"
test_that(".execute_tidyverse_function fails with correct message when unrecognised function passed", {
  expect_error(
    .execute_tidyverse_function("mtcars", "rename", bad_rename_arg),
    "`renameDS`\\s+returned\\s+the\\s+following\\s+error:|Can't\\s+rename\\s+columns\\s+with\\s+`mutate(mpg)`"
  )
})
