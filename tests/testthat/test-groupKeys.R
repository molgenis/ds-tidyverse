library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(DSI)

data("mtcars")
mtcars_group <- mtcars %>% group_by(cyl) %>% mutate(drop_test = factor("a", levels = c("a", "b")))
mtcars_bad_group <- mtcars %>% group_by(qsec)

login_data <- .prepare_dslite(
  NULL, "groupKeysDS",
  list(mtcars = mtcars, mtcars_group = mtcars_group, mtcars_bad_group = mtcars_bad_group))
conns <- datashield.login(logins = login_data)

datashield.assign.table(conns, "mtcars", "mtcars")
datashield.assign.table(conns, "mtcars_group", "mtcars_group")
datashield.assign.table(conns, "mtcars_bad_group", "mtcars_bad_group")

test_that("groupKeysDS correctly returns keys when no disclosure risk", {
  good_keys_call <- .make_tidyverse_call("mtcars_group", "group_keys", tidy_select = NULL, other_args = NULL)

  expect_equal(
    eval(good_keys_call),
    tibble(cyl = c(4, 6, 8))
  )
})

test_that("groupKeysDS fails when data doesn't exist", {
  no_data_call <- .make_tidyverse_call("doesnt_exist", "group_keys", tidy_select = NULL, other_args = NULL)
  expect_error(
    eval(no_data_call),
    "object 'doesnt_exist' not found"
  )
})

test_that("groupKeys passes when called directly and no disclosure risk", {
  call_direct <- call("groupKeysDS", NULL, "mtcars_group")
  groups_returned <- datashield.aggregate(conns, call_direct)

  expect_equal(
    class(groups_returned[[1]]),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    groups_returned[[1]],
    tibble(cyl = c(4, 6, 8))
  )
})

test_that("groupKeys fails when called directly with disclosure risk", {
  call_disc <- call("groupKeysDS", "mtcars_bad_group")
  expect_error(datashield.aggregate(conns, call_disc))
})

test_that(".check_n_groups_compared_with_original doesn't through error if number of groups not too high", {
  density_val <- 0.3
  dims_valid <- list(original = 100, subset = 10)
  expect_silent(.check_n_groups_compared_with_original(dims_valid, density_val))

})

test_that(".check_n_groups_compared_with_original detects disclosure risk correctly", {
  density_val <- 0.3
  dims_high_risk <- list(original = 100, subset = 80)

  expect_error(
    .check_n_groups_compared_with_original(dims_high_risk, 0.3),
    "The group keys cannot be returned due to a disclosure risk"
  )
})

test_that(".check_group_keys_disclosure_risk doesn't through error if number of groups not too high", {
  original_valid <- data.frame(id = 1:100)
  out_valid <- data.frame(id = 1:20)

  expect_silent(
    with_mocked_bindings(
      .check_group_keys_disclosure_risk(original_valid, out_valid),
      ".get_disclosure_value" = function(value) 0.33
    )
  )

})

test_that(".check_group_keys_disclosure_risk throws error if number of groups too high", {
  original_valid <- data.frame(id = 1:100)
  out_valid <- data.frame(id = 1:90)

  expect_error(
    with_mocked_bindings(
      .check_group_keys_disclosure_risk(original_valid, out_valid),
      ".get_disclosure_value" = function(value) 0.33
    )
  )

})
