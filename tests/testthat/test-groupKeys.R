library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(DSI)

options(datashield.env = environment())
data("mtcars")
mtcars_group <- mtcars %>%
  group_by(cyl) %>%
  mutate(drop_test = factor("a", levels = c("a", "b")))

mtcars_bad_group <- mtcars %>% group_by(qsec)

dslite.server <- newDSLiteServer(
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group,
    mtcars_bad_group = mtcars_bad_group
  )
)

dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$aggregateMethod("groupKeysDS", "groupKeysDS")
dslite.server$aggregateMethod("listDisclosureSettingsDS", "listDisclosureSettingsDS")

builder <- DSI::newDSLoginBuilder()

builder$append(
  server = "server_1",
  url = "dslite.server",
  table = "mtcars",
  driver = "DSLiteDriver"
)

logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata, assign = TRUE)

datashield.assign.table(
  conns = conns,
  table = "mtcars_group",
  symbol = "mtcars_group"
)

datashield.assign.table(
  conns = conns,
  table = "mtcars_bad_group",
  symbol = "mtcars_bad_group"
)

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
  call_direct <- call("groupKeysDS", "mtcars_group")
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
