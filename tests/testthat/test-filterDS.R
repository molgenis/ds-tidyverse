library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("filterDS", "dsTidyverse::filterDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

good_filter_arg <- "mpg > 20 & gear > 2"

test_that("filterDS correctly filters where data and columns exist", {
  good_filter_cally <- .make_tidyverse_call("mtcars", "filter", good_filter_arg)
    expect_equal(
    dim(eval(good_filter_cally))[[1]],
    14
  )
})
#
# test_that("renameDS fails when data doesn't exist", {
#   no_data <- .make_tidyverse_call("doesntexist", "rename", good_rename_arg)
#   expect_error(
#     eval(no_data),
#     "`renameDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'doesntexist'\\s+not\\s+found"
#   )
# })
#
# test_that("renameDS fails with bad argument", {
#   bad_rename_arg <- "test_1 = mpg, test_2 = drat, filter/asd"
#   bad_call <- .make_tidyverse_call("mtcars", "rename", bad_rename_arg)
#   expect_error(
#     eval(bad_call),
#     "Can't rename columns that don't exist\\."
#   )
# })
#
# test_that("renameDS passes when called directly", {
#   cally <- call("renameDS", "new_name_1$SPACE$$EQU$$SPACE$mpg$COMMA$$SPACE$new_name_2$SPACE$$EQU$$SPACE$drat",
#   "mtcars")
#
#   datashield.assign(conns, "test", cally)
#
#   expect_equal(
#     ds.class("test")[[1]],
#     "data.frame")
#
#   expect_equal(
#     ds.colnames("test")[[1]],
#     c("new_name_1", "cyl", "disp", "hp", "new_name_2", "wt", "qsec", "vs", "am", "gear", "carb")
#   )
# })

