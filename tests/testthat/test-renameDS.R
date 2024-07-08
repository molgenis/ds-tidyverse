library(DSLite)
library(dplyr)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("renameDS", "dsTidyverse::renameDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettingsTidyVerse", "dsTidyverse::dsListDisclosureSettingsTidyVerse")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

.encode_tidy_eval <- function(input_string, encode_key) {
  encode_vec <- set_names(encode_key$output, encode_key$input)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
}

good_rename_arg <- "test_1 = mpg, test_2 = drat"

test_that("renameDS passes for rename where data and column exist", {
  good_rename_cally <- .make_tidyverse_call("mtcars", "rename", good_rename_arg)
  expected <- c("test_1", "cyl", "disp", "hp", "test_2", "wt", "qsec", "vs", "am", "gear", "carb")
  expect_equal(
    colnames(eval(good_rename_cally)),
    expected
  )
})

test_that("renameDS fails when data doesn't exist", {
  no_data <- .make_tidyverse_call("doesntexist", "rename", good_rename_arg)
  expect_error(
    eval(no_data),
    "`renameDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'doesntexist'\\s+not\\s+found"
  )
})

test_that("renameDS fails with bad argument", {
  bad_rename_arg <- "test_1 = mpg, test_2 = drat, filter/asd"
  bad_call <- .make_tidyverse_call("mtcars", "rename", bad_rename_arg)
  expect_error(
    eval(bad_call),
    "Can't rename columns that don't exist\\."
  )
})
