library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(DSI)

# Load necessary libraries
library(dplyr)
library(purrr)


df <- create_mixed_dataframe()
df_list <- create_additional_dataframes(df)

df_1 <- df
df_2 <- df_list[[1]]
df_3 <- df_list[[2]]
df_4 <- df_list[[3]]

test_that("classAllColsDS returns correct classes", {
  expect_equal(
    classAllColsDS("df_1"),
    tibble(
      fac_col1 = "factor", fac_col2 = "factor", fac_col3 = "factor", fac_col4 = "factor", fac_col5 = "factor",
      fac_col6 = "factor", fac_col7 = "factor", fac_col8 = "factor", fac_col9 = "factor", fac_col10 = "factor",
      fac_col11 = "factor", fac_col12 = "factor", fac_col13 = "factor", fac_col14 = "factor", fac_col15 = "factor",
      col16 = "integer", col17 = "integer", col18 = "numeric", col19 = "numeric", col20 = "character",
      col21 = "character", col22 = "integer", col23 = "numeric", col24 = "character", col25 = "integer",
      col26 = "numeric", col27 = "character", col28 = "integer", col29 = "numeric", col30 = "character"
    )
  )
})

test_that("fixClassDS sets classes correctly", {

  set.seed <- 123
  cols_to_set <- sample(colnames(df_1), 10)
  classes_to_set <- sample(1:5, 10, replace = TRUE)

  classes_changed_df <- fixClassDS("df_1", cols_to_set, classes_to_set)

  expect_equal(
    classes_changed_df %>% map_chr(class) %>% unname(),
    c(
      "integer", "factor", "factor", "factor", "factor", "factor", "factor",
      "numeric", "factor", "factor", "factor", "factor", "factor", "factor",
      "character", "integer", "integer", "numeric", "character", "character", "character",
      "logical", "numeric", "character", "integer", "numeric", "character", "integer",
      "numeric", "character"
    )
  )
})

test_that("convert_class calls the correct function", {

result <- convert_class(c(1, 2, 3), "1")
expect_true(is.factor(result))

result <- convert_class(c(1.5, 2.5, 3.7), "2")
expect_true(is.integer(result))

result <- convert_class(c("1", "2", "3"), "3")
expect_true(is.numeric(result))

result <- convert_class(c(1, 2, 3), "4")
expect_true(is.character(result))

result <- convert_class(c(0, 1, 0), "5")
expect_true(is.logical(result))

}



colnames(df_1)

fixClassDS








 View the structure of the dataframe
str(df)


# View the dataframe
print(df)


options(datashield.env = environment())



data("mtcars")


dslite.server <- newDSLiteServer(
  tables = list(
    mtcars = mtcars,
    mtcars_group = mtcars_group
  )
)

dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("arrangeDS", "arrangeDS")
dslite.server$aggregateMethod("listDisclosureSettingsDS", "listDisclosureSettingsDS")

builder <- DSI::newDSLoginBuilder()

builder$append(
  server = "server_1",
  url = "dslite.server",
  table = "mtcars",
  driver = "DSLiteDriver"
)

logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata, assign = FALSE)

datashield.assign.table(
  conns = conns,
  table = "mtcars",
  symbol = "mtcars"
)

datashield.assign.table(
  conns = conns,
  table = "mtcars_group",
  symbol = "mtcars_group"
)

good_arrange_arg <- "mpg, cyl"

test_that("arrangeDS correctly arranges data", {
  good_arrange_cally <- .make_tidyverse_call("mtcars", "arrange", good_arrange_arg)

  sorted_df <- eval(good_arrange_cally)

  expect_equal(
    class(sorted_df),
    "data.frame"
  )

  expect_equal(
    sorted_df$mpg,
    c(10.4, 10.4, 13.3, 14.3, 14.7, 15.0, 15.2, 15.2, 15.5, 15.8, 16.4, 17.3, 17.8, 18.1, 18.7,
      19.2, 19.2, 19.7, 21.0, 21.0, 21.4, 21.4, 21.5, 22.8, 22.8, 24.4, 26.0, 27.3, 30.4, 30.4,
      32.4, 33.9)
    )

  expect_equal(
    sorted_df$cyl,
    c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 6, 8, 6, 8, 6, 6, 6, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      4)
    )
})


test_that("arrangeDS works with .by_group argument", {

  by_cally <- .make_tidyverse_call("mtcars_group", "arrange", "drat", ".by_group = TRUE")
  no_by_cally <- .make_tidyverse_call("mtcars_group", "arrange", "drat", ".by_group = FALSE")

  arrange_by_t <- eval(by_cally)
  arrange_by_f <- eval(no_by_cally)

  expect_equal(
    class(arrange_by_t),
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    class(arrange_by_f),
    c("grouped_df", "tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    arrange_by_t$drat,
    c(3.69, 3.70, 3.77, 3.85, 3.92, 4.08, 4.08, 4.11, 4.22, 4.43, 4.93, 2.76, 3.08, 3.62, 3.90,
    3.90, 3.92, 3.92, 2.76, 2.93, 3.00, 3.07, 3.07, 3.07, 3.08, 3.15, 3.15, 3.21, 3.23, 3.54,
    3.73, 4.22)
  )

  expect_equal(
    arrange_by_f$drat,
    c(2.76, 2.76, 2.93, 3.00, 3.07, 3.07, 3.07, 3.08, 3.08, 3.15, 3.15, 3.21, 3.23, 3.54, 3.62,
    3.69, 3.70, 3.73, 3.77, 3.85, 3.90, 3.90, 3.92, 3.92, 3.92, 4.08, 4.08, 4.11, 4.22, 4.22,
    4.43, 4.93)
  )
})

test_that("arrangeDS works with desc option", {
  desc_arrange_arg <- "list(desc(mpg))"
  desc_arrange_cally <- .make_tidyverse_call("mtcars", "arrange", desc_arrange_arg)

  desc_df <- eval(desc_arrange_cally)

  expect_equal(
    class(desc_df),
    "data.frame"
  )

  expect_equal(
    desc_df$mpg,
    c(21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,
      16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5,
      15.5, 15.2, 13.3, 19.2, 27.3, 26.0, 30.4, 15.8, 19.7, 15.0, 21.4)
  )
})

test_that("arrangeDS fails when data doesn't exist", {
  no_data <- .make_tidyverse_call("doesnt_exist", "arrange", good_arrange_arg)
  expect_error(
    eval(no_data),
    "object 'doesnt_exist' not found"
  )
})

test_that("arrangeDS passes when called directly", {
  cally <- call("arrangeDS", "drat", "mtcars", NULL)
  datashield.assign(conns, "sorted_df", cally)

  expect_equal(
    ds.class("sorted_df")[[1]],
    "data.frame"
    )

  expect_equal(
    ds.dim("sorted_df")[[1]],
    c(32, 11)
  )
})

test_that("arrangeDS works with desc option when called directly", {
  cally <- call("arrangeDS", "desc$LB$drat$RB$", "mtcars", NULL)
  datashield.assign(conns, "sorted_df", cally)

  expect_equal(
    ds.class("sorted_df")[[1]],
    "data.frame"
  )

  expect_equal(
    ds.dim("sorted_df")[[1]],
    c(32, 11)
  )
})
