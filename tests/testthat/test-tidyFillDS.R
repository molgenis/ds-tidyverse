library(DSLite)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(DSI)

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

  cols_to_set <- c("fac_col13", "fac_col5", "col22", "col19", "col25", "col20", "col28",
                   "fac_col14", "fac_col3", "fac_col8")

  classes_to_set <- c("4", "1", "3", "5", "3", "2", "5", "5", "3", "2")
  classes_changed_df <- fixClassDS("df_1", cols_to_set, classes_to_set)

  expect_equal(
    classes_changed_df %>% map_chr(class) %>% unname(),
    c("factor", "factor", "numeric", "factor", "factor", "factor", "factor", "integer", "factor",
      "factor", "factor", "factor", "character", "logical", "factor", "integer", "integer",
      "numeric", "logical", "integer", "character", "numeric", "numeric", "character", "numeric",
      "numeric", "character", "logical", "numeric", "character")
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

})

test_that("makeColsSameDS correctly adds missing columns", {

  all_cols <- unique(c(colnames(df_1), colnames(df_2), colnames(df_3), colnames(df_4)))
  out <- makeColsSameDS("df_3", all_cols)

  expect_equal(
    colnames(out),
    sort(all_cols))

})

test_that("getAllLevelsDS correctly retrieves the levels of specified factor columns", {

  factor_vars <- c("fac_col1", "fac_col2", "fac_col3", "fac_col4", "fac_col5", "fac_col6", "fac_col7",
                  "fac_col8", "fac_col9", "fac_col10", "fac_col11", "fac_col12", "fac_col14",
                  "fac_col15", "col27")

  observed <- getAllLevelsDS("df_3", factor_vars)

    expected <- list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Blue", "Green", "Red"),
      fac_col3 = c("No", "Yes"),
      fac_col4 = c("A", "B", "C"),
      fac_col5 = c("One", "Three", "Two"),
      fac_col6 = c("Bird", "Cat", "Dog"),
      fac_col7 = c("Large", "Medium", "Small"),
      fac_col8 = c("Alpha", "Beta", "Gamma"),
      fac_col9 = c("False", "True"),
      fac_col10 = c("Left", "Right"),
      fac_col11 = c("East", "North", "South", "West"),
      fac_col12 = c("Day", "Night"),
      fac_col14 = c("Female", "Male"),
      fac_col15 = c("Fall", "Spring", "Summer", "Winter"),
      col27 = letters
    )

  expect_equal(expected, observed)

})

#
# get_factor_levels <- function(df) {
#   factor_levels <- lapply(df, function(col) {
#     if (is.factor(col)) {
#       levels(col)
#     }
#   })
#
#   # Remove NULL entries for non-factor columns
#   factor_levels <- factor_levels[!sapply(factor_levels, is.null)]
#
#   return(factor_levels)
# }
#
# levels_1 <- get_factor_levels(df)
# levels_2 <- get_factor_levels(df_2)
# levels_3 <- get_factor_levels(df_3)
# levels_4 <- get_factor_levels(df_4)
#
# # Combine lists by including all possible elements and merging their character vectors
# combine_lists <- function(...){
#   # Combine all lists into one large list
#   all_lists <- list(...)
#
#   # Get all unique element names (keys)
#   all_keys <- unique(unlist(lapply(all_lists, names)))
#
#   # Initialize an empty list to store the combined result
#   combined_list <- list()
#
#   # Loop over each unique key
#   for (key in all_keys) {
#     # Extract and combine vectors from all lists for the current key, removing duplicates
#     combined_list[[key]] <- unique(unlist(lapply(all_lists, function(lst) {
#       if (key %in% names(lst)) {
#         return(lst[[key]])
#       } else {
#         return(NULL)
#       }
#     })))
#   }
#
#   return(combined_list)
# }
#
# # Apply the function to combine all four lists
# result <- combine_lists(levels_1, levels_2, levels_3, levels_4)
#
#
#
#
# # Remove NULL entries for non-factor columns
# factor_levels <- factor_levels[!sapply(factor_levels, is.null)]
#
# test_that("setAllLevelsDS correctly sets the levels of specified factor columns", {
#
# setAllLevelsDS(names(result), result)
#
# #
#
#
# colnames(df_1)
#
# fixClassDS
#
#
#
#
#
#
#
#
#  View the structure of the dataframe
# str(df)
#
#
# # View the dataframe
# print(df)
#
#
# options(datashield.env = environment())
#
#
#
# data("mtcars")
#
#
# dslite.server <- newDSLiteServer(
#   tables = list(
#     mtcars = mtcars,
#     mtcars_group = mtcars_group
#   )
# )
#
# dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
# dslite.server$assignMethod("arrangeDS", "arrangeDS")
# dslite.server$aggregateMethod("listDisclosureSettingsDS", "listDisclosureSettingsDS")
#
# builder <- DSI::newDSLoginBuilder()
#
# builder$append(
#   server = "server_1",
#   url = "dslite.server",
#   table = "mtcars",
#   driver = "DSLiteDriver"
# )
#
# logindata <- builder$build()
# conns <- DSI::datashield.login(logins = logindata, assign = FALSE)
#
# datashield.assign.table(
#   conns = conns,
#   table = "mtcars",
#   symbol = "mtcars"
# )
#
# datashield.assign.table(
#   conns = conns,
#   table = "mtcars_group",
#   symbol = "mtcars_group"
# )
#
# good_arrange_arg <- "mpg, cyl"
#
# test_that("arrangeDS correctly arranges data", {
#   good_arrange_cally <- .make_tidyverse_call("mtcars", "arrange", good_arrange_arg)
#
#   sorted_df <- eval(good_arrange_cally)
#
#   expect_equal(
#     class(sorted_df),
#     "data.frame"
#   )
#
#   expect_equal(
#     sorted_df$mpg,
#     c(10.4, 10.4, 13.3, 14.3, 14.7, 15.0, 15.2, 15.2, 15.5, 15.8, 16.4, 17.3, 17.8, 18.1, 18.7,
#       19.2, 19.2, 19.7, 21.0, 21.0, 21.4, 21.4, 21.5, 22.8, 22.8, 24.4, 26.0, 27.3, 30.4, 30.4,
#       32.4, 33.9)
#     )
#
#   expect_equal(
#     sorted_df$cyl,
#     c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 6, 8, 6, 8, 6, 6, 6, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 4,
#       4)
#     )
# })
#
#
# test_that("arrangeDS works with .by_group argument", {
#
#   by_cally <- .make_tidyverse_call("mtcars_group", "arrange", "drat", ".by_group = TRUE")
#   no_by_cally <- .make_tidyverse_call("mtcars_group", "arrange", "drat", ".by_group = FALSE")
#
#   arrange_by_t <- eval(by_cally)
#   arrange_by_f <- eval(no_by_cally)
#
#   expect_equal(
#     class(arrange_by_t),
#     c("grouped_df", "tbl_df", "tbl", "data.frame")
#   )
#
#   expect_equal(
#     class(arrange_by_f),
#     c("grouped_df", "tbl_df", "tbl", "data.frame")
#   )
#
#   expect_equal(
#     arrange_by_t$drat,
#     c(3.69, 3.70, 3.77, 3.85, 3.92, 4.08, 4.08, 4.11, 4.22, 4.43, 4.93, 2.76, 3.08, 3.62, 3.90,
#     3.90, 3.92, 3.92, 2.76, 2.93, 3.00, 3.07, 3.07, 3.07, 3.08, 3.15, 3.15, 3.21, 3.23, 3.54,
#     3.73, 4.22)
#   )
#
#   expect_equal(
#     arrange_by_f$drat,
#     c(2.76, 2.76, 2.93, 3.00, 3.07, 3.07, 3.07, 3.08, 3.08, 3.15, 3.15, 3.21, 3.23, 3.54, 3.62,
#     3.69, 3.70, 3.73, 3.77, 3.85, 3.90, 3.90, 3.92, 3.92, 3.92, 4.08, 4.08, 4.11, 4.22, 4.22,
#     4.43, 4.93)
#   )
# })
#
# test_that("arrangeDS works with desc option", {
#   desc_arrange_arg <- "list(desc(mpg))"
#   desc_arrange_cally <- .make_tidyverse_call("mtcars", "arrange", desc_arrange_arg)
#
#   desc_df <- eval(desc_arrange_cally)
#
#   expect_equal(
#     class(desc_df),
#     "data.frame"
#   )
#
#   expect_equal(
#     desc_df$mpg,
#     c(21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,
#       16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5,
#       15.5, 15.2, 13.3, 19.2, 27.3, 26.0, 30.4, 15.8, 19.7, 15.0, 21.4)
#   )
# })
#
# test_that("arrangeDS fails when data doesn't exist", {
#   no_data <- .make_tidyverse_call("doesnt_exist", "arrange", good_arrange_arg)
#   expect_error(
#     eval(no_data),
#     "object 'doesnt_exist' not found"
#   )
# })
#
# test_that("arrangeDS passes when called directly", {
#   cally <- call("arrangeDS", "drat", "mtcars", NULL)
#   datashield.assign(conns, "sorted_df", cally)
#
#   expect_equal(
#     ds.class("sorted_df")[[1]],
#     "data.frame"
#     )
#
#   expect_equal(
#     ds.dim("sorted_df")[[1]],
#     c(32, 11)
#   )
# })
#
# test_that("arrangeDS works with desc option when called directly", {
#   cally <- call("arrangeDS", "desc$LB$drat$RB$", "mtcars", NULL)
#   datashield.assign(conns, "sorted_df", cally)
#
#   expect_equal(
#     ds.class("sorted_df")[[1]],
#     "data.frame"
#   )
#
#   expect_equal(
#     ds.dim("sorted_df")[[1]],
#     c(32, 11)
#   )
# })
