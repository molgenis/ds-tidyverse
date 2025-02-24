require(DSI)
require(DSLite)
require(dplyr)
require(dsBase)
require(dsBaseClient)

data("mtcars")
mtcars_group <- mtcars %>% group_by(cyl)
login_data <- .prepare_dslite("mutateDS", NULL, list(mtcars = mtcars))
conns <- datashield.login(logins = login_data)
datashield.assign.table(conns, "mtcars", "mtcars")

good_mutate_arg <- "mpg_trans = cyl*1000, new_var = (hp-drat)/qsec"
# good_mutate_arg_enc <- .encode_tidy_eval(good_mutate_arg, .get_encode_dictionary())

test_that("mutateDS passes where data and column exist", {
  good_mutate_cally <- .make_tidyverse_call("mtcars", "mutate", good_mutate_arg, list(".keep = \"all\", .before = NULL, .after = NULL"))
  result <- eval(good_mutate_cally)

  expect_equal(
    mean(result$mpg_trans),
    6187.5
  )

  expect_equal(
    colnames(result),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "mpg_trans", "new_var")
  )
})

test_that("mutateDS passes with different .keep argument", {
  good_mutate_cally <- .make_tidyverse_call("mtcars", "mutate", good_mutate_arg, list(".keep = \"none\", .before = NULL, .after = NULL"))
  result <- eval(good_mutate_cally)

  expect_equal(
    colnames(result),
    c("mpg_trans", "new_var")
  )
})

test_that("mutateDS passes with different .before argument", {
  good_mutate_cally <- .make_tidyverse_call("mtcars", "mutate", good_mutate_arg, list(".keep = \"all\", .before = \"disp\", .after = NULL"))
  result <- eval(good_mutate_cally)

  expect_equal(
    colnames(result),
    c("mpg", "cyl", "mpg_trans", "new_var", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})

test_that("mutateDS passes with different .afterb argument", {
  good_mutate_cally <- .make_tidyverse_call("mtcars", "mutate", good_mutate_arg, list(".keep = \"all\", .before = NULL, .after = \"qsec\""))
  result <- eval(good_mutate_cally)

  expect_equal(
    colnames(result),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "mpg_trans", "new_var", "vs", "am", "gear", "carb")
  )
})

test_that("mutateDS passes when called directly", {
  skip_if_not_installed("dsBaseClient")
  cally <- call(
    "mutateDS", "new_var$SPACE$$EQU$$SPACE$mpg$SPACE$$MULT$$SPACE$1000$COMMA$$SPACE$new_var2$SPACE$$EQU$$SPACE$hp$SPACE$$SUB$$SPACE$drat",
    "mtcars", "all", NULL, NULL
  )

  datashield.assign(conns, "test", cally)

  expect_equal(
    ds.class("test", datasources = conns)[[1]],
    "data.frame"
  )

  expect_equal(
    ds.colnames("test", datasources = conns)[[1]],
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "new_var", "new_var2")
  )

  expect_equal(
    ds.mean("test$new_var", datasources = conns)$Mean.by.Study[1, 1],
    20090.625
  )

  expect_equal(
    round(ds.mean("test$new_var2", datasources = conns)$Mean.by.Study[1, 1], 2),
    143.09
  )
})

test_that("mutateDS doesn't work with banned function calls that could create a vector", {
  banned_arg_1 <- "banned = row_number()"
  expect_error(
    .check_tidy_disclosure("mtcars", banned_arg_1),
    "`row_number` is not a permitted function")

  banned_arg_2 <- "banned = seq(1, 32, 1)"
  expect_error(
    .check_tidy_disclosure("mtcars", banned_arg_2),
    "`seq` is not a permitted function")

  banned_arg_3 <- "banned = rep(1, 32)"
  expect_error(
    .check_tidy_disclosure("mtcars", banned_arg_3),
    "`rep` is not a permitted function")
})

test_that(".check_mutate_disclosure blocks the use of ':'", {
  banned_arg_4 <- "banned = 1:32"
  expect_error(
    .check_mutate_disclosure(banned_arg_4),
    "It is not permitted to use the character ':' within ds.mutate."
  )
})

test_that(".check_mutate_disclosure blocks the use of 'c'", {
  banned_arg_5 <- "banned = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)"
  expect_error(
    .check_mutate_disclosure(banned_arg_5),
    "It is not permitted to use the character 'c\\(' within ds.mutate")
})

test_that(".check_mutate_disclosure blocks the use of both 'c' and ':'", {
  banned_arg_6 <- "banned = c(1, 2, 3) + 1:32"
  expect_error(
    .check_mutate_disclosure(banned_arg_6),
    "It is not permitted to use the characters 'c\\( and :' within ds.mutate")
})

test_that(".check_mutate_disclosure doesn't block permitted strings", {
  good_arg_1 <- "ok^2"
  expect_silent(
    .check_mutate_disclosure(good_arg_1)
  )
})


