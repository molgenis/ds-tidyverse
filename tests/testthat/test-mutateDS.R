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
