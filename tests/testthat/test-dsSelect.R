library(testthat)

test_string <- function(string) {
  select_expr <- rlang::parse_exprs(string)
  out <- mtcars %>% select(!!!select_expr)
  return(out)
}

test_that("String with : works in select", {
  out <- test_string("mpg:drat")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "hp", "drat"))
})

test_that("String with 'starts_with()' works", {
  out <- test_string("starts_with('m')")
  expect_equal(colnames(out), "mpg")
})

test_that("String with 'ends_with()' works", {
  out <- test_string("ends_with('m')")
  expect_equal(colnames(out), "am")
})

test_that("String with 'matches()' works", {
  out <- test_string("matches('[aeiou]')")
  expect_equal(colnames(out), c("disp", "drat", "qsec", "am", "gear", "carb"))
})

test_that("String with 'everything()' works", {
  out <- test_string("everything()")
  expect_equal(colnames(out), colnames(mtcars))
})

test_that("String with 'last_col()' works", {
  out <- test_string("last_col()")
  expect_equal(colnames(out), "carb")
})

test_that("String with 'group_cols()' works", {
  out <- test_string("group_cols()")
  expect_equal(colnames(out), character(0))
})

test_that("String with 'contains()' works", {
  out <- test_string("contains('ra')")
  expect_equal(colnames(out), "drat")
})

test_that("Strings with '&' work", {
  out <- test_string("starts_with('c') & ends_with('b')")
  expect_equal(colnames(out), "carb")
})

test_that("String with '|' work", {
  out <- test_string("starts_with('c') | ends_with('b')")
  expect_equal(colnames(out), c("cyl", "carb"))
})

test_that("String with 'all_of' work", {
  out <- test_string("all_of(c('mpg','cyl'))")
  expect_equal(colnames(out), c("mpg", "cyl"))
})

test_that("String with 'any_of' work", {
  out <- test_string("all_of(c('mpg','cyl'))")
  expect_equal(colnames(out), c("mpg", "cyl"))
})

test_that("Complex strings work", {
  out <- test_string("(starts_with('c') & ends_with('b')) | contains('ra') | gear:carb")
  expect_equal(colnames(out), c("carb", "drat", "gear"))
})

test_that("String with '-' works", {
  out <- test_string("-mpg")
  expect_equal(colnames(out), c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
})

test_that("String with 'where()' works", {
  out <- test_string("where(is.numeric)")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
})


### FAIL
test_that("String with '= works", {
  out <- test_string("test = mpg")
  expect_equal(colnames(out), "mpg")
})

select_expr <- rlang::parse_exprs(c("test = mpg"))


select_expr <- rlang::parse_exprs(c("disp,drat,qsec,all_of(c('mpg','cyl'))"))
select_expr <- rlang::parse_exprs(c("disp,drat,qsec,all_of(c('mpg','cyl'))"))

dplyr::select(mtcars, disp, drat, qsec, all_of(c("mpg", "cyl")))

select_expr <- rlang::parse_exprs(c("c(disp,drat,qsec)"))
select_expr <- rlang::parse_exprs(c("disp,drat,qsec"))

test_that("String with simple column names works", {
  out <- test_string(c("c(disp,drat,qsec)"))
  expect_equal(colnames(out), c("disp", "drat", "qsec"))
})
