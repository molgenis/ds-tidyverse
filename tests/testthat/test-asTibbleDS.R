library(DSLite)
library(dplyr)
library(dsTidyverse)
library(dsBase)
library(dsBaseClient)
library(DSI)

options(datashield.env = environment())
data("mtcars")
mtcars_dup_names <- cbind(mtcars, tibble(cyl = 2))

test_matrix <- matrix(data = 1:20, ncol = 4)

dslite.server <- newDSLiteServer(
  tables = list(
    mtcars = mtcars,
    mtcars_dup_names = mtcars_dup_names,
    test_matrix = test_matrix
  )
)

dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("asTibbleDS", "asTibbleDS")
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
  table = "mtcars_dup_names",
  symbol = "mtcars_dup_names"
)

datashield.assign.table(
  conns = conns,
  table = "test_matrix",
  symbol = "test_matrix"
)

tibble_class <- c("tbl_df", "tbl", "data.frame")

test_that("asTibbleDS correctly converts a data frame to a tibble", {
  good_tibble_cally <- .make_tidyverse_call("mtcars", "as_tibble", tidy_select = NULL)

  now_a_tibble <- eval(good_tibble_cally)

  expect_equal(
    class(now_a_tibble),
    tibble_class
  )

  expect_equal(
    dim(now_a_tibble),
    c(32, 11)
  )

})

test_that("asTibbleDS correctly converts a matrix to a tibble", {
  good_matrix_cally <- .make_tidyverse_call("test_matrix", "as_tibble", tidy_select = NULL, ".name_repair = 'minimal'")

  matrix_to_tibble <- eval(good_matrix_cally)

  expect_equal(
    class(matrix_to_tibble),
    tibble_class
  )

  expect_equal(
    dim(matrix_to_tibble),
    c(5, 4)
  )

})

test_that("asTibbleDS fails when data doesn't exist", {
  no_data <- .make_tidyverse_call("doesnt_exist", "as_tibble", tidy_select = NULL)
  expect_error(
    eval(no_data),
    "object 'doesnt_exist' not found"
  )
})


test_that("asTibbleDS works with the name_repair argument", {

  repair_min_call <- .make_tidyverse_call("mtcars_dup_names", "as_tibble", tidy_select = NULL, '.name_repair = "minimal"')
  tib_min_repair <- eval(repair_min_call)

  expect_equal(
    class(tib_min_repair),
    tibble_class
  )

  expect_equal(
    colnames(tib_min_repair),
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "cyl")
  )

  repair_unique_call <- .make_tidyverse_call("mtcars_dup_names", "as_tibble", tidy_select = NULL, '.name_repair = "unique"')

  expect_message(
    tib_unique_repair <- eval(repair_unique_call)
  )

  expect_equal(
    class(tib_unique_repair),
    tibble_class
  )

  expect_equal(
    colnames(tib_unique_repair),
    c("mpg", "cyl...2", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "cyl...12")
  )

  repair_check_call <- .make_tidyverse_call("mtcars_dup_names", "as_tibble", tidy_select = NULL, '.name_repair = "check_unique"')

  expect_error(
    tib_unique_repair <- eval(repair_check_call)
  )

  repair_uni_call <- .make_tidyverse_call("mtcars_dup_names", "as_tibble", tidy_select = NULL, '.name_repair = "universal"')

  expect_message(
    tib_uni_repair <- eval(repair_uni_call)
  )

  expect_equal(
    class(tib_uni_repair),
    tibble_class
  )

  expect_equal(
    colnames(tib_uni_repair),
    c("mpg", "cyl...2", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "cyl...12")
  )

})

test_that("asTibbleDS works with the rownames argument", {

  row_null_call <- .make_tidyverse_call("mtcars", "as_tibble", tidy_select = NULL, 'rownames = NULL')
  row_null_tibble <- eval(row_null_call)

  expect_equal(
    class(row_null_tibble),
    tibble_class
  )

  expect_equal(
    rownames(row_null_tibble),
    as.character(rep(1:32))
  )

  row_na_call <- .make_tidyverse_call("mtcars", "as_tibble", tidy_select = NULL, 'rownames = NA')
  row_na_tibble <- eval(row_na_call)

  expect_equal(
    class(row_na_tibble),
    tibble_class
  )

  car_names <- c(
    "Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive",
    "Hornet Sportabout", "Valiant", "Duster 360", "Merc 240D", "Merc 230",
    "Merc 280", "Merc 280C", "Merc 450SE", "Merc 450SL", "Merc 450SLC",
    "Cadillac Fleetwood", "Lincoln Continental", "Chrysler Imperial",
    "Fiat 128", "Honda Civic", "Toyota Corolla", "Toyota Corona",
    "Dodge Challenger", "AMC Javelin", "Camaro Z28", "Pontiac Firebird",
    "Fiat X1-9", "Porsche 914-2", "Lotus Europa", "Ford Pantera L",
    "Ferrari Dino", "Maserati Bora", "Volvo 142E"
  )

  expect_equal(
    rownames(row_na_tibble),
    car_names
  )

  row_col_call <- .make_tidyverse_call("mtcars", "as_tibble", tidy_select = NULL, 'rownames = "col_with_names"')
  tib_with_col <- eval(row_col_call)

  expect_equal(
    class(tib_with_col),
    tibble_class
  )

  expect_equal(
    colnames(tib_with_col),
    c(
      "col_with_names", "mpg", "cyl", "disp", "hp", "drat",
      "wt", "qsec", "vs", "am", "gear", "carb"
    )
  )

  expect_equal(
    tib_with_col$col_with_names,
    car_names
  )

})

test_that("asTibbleDS passes when called directly", {
  cally <- call("asTibbleDS", NULL, "mtcars", NULL, "minimal", NULL)
  datashield.assign(conns, "new_tibble", cally)

  expect_equal(
    ds.class("new_tibble")[[1]],
    tibble_class
    )

  expect_equal(
    ds.colnames("new_tibble")[[1]],
    c(
      "mpg", "cyl", "disp", "hp", "drat",
      "wt", "qsec", "vs", "am", "gear", "carb"
    )
  )

  expect_equal(
    ds.dim("new_tibble")[[1]],
    c(32, 11)
  )
})
