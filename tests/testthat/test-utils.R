library(stringr)
library(dplyr)
library(DSLite)
library(rlang)

test_that(".make_tidyverse_call creates argument to pass to `eval_tidy", {
  input_string <- "asd, qwe, starts_with('test')"
  expected_string <- rlang::parse_expr("test %>% dplyr::select(asd, qwe, starts_with('test'))")
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string)
  expect_equal(expected_string, observed_string)

  expected_string <- rlang::parse_expr("test %>% dplyr::select(asd, qwe, starts_with('test'), arg_1,
    arg_2)")
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string, other_args = list("arg_1", "arg_2"))
  expect_equal(expected_string, observed_string)
})

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_good_str <- .make_tidyverse_call(.data = "mtcars", fun = "select", tidy_select = mtcars_good_arg)

test_that(".execute_with_error_handling works where data and object exists", {
  observed <- .execute_with_error_handling("select", mtcars_good_str)
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))
})

mtcars_wrong_data_str <- .make_tidyverse_call(.data = "data_not_here", fun = "select", tidy_select = mtcars_good_arg)

test_that(".execute_with_error_handling fails with correct message if object doesn't exist", {
  expect_error(
    .execute_with_error_handling("select", mtcars_wrong_data_str),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_wrong_data_expr'\\s+not\\s+found"
  )
})

mtcars_missing_col_arg <- "all_of('test_col'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_missing_col_str <- .make_tidyverse_call(.data = "mtcars", fun = "select", tidy_select = mtcars_missing_col_arg)

test_that(".execute_with_error_handling fails with correct message if column doesn't exist", {
  expect_error(
    .execute_with_error_handling("select", mtcars_missing_col_str),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_missing_col_expr'\\s+not\\s+found"
  )
})

mtcars_random_arg <- "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_random_str <- .make_tidyverse_call(.data = "mtcars", fun = "select", tidy_select = mtcars_random_arg)

test_that(".execute_with_error_handling fails with correct message when unrecognised function passed", {
  expect_error(
    .execute_with_error_handling("select", mtcars_random_str),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_random_expr'\\s+not\\s+found"
  )
})

test_that(".get_encode_dictionary returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("(", ")", "\"", ",", " ", ":", "!", "&", "|", "'", "[", "]", "=", "+", "-", "*", "/", "^"),
    output = c("$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$APO$", "$LSQ$", "$RSQ", "$EQU$", "$ADD$", "$SUB$", "$MULT$",
               "$DIVIDE$", "$POWER$")
  )
  actual_encode_list <- .get_encode_dictionary()
  expect_equal(actual_encode_list, expected_encode_list)
})

test_that(".get_encode_dictionary returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|", "=="),
    output = c("$LIST$", "$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$EQUALS$")
  )
  actual_encode_list <- .get_encode_dictionary()
  expect_false(isTRUE(all.equal(expected_encode_list, actual_encode_list)))
})

expected_string <- "asd, qwe, starts_with('test')"

test_that(".decode_tidy_eval correctly decodes an encoded string passed via the R parser", {
  encoded_string <- "asd$COMMA$$SPACE$qwe$COMMA$$SPACE$starts_with$LB$'test'$RB$"
  decoded_string <- .decode_tidy_eval(encoded_string, .get_encode_dictionary())
  expect_equal(decoded_string, expected_string)
})

test_that(".execute_with_error_handling passes if additional arguments are included", {
  mutate_extra_args <- .make_tidyverse_call("mtcars", "mutate", "new_name_1 = mpg, new_name_2 = drat", list('all', 'disp', NULL))
  observed <- .execute_with_error_handling("mutate", mutate_extra_args)
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))

  )
})

input_string <- "asd, qwe, starts_with('test')"
expected_string <- rlang::parse_expr("test %>% dplyr::select(asd, qwe, starts_with('test'))")
observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string)



options(datashield.env = environment())
logindata.dslite.cnsim <- setupCNSIMTest()
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = F)

test_that(".set_privacy_option works with or without values specified in options", {

  expect_equal(
    .set_privacy_option("datashield.privacyControlLevel"),
    "banana")

  options(datashield.privacyControlLevel = "test")
  expect_equal(
    .set_privacy_option("datashield.privacyControlLevel"),
    "test")

  expect_equal(
    .set_privacy_option("nfilter.stringShort"),
    20)

  options(nfilter.stringShort = 100)
  expect_equal(
    .set_privacy_option("nfilter.stringShort"),
    100)

}
)

test_that(".list_privacy_settings returns correct string", {
  expect_equal(
    .list_privacy_settings(),
    c(
      "datashield.privacyControlLevel", "nfilter.tab", "nfilter.subset",
      "nfilter.glm", "nfilter.string", "nfilter.stringShort", "nfilter.kNN",
      "nfilter.levels.density", "nfilter.levels.max", "nfilter.noise"
    )
  )
})

test_that("dsListDisclosureSettingsTidyVerse returns correct settings", {

options(datashield.privacyControlLevel = "test_privacy")
options(nfilter.tab = 9)
options(nfilter.subset = 9)
options(nfilter.glm = 0.99)
options(nfilter.string = 99)
options(nfilter.stringShort = 19)
options(nfilter.kNN = 9)
options(nfilter.levels.density = 0.99)
options(nfilter.levels.max = 29)
options(nfilter.noise = 0.99)

expected <- list(
  datashield.privacyControlLevel = "test_privacy",
  nfilter.tab = 9,
  nfilter.subset = 9,
  nfilter.glm = 0.99,
  nfilter.string = 99,
  nfilter.stringShort = 19,
  nfilter.kNN = 9,
  nfilter.levels.density = 0.99,
  nfilter.levels.max = 29,
  nfilter.noise = 0.99)

expect_equal(dsListDisclosureSettingsTidyVerse(), expected)
})

## Add tests where other_args != NULL

