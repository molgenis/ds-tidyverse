library(stringr)
library(dplyr)
library(DSLite)
library(rlang)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "selectDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

disc_settings <- listDisclosureSettingsDS()

test_that(".make_tidyverse_call creates call with no additional arguments", {
  input_string <- "asd, qwe, starts_with('test')"
  expected_string <- rlang::parse_expr('test |> dplyr::select(asd, qwe, starts_with("test"))')
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string)
  expect_equal(expected_string, observed_string)
})

extra_args <- c(".keep = \"all\", .before = NULL, .after = \"disp\"")
input_string <- "asd, qwe, starts_with('test')"

test_that(".make_tidyverse_call creates call with additional arguments", {
  expected_string <- rlang::parse_expr('test |> dplyr::select(asd, qwe, starts_with("test"), .keep = "all",
    .before = NULL, .after = "disp")')
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string, other_args = extra_args)
  expect_equal(expected_string, observed_string)
})

test_that(".make_tidyverse_call creates call when inc_data = F", {
  expected_string <- rlang::parse_expr('dplyr::select(asd, qwe, starts_with("test"), .keep = "all",
  .before = NULL, .after = "disp")')
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string, other_args = extra_args, inc_data = F)
  expect_equal(expected_string, observed_string)
})

test_that(".make_tidyverse_call creates call when inc_data = F", {
  expected_string <- rlang::parse_expr('dplyr::select(asd, qwe, starts_with("test"), .keep = "all",
  .before = NULL, .after = "disp")')
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string, other_args = extra_args, inc_data = F)
  expect_equal(expected_string, observed_string)
})

test_that(".make_tidyverse_call creates call when inc_data = F", {
  expected_string <- rlang::parse_expr('dplyr::select(asd, qwe, starts_with("test"), .keep = "all",
  .before = NULL, .after = "disp")')
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string, other_args = extra_args, inc_data = F)
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

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_good_expr <- .make_tidyverse_call(.data = "mtcars", fun = "select", tidy_select = mtcars_good_arg)
# mtcars_good_expr <- rlang::parse_expr(mtcars_good_str)

test_that(".tidy_eval_handle_errors works where data and object exists", {
  observed <- .execute_with_error_handling("select", mtcars_good_expr)
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))
})

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_wrong_data_expr <- .make_tidyverse_call(.data = "data_not_here", fun = "select", tidy_select = mtcars_good_arg)

test_that(".tidy_eval_handle_errors fails with correct message if object doesn't exist", {
  expect_error(
    .execute_with_error_handling("select", mtcars_wrong_data_expr),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_wrong_data_expr'\\s+not\\s+found"
  )
})

mtcars_missing_col_arg <- "all_of('test_col'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_missing_col_expr <- .make_tidyverse_call(.data = "mtcars", fun = "select", tidy_select = mtcars_missing_col_arg)

test_that(".tidy_eval_handle_errors fails with correct message if column doesn't exist", {
  expect_error(
    .execute_with_error_handling("select", mtcars_missing_col_expr),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_missing_col_expr'\\s+not\\s+found"
  )
})

mtcars_random_arg <- "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_random_expr <- .make_tidyverse_call(.data = "mtcars", fun = "select", tidy_select = mtcars_random_arg)

test_that(".tidy_eval_handle_errors fails with correct message when unrecognised function passed", {
  expect_error(
    .execute_with_error_handling("select", mtcars_random_expr),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_random_expr'\\s+not\\s+found"
  )
})

test_that(".get_encode_dictionary returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("(", ")", "\"", ",", " ", ":", "!", "&", "|", "'", "[", "]", "=", "+", "-", "*", "/", "^", ">", "<", "~", "\n"),
    output = c(
      "$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$APO$", "$LSQ$", "$RSQ", "$EQU$", "$ADD$", "$SUB$", "$MULT$",
      "$DIVIDE$", "$POWER$", "$GT$", "$LT$", "$TILDE$", "$LINE$"
    )
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
  mutate_extra_args <- .make_tidyverse_call("mtcars", "mutate", "new_name_1 = mpg, new_name_2 = drat", ".keep = 'all', .before = NULL, .after = \"disp\"")
  observed <- .execute_with_error_handling("mutate", mutate_extra_args)
  expect_equal(colnames(observed), c("mpg", "cyl", "disp", "new_name_1", "new_name_2", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
})

test_that(".execute_with_error_handling passes if all additional arguments are NULL", {
  mutate_null_args <- .make_tidyverse_call("mtcars", "mutate", "new_name_1 = mpg, new_name_2 = drat", ".keep = 'all', .before = NULL, .after = NULL")
  observed <- .execute_with_error_handling("mutate", mutate_null_args)
  expect_equal(colnames(observed), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "new_name_1", "new_name_2"))
})

test_that(".paste_character_args creates correct string", {
  true <- "high"
  false <- "low"
  missing <- NULL
  ptype <- NULL
  size <- NULL

  expected <- "true = \"high\", false = \"low\", missing = NULL, ptype = NULL, size = NULL"
  expect_equal(
    .paste_character_args(true, false, missing, ptype, size),
    expected
  )
})

test_that(".get_nfilter_subset_value retrieves value correctly", {
  expect_equal(
    .get_nfilter_subset_value(),
    3)
})

test_that(".get_dimensions correctly returns dimensions", {
  original <- tibble(a = 1:3, b = 4:6)
  out <- tibble(a = 1:2, b = 1:2)

  returned <- .get_dimensions(original, out)

  expect_equal(
    class(returned),
    "list"
  )

  expect_equal(
    returned,
    list(original = 3, subset = 2)
  )
})

test_that(".check_subset_size returns an error if subset is too small", {
  expect_error(
    .check_subset_size(subset_rows = 3, nfilter.subset = 10),
    ".*Subset to be created is too small \\(< nfilter\\.subset\\).*"
  )
})

test_that(".check_subset_size doesn't return an error if subset is large enough", {
  expect_silent(
    .check_subset_size(subset_rows = 20, nfilter.subset = 2)
  )
})

test_that(".check_rows_compared_with_original returns an error correctly", {
  expect_error(
    .check_rows_compared_with_original(10, 9, 3),
    "The difference in row length between the original dataframe and the new dataframe"
  )
})

test_that(".check_rows_compared_with_original doesn't return an error if different in dimensions is large enough", {
  expect_silent(
    .check_rows_compared_with_original(100, 80, 3)
  )
})

test_that(".check_subset_disclosure_risk returns an error correctly", {
  original <- tibble(a = 1:3, b = 4:6)
  out <- tibble(a = 1:2, b = 1:2)

  expect_error(
    .check_subset_disclosure_risk(original, out),
    ".*Subset to be created is too small \\(< nfilter\\.subset\\).*")

  original <- tibble(a = 1:10, b = 1:10)
  out <- tibble(a = 1:9, b = 1:9)

  expect_error(
    .check_subset_disclosure_risk(original, out),
    "The difference in row length between the original dataframe and the new dataframe"
  )

})

test_that(".check_subset_disclosure_risk doesn't return errors if subset sizes are ok", {
  original <- tibble(a = 1:10, b = 1:10)
  out <- tibble(a = 1:4, b = 1:4)

  expect_silent(
    .check_subset_disclosure_risk(original, out)
  )

  original <- tibble(a = 1:10, b = 1:10)
  out <- tibble(a = 1:5, b = 1:5)

  expect_silent(
    .check_subset_disclosure_risk(original, out)
  )

})

test_that(".check_data_name_length throws an error if length of .data exceeds nfilter.string", {
  .data <- paste(rep("a", 101), collapse = "")
  expect_snapshot(.check_data_name_length(.data, disc_settings), error = TRUE)
})

test_that(".check_data_name_length does not throw an error if length of .data is within nfilter.string", {
  .data <- paste(rep("a", 79), collapse = "")
  expect_silent(.check_data_name_length(.data, disc_settings))
})

arg_permitted <- "asd, sdf, dfg, everything(), starts_with(\"A\"), ends_with(\"Z\")"
arg_unpermitted <- "asd, sdf, dfg, everything(), filter(test == 2), slice(3), mutate(new_name = old_name), starts_with(\"A\"), ends_with(\"Z\")"
small_var <- paste(rep("a", 5), collapse = "")
large_var <- paste(rep("a", 200), collapse = "")

test_that(".check_function_names allows permitted names to pass", {
  expect_silent(.check_function_names(arg_permitted))
})

test_that(".check_function_names blocks unpermitted function names", {
  arg_unpermitted
  expect_snapshot(
    .check_function_names(arg_unpermitted),
    error = TRUE
  )
})

test_that(".check_variable_length allows variables with value less than nfilter.string", {

  expect_silent(
    .check_variable_length(small_var, disc_settings)
  )
})

test_that(".check_variable_length blocks variables with value greater than than nfilter.string", {
  expect_snapshot(
    .check_variable_length(large_var, disc_settings),
    error = TRUE
  )
})

test_that(".tidy_disclosure_checks allows permitted arg to pass", {
  expect_silent(.check_tidy_disclosure("dataframe", arg_permitted))
})

test_that(".tidy_disclosure_checks blocks argument with unpermitted variable length", {
  arg_unpermitted_2 <- paste0(large_var, arg_permitted)
  expect_snapshot(
    .check_tidy_disclosure("dataframe", arg_unpermitted_2),
    error = TRUE
  )
})

test_that(".tidy_disclosure_checks blocks argument with unpermitted function names", {
  arg_unpermitted_3 <- arg_unpermitted
  expect_snapshot(
    .check_tidy_disclosure("dataset", arg_unpermitted_3),
    error = TRUE
  )
})
