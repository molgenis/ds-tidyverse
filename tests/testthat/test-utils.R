library(stringr)
library(dplyr)
library(DSLite)
library(rlang)

test_that(".make_tidyverse_call creates call with no additional arguments", {
  input_string <- "asd, qwe, starts_with('test')"
  expected_string <- rlang::parse_expr('test %>% dplyr::select(asd, qwe, starts_with("test"))')
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string)
  expect_equal(expected_string, observed_string)
})

extra_args <- c(".keep = \"all\", .before = NULL, .after = \"disp\"")
input_string <- "asd, qwe, starts_with('test')"

test_that(".make_tidyverse_call creates call with additional arguments", {
  expected_string <- rlang::parse_expr('test %>% dplyr::select(asd, qwe, starts_with("test"), .keep = "all",
    .before = NULL, .after = "disp")')
  observed_string <- .make_tidyverse_call(.data = "test", fun = "select", tidy_select = input_string, other_args = extra_args)
  expect_equal(expected_string, observed_string)

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

test_that(".make_tidyselect_arg creates argument to pass to `eval_tidy", {
  input_string <- "asd, qwe, starts_with('test')"
  expected_string <- "test %>% dplyr::select(asd, qwe, starts_with('test'))"
  observed_string <- .make_tidyselect_arg(.data = "test", fun = "select", tidy_select_args = input_string)
  expect_equal(expected_string, observed_string)
})

test_that(".make_tidyselect_arg fails with correct message if attempt to use non permitted tidyverse serverside command", {
  expect_error(
    .execute_tidyverse_function("mtcars", "filter", mtcars_random_arg),
    "\\b(permitted\\s+tidyverse\\s+functions\\s+within\\DataSHIELD)|Permitted\\s+functions\\s+are\\s+select|You\\s+have\\s+attempted\\s+to\\s+pass\\s+filter)\\b")
})

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_good_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_good_arg)
mtcars_good_expr <- rlang::parse_expr(mtcars_good_str)

test_that(".tidy_eval_handle_errors works where data and object exists", {
  observed <- .tidy_eval_handle_errors(mtcars_good_expr, "mtcars")
  expect_equal(colnames(observed), c("mpg", "cyl", "gear", "carb"))
})

mtcars_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_wrong_data_str <- .make_tidyselect_arg(.data = "data_not_here", fun = "select", tidy_select_args = mtcars_good_arg)
mtcars_wrong_data_expr <- rlang::parse_expr(mtcars_wrong_data_str)

test_that(".tidy_eval_handle_errors fails with correct message if object doesn't exist", {
  expect_error(
    .tidy_eval_handle_errors(mtcars_wrong_data_expr, "data_not_here"),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_wrong_data_expr'\\s+not\\s+found"
  )
})

mtcars_missing_col_arg <- "all_of('test_col'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_missing_col_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_missing_col_arg)
mtcars_missing_col_expr <- rlang::parse_expr(mtcars_missing_col_str)

test_that(".tidy_eval_handle_errors fails with correct message if column doesn't exist", {
  expect_error(
    .tidy_eval_handle_errors(mtcars_missing_col_expr, "mtcars"),
    "`selectDS`\\s+returned\\s+the\\s+following\\s+error:|object\\s+'mtcars_missing_col_expr'\\s+not\\s+found"
  )
})

mtcars_random_arg <- "filter('mpg'), mpg, cyl, starts_with('g'), ends_with('b')"
mtcars_random_str <- .make_tidyselect_arg(.data = "mtcars", fun = "select", tidy_select_args = mtcars_random_arg)
mtcars_random_expr <- rlang::parse_expr(mtcars_random_str)

test_that(".tidy_eval_handle_errors fails with correct message when unrecognised function passed", {
  expect_error(
    .tidy_eval_handle_errors(mtcars_random_expr, "mtcars"),
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

  true = "high"
  false = "low"
  missing = NULL
  ptype = NULL
  size = NULL

  expected <- "true = \"high\", false = \"low\", missing = NULL, ptype = NULL, size = NULL"
  expect_equal(
    .paste_character_args(true, false, missing, ptype, size),
    expected)
})
