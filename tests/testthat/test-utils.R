library(stringr)

test_that(".getEncodeKey returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("(", ")", "\"", ",", " ", ":", "!", "&", "|", "'", "[", "]", "="),
    output = c("$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$APO$", "$LSQ$", "$RSQ", "$EQU$")
  )
  actual_encode_list <- .getEncodeKey()
  expect_equal(actual_encode_list, expected_encode_list)
})

test_that(".getEncodeKey returns the expected encoding key", {
  expected_encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|", "=="),
    output = c("$LIST$", "$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$EQUALS$")
  )
  actual_encode_list <- .getEncodeKey()
  expect_false(isTRUE(all.equal(expected_encode_list, actual_encode_list)))
})

expected_string <- "asd, qwe, starts_with('test')"

test_that(".decode_tidy_eval correctly decodes an encoded string passed via the R parser", {
  encoded_string <- "asd$COMMA$$SPACE$qwe$COMMA$$SPACE$starts_with$LB$'test'$RB$"
  decoded_string <- .decode_tidy_eval(encoded_string, .getEncodeKey())
  expect_equal(decoded_string, expected_string)
})


test_that(".make_tidyselect_arg passes if select is passed", {
  select_good_arg <- "mpg, cyl, starts_with('g'), ends_with('b')"
  expect_silent(
    .execute_tidyverse_function("mtcars", "select", select_good_arg)
  )
})

test_that(".make_tidyselect_arg passes if select is passed", {
  rename_good_arg <- "new_name_1 = mpg, new_name_2 = drat"
  expect_silent(
    .execute_tidyverse_function("mtcars", "rename", rename_good_arg)
  )
})
