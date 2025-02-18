# .check_data_name_length throws an error if length of .data exceeds nfilter.string

    Code
      .check_data_name_length(.data, disc_settings)
    Condition
      Error in `.check_data_name_length()`:
      ! Error: The length of string passed to `df.name` must be less than nfilter.string.
      i The value of nfilter.string is:
      80
      i The length of `df.name` is:
      101

# .check_function_names blocks unpermitted function names

    Code
      .check_function_names(arg_unpermitted)
    Condition
      Error:
      ! Values passed to `expr` may only contain permitted functions.
      i Permitted functions are everything, last_col, group_cols, starts_with, ends_with, contains, matches, num_range, all_of, any_of, where, rename, mutate, if_else, case_when, mean, median, mode, ..., atan, and c.
      i `filter and slice` are not permitted functions.

# .check_variable_length blocks variables with value greater than than nfilter.string

    Code
      .check_variable_length(large_var, disc_settings)
    Condition
      Error:
      ! Error: The maximum length of columns specified in `tidy_select` must be shorter than nfilter.string.
      i The values of nfilter.string are:
      80
      i aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa is longer than this:

# .tidy_disclosure_checks blocks argument with unpermitted variable length

    Code
      .check_tidy_disclosure("dataframe", arg_unpermitted_2)
    Condition
      Error:
      ! Error: The maximum length of columns specified in `tidy_select` must be shorter than nfilter.string.
      i The values of nfilter.string are:
      80
      i aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaasd is longer than this:

# .tidy_disclosure_checks blocks argument with multiple unpermitted function names

    Code
      .check_tidy_disclosure("dataset", arg_unpermitted_3)
    Condition
      Error:
      ! Values passed to `expr` may only contain permitted functions.
      i Permitted functions are everything, last_col, group_cols, starts_with, ends_with, contains, matches, num_range, all_of, any_of, where, rename, mutate, if_else, case_when, mean, median, mode, ..., atan, and c.
      i `filter and slice` are not permitted functions.

# .tidy_disclosure_checks blocks argument with single unpermitted function name

    Code
      .check_tidy_disclosure("dataset", arg_unpermitted_4)
    Condition
      Error:
      ! Values passed to `expr` may only contain permitted functions.
      i Permitted functions are everything, last_col, group_cols, starts_with, ends_with, contains, matches, num_range, all_of, any_of, where, rename, mutate, if_else, case_when, mean, median, mode, ..., atan, and c.
      i `slice` is not a permitted function.

