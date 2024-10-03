# .check_data_name_length throws an error if length of .data exceeds nfilter.string

    Code
      .check_data_name_length(.data, disc_settings)
    Condition
      Error in `if (data_length > disclosure$nfilter.string) ...`:
      ! argument is of length zero

# .check_function_names blocks unpermitted function names

    Code
      .check_function_names(arg_unpermitted)
    Condition
      Error:
      ! Values passed to `expr` may only contain permitted functions.
      i Permitted functions are everything, last_col, group_cols, starts_with, ends_with, contains, matches, num_range, all_of, any_of, where, c, rename, mutate, if_else, case_when, mean, median, ..., diff, and lag.
      i `filter and slice` are not permitted functions.

# .check_variable_length blocks variables with value greater than than nfilter.string

    Code
      .check_variable_length(large_var, disc_settings)
    Condition
      Error in `map_lgl()`:
      i In index: 1.
      Caused by error:
      ! Result must be length 1, not 0.

# .tidy_disclosure_checks blocks argument with unpermitted variable length

    Code
      .check_tidy_disclosure("dataframe", arg_unpermitted_2)
    Condition
      Error in `if (data_length > disclosure$nfilter.string) ...`:
      ! argument is of length zero

# .tidy_disclosure_checks blocks argument with multiple unpermitted function names

    Code
      .check_tidy_disclosure("dataset", arg_unpermitted_3)
    Condition
      Error in `if (data_length > disclosure$nfilter.string) ...`:
      ! argument is of length zero

# .tidy_disclosure_checks blocks argument with single unpermitted function name

    Code
      .check_tidy_disclosure("dataset", arg_unpermitted_4)
    Condition
      Error in `if (data_length > disclosure$nfilter.string) ...`:
      ! argument is of length zero

