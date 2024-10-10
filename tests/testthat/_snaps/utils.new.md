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
      Error in `.check_function_names()`:
      ! non-interactive browser() -- left over from debugging?

# .check_variable_length blocks variables with value greater than than nfilter.string

    Code
      .check_variable_length(large_var, disc_settings)
    Condition
      Error in `variable_names[sapply(variable_lengths, function(x) x > threshold)]`:
      ! invalid subscript type 'list'

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

