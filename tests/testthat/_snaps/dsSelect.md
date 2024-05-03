# .make_tidyselect_arg fails with correct message if attempt to use non permitted tidyverse serverside command

    Code
      .execute_tidyverse_function("mtcars", "filter", mtcars_random_arg)
    Condition
      Error:
      ! You must only use permitted tidyverse functions within DataSHIELD
      i Permitted functions are select
      x You have attempted to pass filter

# .tidy_eval_handle_errors fails with correct message if object doesn't exist

    Code
      .tidy_eval_handle_errors(mtcars_wrong_data_expr, "data_not_here")
    Condition
      Error:
      x `selectDS` returned the following error:
      i object 'data_not_here' not found

# .tidy_eval_handle_errors fails with correct message if column doesn't exist

    Code
      .tidy_eval_handle_errors(mtcars_missing_col_expr, "mtcars")
    Condition
      Error:
      x `selectDS` returned the following error:
      i i In argument: `all_of("test_col")`. Caused by error in `all_of()`: ! Can't subset elements that don't exist. x Element `test_col` doesn't exist.

# .tidy_eval_handle_errors fails with correct message when unrecognised function passed

    Code
      .tidy_eval_handle_errors(mtcars_random_expr, "mtcars")
    Condition
      Error:
      x `selectDS` returned the following error:
      i i In argument: `filter("mpg")`. Caused by error in `UseMethod()`: ! no applicable method for 'filter' applied to an object of class "character"

# .execute_tidyverse_function fails with correct message when data doesn't exist

    Code
      .execute_tidyverse_function("data_not_there", "select", mtcars_good_arg)
    Condition
      Error:
      x `selectDS` returned the following error:
      i object 'data_not_there' not found

# .execute_tidyverse_function fails with correct message when unrecognised function passed

    Code
      .execute_tidyverse_function("mtcars", "select", mtcars_random_arg)
    Condition
      Error:
      x `selectDS` returned the following error:
      i i In argument: `filter("mpg")`. Caused by error in `UseMethod()`: ! no applicable method for 'filter' applied to an object of class "character"

