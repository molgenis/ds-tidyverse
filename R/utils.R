#' Generate an encoding key which is used for encoding and decoding strings to pass the R parser
#'
#' @return A list containing the encoding key, with 'input' specifying the characters to be encoded
#' and 'output' specifying their corresponding encoded values.
#' @noRd
.get_encode_dictionary <- function() {
  encode_list <- list(
    input = c("(", ")", "\"", ",", " ", ":", "!", "&", "|", "'", "[", "]", "=", "+", "-", "*", "/", "^", ">", "<", "~", "\n"),
    output = c("$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$COLON$", "$EXCL$", "$AND$", "$OR$",
               "$APO$", "$LSQ$", "$RSQ", "$EQU$", "$ADD$", "$SUB$", "$MULT$", "$DIVIDE$", "$POWER$", "$GT$", "$LT$", "$TILDE$", "$LINE$")
  )
  return(encode_list)
}

#' Decode a string using the provided encoding key.
#'
#' @param input_string The encoded string passed through the R parser.
#' @param encode_key The encoding key generated by '.getEncodeKey()'.
#' @return The decoded string.
#' @importFrom stringr str_replace_all fixed
#' @importFrom rlang set_names
#' @noRd
.decode_tidy_eval <- function(input_string, encode_key) {
  encode_vec <- set_names(encode_key$input, encode_key$output)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
  return(output_string)
}

#' Create a tidyverse call expression
#'
#' This function creates a tidyverse call expression in string form,
#' using the provided data frame, function name, tidy select statement,
#' and additional arguments.
#'
#' @param .data The name of the data frame.
#' @param fun The name of the function to be applied (e.g., "select").
#' @param tidy_select The tidy select statement (e.g., column names).
#' @param other_args Additional arguments to the function.
#' @param inc_data Boolean, whether to include a data object in the call
#' @return An expression object of the tidyverse call.
#' @noRd
.make_tidyverse_call <- function(.data, fun, tidy_select, other_args = NULL, inc_data = TRUE) {
  if (is.null(other_args)) {
    tidy_string <- paste0("dplyr::", fun, "(", tidy_select, ")")
  } else {
    tidy_string <- paste0("dplyr::", fun, "(", tidy_select, ", ", other_args, ")")
  }
  if (inc_data) {
    tidy_string <- paste0(.data, " %>% ", tidy_string)
  }
  return(rlang::parse_expr(tidy_string))
}

#' Paste Character Arguments
#'
#' This function takes any number of arguments and returns a formatted string
#' that includes the argument names and their corresponding values.
#'
#' @param ... Any number of arguments.
#' @return A character string with the argument names and values.
#' @importFrom purrr map imap set_names
#' @importFrom dplyr %>%
#' @noRd
.paste_character_args <- function(...) {
  arg_values <- list(...) %>% purrr::map(deparse)
  call_stack <- sys.call()
  arg_names <- as.character(call_stack)[-1]
  arg_values <- purrr::set_names(arg_values, arg_names)
  args_formatted <- arg_values %>% purrr::imap(~ paste0(.y, " = ", .x))
  args_as_vector <- paste(unlist(args_formatted), collapse = ", ")
  return(args_as_vector)
}


#' Evaluate an expression and handle errors gracefully.
#'
#' @param string_as_expr An expression to be evaluated.
#' @param .data The data environment in which the expression should be evaluated.
#' @importFrom cli cli_abort
#' @importFrom rlang eval_tidy
#' @return The result of evaluating the expression, or an error message if evaluation fails.
#' @noRd
.execute_with_error_handling <- function(fun, string_as_expr) {
  object_out <- tryCatch(
    eval_tidy(string_as_expr),
    error = function(e) {
      cli_abort(
        c("x" = "`{fun}DS` returned the following error:", "i" = conditionMessage(e)),
        call = NULL
      )
    }
  )

  return(object_out)
}
