.getEncodeKey <- function(){

  encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|", "\n"),
    output = c("$LIST$", "$LB", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$", "$N$")
  )
  return(encode_list)
}

.decode_tidy_eval <- function(input_string, encode_key){
  encode_vec <- setNames(encode_key$input, encode_key$output)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
  return(output_string)
}

.remove_list <- function(string_decoded){

  string_decoded %>%
    str_replace_all(pattern = fixed("list("), replacement = "") %>%
    str_sub(end = -2)
}

.format_encoded_expr <- function(expr){

  return(out)
}

.remove_spaces <- function(expr){
  out <- str_replace_all(expr, fixed(" "), "")
  return(out)
}

.parse_data <- function(.data){
  parsed <- eval(parse(text = .data), envir = parent.frame())
  return(out)
}

.check_data_exists <- function(.data){
  obj_exists <- exists(.data)
  if(!obj_exists){
    cli_abort("The serverside object '{(.data)}' specified in `.data` does not exist")
  }
}

.check_data_class <- function(.data){
  ds_data <- .parse_data(.data)
  ds_class <- class(ds_data)
  if(!ds_class %in% c("data.frame", "tbl_df")){
    cli_abort(
      c(
        "The serverside object '{(.data)}' specified in `.data` must be a dataframe or tibble",
        "x" = "'{(.data)}' is class {ds_class}"
        )
    )
  }
}

.check_data <- function(.data){
  .check_data_exists(.data)
  .check_data_class(.data)
}

.execute_tidyverse_function <- function(.data, fun, tidy_select_args){
  string_to_eval <- paste0(.data, " %>% dplyr::", fun, "(", tidy_select_args, ")")
  string_as_expr <- rlang::parse_expr(string_to_eval)
  out <- eval_tidy(string_as_expr)
  return(out)
}

