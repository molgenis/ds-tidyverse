.getEncodeKey <- function() {
  encode_list <- list(
    input = c("list", "(", ")", "\"", ",", " ", "c", ":", "!", "&", "|"),
    output = c("$LIST$", "$LB", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$C$", "$COLON$", "$EXCL$", "$AND$", "$OR$")
  )
  return(encode_list)
}

.decode_tidy_eval <- function(input_string, encode_key) {
  encode_vec <- setNames(encode_key$input, encode_key$output)
  output_string <- str_replace_all(input_string, fixed(encode_vec))
  return(output_string)
}

.remove_spaces <- function(expr) {
  out <- str_replace_all(expr, fixed(" "), "")
  return(out)
}
