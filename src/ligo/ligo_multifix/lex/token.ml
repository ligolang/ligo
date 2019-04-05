include Token_type

let to_string : token -> string = function
  | TIMES -> "TIMES"
  | STRING _ -> "STRING"
  | NAME _ -> "NAME s"
  | INT _ -> "INT n"
  | SEMICOLON -> "SEMICOLON"
  | RSQUARE -> "RSQUARE"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | LSQUARE -> "LSQUARE"
  | LIST -> "LIST"
  | LET -> "LET"
  | IN -> "IN"
  | EQUAL -> "EQUAL"
  | EOF -> "EOF"
  | DIV -> "DIV"
