type s_syntax = Syntax_name of string

type t =
  | CameLIGO
  | JsLIGO
  | PascaLIGO
[@@deriving eq, ord]
