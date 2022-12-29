type s_syntax = Syntax_name of string
type s_dialect = Dialect_name of string

type t =
  | PascaLIGO
  | CameLIGO
  | JsLIGO
[@@deriving eq, ord]
