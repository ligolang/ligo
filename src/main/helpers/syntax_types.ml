type s_syntax = Syntax_name of string
type s_dialect = Dialect_name of string

type pascaligo_dialect = Terse | Verbose
[@@deriving eq]

type t =
  | PascaLIGO of pascaligo_dialect option
  | CameLIGO
  | ReasonLIGO
  | JsLIGO
  [@@deriving eq]