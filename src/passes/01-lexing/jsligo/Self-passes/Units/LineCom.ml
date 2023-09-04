(* Embedding line comments into the preceding token on the same line
   (if any) *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Std       = Simple_utils.Std
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = Preprocessor.Directive
module Unit      = LexerLib.Unit
module Wrap      = Lexing_shared.Wrap

(* Local dependencies *)

module Token = Lx_js_self_tokens.Token

(* Filter *)

let add_comment (comment : string Region.reg) : Token.t -> Token.t = function
  Directive _
| BlockCom _
| LineCom  _
| ZWSP _
| PARAMS _
| ES6FUN _
| EOF _ as token -> token

  (* Literals *)

| String   w -> String (w#add_line_comment comment)
| Verbatim w -> Verbatim (w#add_line_comment comment)
| Bytes    w -> Bytes (w#add_line_comment comment)
| Int      w -> Int (w#add_line_comment comment)
| Nat      w -> Nat (w#add_line_comment comment)
| Mutez    w -> Mutez (w#add_line_comment comment)
| Ident    w -> Ident (w#add_line_comment comment)
| UIdent   w -> UIdent (w#add_line_comment comment)
| Attr     w -> Attr (w#add_line_comment comment)

(* Symbols *)

| SHARP      w -> SHARP (w#add_line_comment comment)
| MINUS      w -> MINUS (w#add_line_comment comment)
| PLUS       w -> PLUS (w#add_line_comment comment)
| SLASH      w -> SLASH (w#add_line_comment comment)
| TIMES      w -> TIMES (w#add_line_comment comment)
| REM        w -> REM (w#add_line_comment comment)
| QMARK      w -> QMARK (w#add_line_comment comment)
| PLUS2      w -> PLUS2 (w#add_line_comment comment)
| MINUS2     w -> MINUS2 (w#add_line_comment comment)
| LPAR       w -> LPAR (w#add_line_comment comment)
| RPAR       w -> RPAR (w#add_line_comment comment)
| LBRACKET   w -> LBRACKET (w#add_line_comment comment)
| RBRACKET   w -> RBRACKET (w#add_line_comment comment)
| LBRACE     w -> LBRACE (w#add_line_comment comment)
| RBRACE     w -> RBRACE (w#add_line_comment comment)
| COMMA      w -> COMMA (w#add_line_comment comment)
| SEMI       w -> SEMI (w#add_line_comment comment)
| COLON      w -> COLON (w#add_line_comment comment)
| DOT        w -> DOT (w#add_line_comment comment)
| ELLIPSIS   w -> ELLIPSIS (w#add_line_comment comment)
| OR         w -> OR (w#add_line_comment comment)
| AND        w -> AND (w#add_line_comment comment)
| NOT        w -> NOT (w#add_line_comment comment)
| XOR        w -> XOR (w#add_line_comment comment)
| BIT_AND    w -> BIT_AND (w#add_line_comment comment)
| BIT_NOT    w -> BIT_NOT (w#add_line_comment comment)
| BIT_XOR    w -> BIT_XOR (w#add_line_comment comment)
| BIT_SL     w -> BIT_SL (w#add_line_comment comment)
| BIT_SR     w -> BIT_SR (w#add_line_comment comment)
| EQ         w -> EQ (w#add_line_comment comment)
| EQ2        w -> EQ2 (w#add_line_comment comment)
| NE         w -> NE (w#add_line_comment comment)
| LT         w -> LT (w#add_line_comment comment)
| GT         w -> GT (w#add_line_comment comment)
| LE         w -> LE (w#add_line_comment comment)
| PLUS_EQ    w -> PLUS_EQ (w#add_line_comment comment)
| MINUS_EQ   w -> MINUS_EQ (w#add_line_comment comment)
| MULT_EQ    w -> MULT_EQ (w#add_line_comment comment)
| REM_EQ     w -> REM_EQ (w#add_line_comment comment)
| DIV_EQ     w -> DIV_EQ (w#add_line_comment comment)
| BIT_SL_EQ  w -> BIT_SL_EQ (w#add_line_comment comment)
| BIT_SR_EQ  w -> BIT_SR_EQ (w#add_line_comment comment)
| BIT_AND_EQ w -> BIT_AND_EQ (w#add_line_comment comment)
| BIT_OR_EQ  w -> BIT_OR_EQ (w#add_line_comment comment)
| BIT_XOR_EQ w -> BIT_XOR_EQ (w#add_line_comment comment)
| VBAR       w -> VBAR (w#add_line_comment comment)
| ARROW      w -> ARROW (w#add_line_comment comment)
| WILD       w -> WILD (w#add_line_comment comment)

(* JavaScript Keywords *)

| Break    w -> Break (w#add_line_comment comment)
| Case     w -> Case (w#add_line_comment comment)
| Const    w -> Const (w#add_line_comment comment)
| Continue w -> Continue (w#add_line_comment comment)
| Default  w -> Default (w#add_line_comment comment)
| Else     w -> Else (w#add_line_comment comment)
| Export   w -> Export (w#add_line_comment comment)
| False    w -> False (w#add_line_comment comment)
| For      w -> For (w#add_line_comment comment)
| From     w -> From (w#add_line_comment comment)
| If       w -> If (w#add_line_comment comment)
| Import   w -> Import (w#add_line_comment comment)
| Let      w -> Let (w#add_line_comment comment)
| Of       w -> Of (w#add_line_comment comment)
| Return   w -> Return (w#add_line_comment comment)
| Switch   w -> Switch (w#add_line_comment comment)
| True     w -> True (w#add_line_comment comment)
| While    w -> While (w#add_line_comment comment)

(* TypeScript keywords *)

| As          w -> As (w#add_line_comment comment)
| Function    w -> Function (w#add_line_comment comment)
| Implements  w -> Implements (w#add_line_comment comment)
| Interface   w -> Interface (w#add_line_comment comment)
| Namespace   w -> Namespace (w#add_line_comment comment)
| Type        w -> Type (w#add_line_comment comment)

(* JsLIGO-specific keywords *)

| ContractOf  w -> ContractOf (w#add_line_comment comment)
| Do          w -> Do (w#add_line_comment comment)
| Match       w -> Match (w#add_line_comment comment)
| ParameterOf w -> ParameterOf (w#add_line_comment comment)
| When        w -> When (w#add_line_comment comment)

(* Virtual tokens *)

| SEMI_ELSE (w1, w2) -> SEMI_ELSE (w1, w2#add_line_comment comment)


let filter (units : Token.t Unit.t list) : Token.t Unit.t list =
  let open! Token in
  let rec aux acc = function
    `Token token :: rest -> skip_spaces token acc [] rest
  |        other :: rest -> aux (other :: acc) rest
  |                   [] -> acc
  and skip_spaces token acc spaces = function
    `Markup Markup.(Space _ | Tabs _) as space :: rest ->
       skip_spaces token acc (space :: spaces) rest
  | `Markup (Markup.LineCom comment) :: rest ->
       let token' = `Token (add_comment comment token)
       in aux (spaces @ (token' :: acc)) rest
  | rest -> aux (spaces @ (`Token token :: acc)) rest
  in List.rev (aux [] units)

(* Exported *)

type item = Token.t Unit.t

type units = item list

type message = string Region.reg

type result = (units, units * message) Stdlib.result

let filter ?print_passes ~add_warning:_ units : result =
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
                      "Running JsLIGO unit  self-pass: \
                       Hooking line comments to the left token.")
    | None -> ()
  in Ok (filter units)
