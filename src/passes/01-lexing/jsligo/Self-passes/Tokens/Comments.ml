(* Embedding comments in the next token (including preprocessing
   directives) -- including EOF *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Utils   = Simple_utils.Utils
module Snippet = Simple_utils.Snippet

(* Local dependencies *)

module Directive = Preprocessor.Directive
module Attr      = Lexing_shared.Attr
module Wrap      = Lexing_shared.Wrap

(* Utilities *)

type tokens = Token.t list

(* Filter (right to left) *)

let add_comment (comment : Wrap.comment) : Token.t -> Token.t = function
  Directive w ->
    let comment =
      match comment with
        Wrap.Block {region; value} ->
          let value = Preprocessing_jsligo.Config.block, value
          in `BlockComment Region.{region; value}
      | Wrap.Line {region; value} ->
          let value = Preprocessing_jsligo.Config.line, value
          in `LineComment  Region.{region; value}
    in Directive (Directive.add_comment comment w)

  (* Comments *)

| BlockCom w -> BlockCom (w#add_comment comment)
| LineCom  w -> LineCom (w#add_comment comment)

  (* Literals *)

| String   w -> String (w#add_comment comment)
| Verbatim w -> Verbatim (w#add_comment comment)
| Bytes    w -> Bytes (w#add_comment comment)
| Int      w -> Int (w#add_comment comment)
| Nat      w -> Nat (w#add_comment comment)
| Mutez    w -> Mutez (w#add_comment comment)
| Ident    w -> Ident (w#add_comment comment)
| UIdent   w -> UIdent (w#add_comment comment)
| EIdent   w -> EIdent (w#add_comment comment)
| Attr     w -> Attr (w#add_comment comment)

(* Symbols *)

| SHARP      w -> SHARP (w#add_comment comment)
| MINUS      w -> MINUS (w#add_comment comment)
| PLUS       w -> PLUS (w#add_comment comment)
| SLASH      w -> SLASH (w#add_comment comment)
| TIMES      w -> TIMES (w#add_comment comment)
| REM        w -> REM (w#add_comment comment)
| QMARK      w -> QMARK (w#add_comment comment)
| PLUS2      w -> PLUS2 (w#add_comment comment)
| MINUS2     w -> MINUS2 (w#add_comment comment)
| LPAR       w -> LPAR (w#add_comment comment)
| RPAR       w -> RPAR (w#add_comment comment)
| LBRACKET   w -> LBRACKET (w#add_comment comment)
| RBRACKET   w -> RBRACKET (w#add_comment comment)
| LBRACE     w -> LBRACE (w#add_comment comment)
| RBRACE     w -> RBRACE (w#add_comment comment)
| COMMA      w -> COMMA (w#add_comment comment)
| SEMI       w -> SEMI (w#add_comment comment)
| COLON      w -> COLON (w#add_comment comment)
| DOT        w -> DOT (w#add_comment comment)
| ELLIPSIS   w -> ELLIPSIS (w#add_comment comment)
| OR         w -> OR (w#add_comment comment)
| AND        w -> AND (w#add_comment comment)
| NOT        w -> NOT (w#add_comment comment)
| XOR        w -> XOR (w#add_comment comment)
| BIT_AND    w -> BIT_AND (w#add_comment comment)
| BIT_NOT    w -> BIT_NOT (w#add_comment comment)
| BIT_XOR    w -> BIT_XOR (w#add_comment comment)
| BIT_SL     w -> BIT_SL (w#add_comment comment)
| EQ         w -> EQ (w#add_comment comment)
| EQ2        w -> EQ2 (w#add_comment comment)
| NE         w -> NE (w#add_comment comment)
| LT         w -> LT (w#add_comment comment)
| GT         w -> GT (w#add_comment comment)
| LE         w -> LE (w#add_comment comment)
| PLUS_EQ    w -> PLUS_EQ (w#add_comment comment)
| MINUS_EQ   w -> MINUS_EQ (w#add_comment comment)
| MULT_EQ    w -> MULT_EQ (w#add_comment comment)
| REM_EQ     w -> REM_EQ (w#add_comment comment)
| DIV_EQ     w -> DIV_EQ (w#add_comment comment)
| BIT_SL_EQ  w -> BIT_SL_EQ (w#add_comment comment)
| BIT_SR_EQ  w -> BIT_SR_EQ (w#add_comment comment)
| BIT_AND_EQ w -> BIT_AND_EQ (w#add_comment comment)
| BIT_OR_EQ  w -> BIT_OR_EQ (w#add_comment comment)
| BIT_XOR_EQ w -> BIT_XOR_EQ (w#add_comment comment)
| VBAR       w -> VBAR (w#add_comment comment)
| ARROW      w -> ARROW (w#add_comment comment)
| WILD       w -> WILD (w#add_comment comment)

(* JavaScript Keywords *)

| Break    w -> Break (w#add_comment comment)
| Case     w -> Case (w#add_comment comment)
| Const    w -> Const (w#add_comment comment)
| Continue w -> Continue (w#add_comment comment)
| Default  w -> Default (w#add_comment comment)
| Else     w -> Else (w#add_comment comment)
| Export   w -> Export (w#add_comment comment)
| False    w -> False (w#add_comment comment)
| For      w -> For (w#add_comment comment)
| From     w -> From (w#add_comment comment)
| If       w -> If (w#add_comment comment)
| Import   w -> Import (w#add_comment comment)
| Let      w -> Let (w#add_comment comment)
| Of       w -> Of (w#add_comment comment)
| Return   w -> Return (w#add_comment comment)
| Switch   w -> Switch (w#add_comment comment)
| True     w -> True (w#add_comment comment)
| While    w -> While (w#add_comment comment)

(* TypeScript keywords *)

| As          w -> As (w#add_comment comment)
| Extends     w -> Extends (w#add_comment comment)
| Function    w -> Function (w#add_comment comment)
| Implements  w -> Implements (w#add_comment comment)
| Interface   w -> Interface (w#add_comment comment)
| Namespace   w -> Namespace (w#add_comment comment)
| Type        w -> Type (w#add_comment comment)

(* JsLIGO-specific keywords *)

| ContractOf  w -> ContractOf (w#add_comment comment)
| Do          w -> Do (w#add_comment comment)
| Match       w -> Match (w#add_comment comment)
| ParameterOf w -> ParameterOf (w#add_comment comment)
| When        w -> When (w#add_comment comment)

(* Virtual tokens *)

| ZWSP      w -> ZWSP (w#add_comment comment)
| PARAMS    w -> PARAMS (w#add_comment comment)
| ES6FUN    w -> ES6FUN (w#add_comment comment)
| SEMI_ELSE (w1, w2) -> SEMI_ELSE (w1, w2#add_comment comment)

(* End-Of-File *)

| EOF w -> EOF (w#add_comment comment)

(* Filter *)

let rec hook_comments_to_token (t : Token.t) (acc : Token.t list) = function
  Token.BlockCom w :: tokens ->
    let comment = Wrap.Block Region.{region=w#region; value=w#payload}
    in hook_comments_to_token (add_comment comment t) acc tokens
| Token.LineCom w :: tokens ->
    let comment = Wrap.Line Region.{region=w#region; value=w#payload}
    in hook_comments_to_token (add_comment comment t) acc tokens
| tokens -> t :: acc, tokens

let filter (tokens : tokens) =
  let open! Token
  in
  let rec aux acc = function
    t :: tokens -> Utils.uncurry aux @@ hook_comments_to_token t acc tokens
  | [] -> acc (* Restore original order *)
  in
  aux [] (List.rev tokens)

(* Exported *)

type message = string Region.reg

let filter :
      ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      tokens ->
      (tokens, tokens * message) result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Embedding comments in the next token.")
    | None -> ()
  in Ok (filter tokens)
