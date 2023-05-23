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
  Directive w -> (
    let region, value =
      match comment with
        Wrap.Block Region.{region; value} -> region, value
      | Wrap.Line  Region.{region; value} -> region, value in
    let reg = Region.{value; region} in
    Directive (Directive.add_comment reg w))

  (* Comments *)

| BlockCom w -> BlockCom (w#add_comment comment)
| LineCom  w -> LineCom (w#add_comment comment)

  (* Literals *)

| String   w -> String (w#add_comment comment)
| Verbatim w -> Verbatim (w#add_comment comment)
| Bytes    w -> Bytes (w#add_comment comment)
| Int      w -> Int (w#add_comment comment)
| Ident    w -> Ident (w#add_comment comment)
| UIdent   w -> UIdent (w#add_comment comment)
| Attr     w -> Attr (w#add_comment comment)

(* Symbols *)

| MINUS    w -> MINUS (w#add_comment comment)
| PLUS     w -> PLUS (w#add_comment comment)
| SLASH    w -> SLASH (w#add_comment comment)
| TIMES    w -> TIMES (w#add_comment comment)
| REM      w -> REM (w#add_comment comment)
| QMARK    w -> QMARK (w#add_comment comment)
| LPAR     w -> LPAR (w#add_comment comment)
| RPAR     w -> RPAR (w#add_comment comment)
| LBRACKET w -> LBRACKET (w#add_comment comment)
| RBRACKET w -> RBRACKET (w#add_comment comment)
| LBRACE   w -> LBRACE (w#add_comment comment)
| RBRACE   w -> RBRACE (w#add_comment comment)
| COMMA    w -> COMMA (w#add_comment comment)
| SEMI     w -> SEMI (w#add_comment comment)
| COLON    w -> COLON (w#add_comment comment)
| DOT      w -> DOT (w#add_comment comment)
| ELLIPSIS w -> ELLIPSIS (w#add_comment comment)
| BOOL_OR  w -> BOOL_OR (w#add_comment comment)
| BOOL_AND w -> BOOL_AND (w#add_comment comment)
| BOOL_NOT w -> BOOL_NOT (w#add_comment comment)
| EQ       w -> EQ (w#add_comment comment)
| EQ2      w -> EQ2 (w#add_comment comment)
| NE       w -> NE (w#add_comment comment)
| LT       w -> LT (w#add_comment comment)
| GT       w -> GT (w#add_comment comment)
| LE       w -> LE (w#add_comment comment)
| PLUS_EQ  w -> PLUS_EQ (w#add_comment comment)
| MINUS_EQ w -> MINUS_EQ (w#add_comment comment)
| MULT_EQ  w -> MULT_EQ (w#add_comment comment)
| REM_EQ   w -> REM_EQ (w#add_comment comment)
| DIV_EQ   w -> DIV_EQ (w#add_comment comment)
| VBAR     w -> VBAR (w#add_comment comment)
| ARROW    w -> ARROW (w#add_comment comment)
| WILD     w -> WILD (w#add_comment comment)
| INCR     w -> INCR (w#add_comment comment)
| DECR     w -> DECR (w#add_comment comment)

(* JavaScript Keywords *)

| Break    w -> Break (w#add_comment comment)
| Case     w -> Case (w#add_comment comment)
| Const    w -> Const (w#add_comment comment)
| Default  w -> Default (w#add_comment comment)
| Else     w -> Else (w#add_comment comment)
| Export   w -> Export (w#add_comment comment)
| For      w -> For (w#add_comment comment)
| From     w -> From (w#add_comment comment)
| If       w -> If (w#add_comment comment)
| Import   w -> Import (w#add_comment comment)
| Let      w -> Let (w#add_comment comment)
| Of       w -> Of (w#add_comment comment)
| Return   w -> Return (w#add_comment comment)
| Switch   w -> Switch (w#add_comment comment)
| While    w -> While (w#add_comment comment)

(* TypeScript keywords *)

| As          w -> As (w#add_comment comment)
| Namespace   w -> Namespace (w#add_comment comment)
| Type        w -> Type (w#add_comment comment)

(* Contract keywords *)

| Contract  w -> Contract (w#add_comment comment)
| Parameter w -> Parameter (w#add_comment comment)

(* Virtual tokens *)

| ZWSP   w -> ZWSP (w#add_comment comment)
| ES6FUN w -> ES6FUN (w#add_comment comment)

(* End-Of-File *)

| EOF w -> EOF (w#add_comment comment)

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
