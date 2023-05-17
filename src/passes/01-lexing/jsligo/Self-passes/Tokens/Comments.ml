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

let sprintf = Printf.sprintf

let format_error ~no_colour ~file value (region: Region.t) =
  let value =
    if file then
      sprintf "%s%s"
        (Format.asprintf "%a" (Snippet.pp_lift ~no_colour) region)
        (Std.redden value)
    else
      let header = region#to_string ~file ~offsets:true `Point
      in sprintf "%s:\n%s" header value
  in Region.{region; value}

let warn_about msg region =
  let msg = format_error ~no_colour:false ~file:true msg region
  in Printf.eprintf "%s\n%!" msg.value

(* Filter (right to left) *)

let add_wrap_comment (comment : string Wrap.t) (w : 'a Wrap.t) : 'a Wrap.t =
  w#add_comment {region = comment#region; value = comment#payload}

let add_comment (comment : string Wrap.t) : Token.t -> Token.t = function
  Directive w -> let region = comment#region
                 and value  = comment#payload in
                 let reg    = Region.{value; region} in
                 Directive (Directive.add_comment reg w)

  (* Comments *)

| BlockCom w -> BlockCom (add_wrap_comment comment w)
| LineCom  w -> LineCom (add_wrap_comment comment w)

  (* Literals *)

| String   w -> String (add_wrap_comment comment w)
| Verbatim w -> Verbatim (add_wrap_comment comment w)
| Bytes    w -> Bytes (add_wrap_comment comment w)
| Int      w -> Int (add_wrap_comment comment w)
| Ident    w -> Ident (add_wrap_comment comment w)
| UIdent   w -> UIdent (add_wrap_comment comment w)
| Attr     w -> Attr (add_wrap_comment comment w)

(* Symbols *)

| MINUS    w -> MINUS (add_wrap_comment comment w)
| PLUS     w -> PLUS (add_wrap_comment comment w)
| SLASH    w -> SLASH (add_wrap_comment comment w)
| TIMES    w -> TIMES (add_wrap_comment comment w)
| REM      w -> REM (add_wrap_comment comment w)
| QMARK    w -> QMARK (add_wrap_comment comment w)
| LPAR     w -> LPAR (add_wrap_comment comment w)
| RPAR     w -> RPAR (add_wrap_comment comment w)
| LBRACKET w -> LBRACKET (add_wrap_comment comment w)
| RBRACKET w -> RBRACKET (add_wrap_comment comment w)
| LBRACE   w -> LBRACE (add_wrap_comment comment w)
| RBRACE   w -> RBRACE (add_wrap_comment comment w)
| COMMA    w -> COMMA (add_wrap_comment comment w)
| SEMI     w -> SEMI (add_wrap_comment comment w)
| COLON    w -> COLON (add_wrap_comment comment w)
| DOT      w -> DOT (add_wrap_comment comment w)
| ELLIPSIS w -> ELLIPSIS (add_wrap_comment comment w)
| BOOL_OR  w -> BOOL_OR (add_wrap_comment comment w)
| BOOL_AND w -> BOOL_AND (add_wrap_comment comment w)
| BOOL_NOT w -> BOOL_NOT (add_wrap_comment comment w)
| EQ       w -> EQ (add_wrap_comment comment w)
| EQ2      w -> EQ2 (add_wrap_comment comment w)
| NE       w -> NE (add_wrap_comment comment w)
| LT       w -> LT (add_wrap_comment comment w)
| GT       w -> GT (add_wrap_comment comment w)
| LE       w -> LE (add_wrap_comment comment w)
| PLUS_EQ  w -> PLUS_EQ (add_wrap_comment comment w)
| MINUS_EQ w -> MINUS_EQ (add_wrap_comment comment w)
| MULT_EQ  w -> MULT_EQ (add_wrap_comment comment w)
| REM_EQ   w -> REM_EQ (add_wrap_comment comment w)
| DIV_EQ   w -> DIV_EQ (add_wrap_comment comment w)
| VBAR     w -> VBAR (add_wrap_comment comment w)
| ARROW    w -> ARROW (add_wrap_comment comment w)
| WILD     w -> WILD (add_wrap_comment comment w)
| INCR     w -> INCR (add_wrap_comment comment w)
| DECR     w -> DECR (add_wrap_comment comment w)

(* JavaScript Keywords *)

| Break    w -> Break (add_wrap_comment comment w)
| Case     w -> Case (add_wrap_comment comment w)
| Const    w -> Const (add_wrap_comment comment w)
| Default  w -> Default (add_wrap_comment comment w)
| Else     w -> Else (add_wrap_comment comment w)
| Export   w -> Export (add_wrap_comment comment w)
| For      w -> For (add_wrap_comment comment w)
| From     w -> From (add_wrap_comment comment w)
| If       w -> If (add_wrap_comment comment w)
| Import   w -> Import (add_wrap_comment comment w)
| Let      w -> Let (add_wrap_comment comment w)
| Of       w -> Of (add_wrap_comment comment w)
| Return   w -> Return (add_wrap_comment comment w)
| Switch   w -> Switch (add_wrap_comment comment w)
| While    w -> While (add_wrap_comment comment w)

(* TypeScript keywords *)

| As          w -> As (add_wrap_comment comment w)
| Namespace   w -> Namespace (add_wrap_comment comment w)
| Type        w -> Type (add_wrap_comment comment w)

(* Contract keywords *)

| Contract  w -> Contract (add_wrap_comment comment w)
| Parameter w -> Parameter (add_wrap_comment comment w)

(* Virtual tokens *)

| ZWSP   w -> ZWSP (add_wrap_comment comment w)
| ES6FUN w -> ES6FUN (add_wrap_comment comment w)

(* End-Of-File *)

| EOF w -> EOF (add_wrap_comment comment w)

let rec hook_comments_to_token (t : Token.t) (acc : Token.t list) = function
  Token.(BlockCom w | LineCom w) :: tokens ->
    hook_comments_to_token (add_comment w t) acc tokens
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
