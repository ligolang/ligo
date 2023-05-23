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

module Token = Lx_ml_self_tokens.Token

(* Filter *)

let add_comment (comment : string Region.reg) : Token.t -> Token.t = function
  Directive _
| BlockCom _
| LineCom  _
| ZWSP _
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
| Lang     w -> Lang (w#add_line_comment comment)
| Attr     w -> Attr (w#add_line_comment comment)

  (* Symbols *)

| ARROW    w -> ARROW (w#add_line_comment comment)
| ASS      w -> ASS (w#add_line_comment comment)
| CONS     w -> CONS (w#add_line_comment comment)
| CARET    w -> CARET (w#add_line_comment comment)
| MINUS    w -> MINUS (w#add_line_comment comment)
| PLUS     w -> PLUS (w#add_line_comment comment)
| SLASH    w -> SLASH (w#add_line_comment comment)
| TIMES    w -> TIMES (w#add_line_comment comment)
| LPAR     w -> LPAR (w#add_line_comment comment)
| RPAR     w -> RPAR (w#add_line_comment comment)
| LBRACKET w -> LBRACKET (w#add_line_comment comment)
| RBRACKET w -> RBRACKET (w#add_line_comment comment)
| LBRACE   w -> LBRACE (w#add_line_comment comment)
| RBRACE   w -> RBRACE (w#add_line_comment comment)
| COMMA    w -> COMMA (w#add_line_comment comment)
| SEMI     w -> SEMI (w#add_line_comment comment)
| VBAR     w -> VBAR (w#add_line_comment comment)
| COLON    w -> COLON (w#add_line_comment comment)
| DOT      w -> DOT (w#add_line_comment comment)
| WILD     w -> WILD (w#add_line_comment comment)
| EQ       w -> EQ (w#add_line_comment comment)
| NE       w -> NE (w#add_line_comment comment)
| LT       w -> LT (w#add_line_comment comment)
| GT       w -> GT (w#add_line_comment comment)
| LE       w -> LE (w#add_line_comment comment)
| BOOL_OR  w -> BOOL_OR (w#add_line_comment comment)
| BOOL_AND w -> BOOL_AND (w#add_line_comment comment)
| QUOTE    w -> QUOTE (w#add_line_comment comment)
| REV_APP  w -> REV_APP (w#add_line_comment comment)
| PLUS_EQ  w -> PLUS_EQ (w#add_line_comment comment)
| MINUS_EQ w -> MINUS_EQ (w#add_line_comment comment)
| TIMES_EQ w -> TIMES_EQ (w#add_line_comment comment)
| SLASH_EQ w -> SLASH_EQ (w#add_line_comment comment)
| VBAR_EQ  w -> VBAR_EQ (w#add_line_comment comment)

  (* Keywords *)

| Begin     w -> Begin (w#add_line_comment comment)
| Do        w -> Do (w#add_line_comment comment)
| Done      w -> Done (w#add_line_comment comment)
| Downto    w -> Downto (w#add_line_comment comment)
| Else      w -> Else (w#add_line_comment comment)
| End       w -> End (w#add_line_comment comment)
| For       w -> For (w#add_line_comment comment)
| Fun       w -> Fun (w#add_line_comment comment)
| If        w -> If (w#add_line_comment comment)
| In        w -> In (w#add_line_comment comment)
| Land      w -> Land (w#add_line_comment comment)
| Let       w -> Let (w#add_line_comment comment)
| Lor       w -> Lor (w#add_line_comment comment)
| Lsl       w -> Lsl (w#add_line_comment comment)
| Lsr       w -> Lsr (w#add_line_comment comment)
| Lxor      w -> Lxor (w#add_line_comment comment)
| Match     w -> Match (w#add_line_comment comment)
| Mod       w -> Mod (w#add_line_comment comment)
| Module    w -> Module (w#add_line_comment comment)
| Mut       w -> Mut (w#add_line_comment comment)
| Not       w -> Not (w#add_line_comment comment)
| Of        w -> Of (w#add_line_comment comment)
| Or        w -> Or (w#add_line_comment comment)
| Rec       w -> Rec (w#add_line_comment comment)
| Struct    w -> Struct (w#add_line_comment comment)
| Then      w -> Then (w#add_line_comment comment)
| Type      w -> Type (w#add_line_comment comment)
| Upto      w -> Upto (w#add_line_comment comment)
| While     w -> While (w#add_line_comment comment)
| With      w -> With (w#add_line_comment comment)
| Contract  w -> Contract (w#add_line_comment comment)
| Parameter w -> Parameter (w#add_line_comment comment)

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
                      "Running CameLIGO unit  self-pass: \
                       Hooking line comments to the left token.")
    | None -> ()
  in Ok (filter units)
