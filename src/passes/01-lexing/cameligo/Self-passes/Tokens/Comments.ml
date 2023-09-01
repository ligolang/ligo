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
        Wrap.Block {region; value = c} ->
          `BlockComment Region.{region; value = Preprocessing_cameligo.Config.block, c}
        | Wrap.Line {region; value = c} ->
          `LineComment  Region.{region; value = Preprocessing_cameligo.Config.line, c}
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
| Lang     w -> Lang (w#add_comment comment)
| Attr     w -> Attr (w#add_comment comment)

  (* Symbols *)

| ARROW    w -> ARROW (w#add_comment comment)
| ASS      w -> ASS (w#add_comment comment)
| CONS     w -> CONS (w#add_comment comment)
| CARET    w -> CARET (w#add_comment comment)
| MINUS    w -> MINUS (w#add_comment comment)
| PLUS     w -> PLUS (w#add_comment comment)
| SLASH    w -> SLASH (w#add_comment comment)
| TIMES    w -> TIMES (w#add_comment comment)
| LPAR     w -> LPAR (w#add_comment comment)
| RPAR     w -> RPAR (w#add_comment comment)
| LBRACKET w -> LBRACKET (w#add_comment comment)
| RBRACKET w -> RBRACKET (w#add_comment comment)
| LBRACE   w -> LBRACE (w#add_comment comment)
| RBRACE   w -> RBRACE (w#add_comment comment)
| COMMA    w -> COMMA (w#add_comment comment)
| SEMI     w -> SEMI (w#add_comment comment)
| VBAR     w -> VBAR (w#add_comment comment)
| COLON    w -> COLON (w#add_comment comment)
| DOT      w -> DOT (w#add_comment comment)
| WILD     w -> WILD (w#add_comment comment)
| EQ       w -> EQ (w#add_comment comment)
| NE       w -> NE (w#add_comment comment)
| LT       w -> LT (w#add_comment comment)
| GT       w -> GT (w#add_comment comment)
| LE       w -> LE (w#add_comment comment)
| BOOL_OR  w -> BOOL_OR (w#add_comment comment)
| BOOL_AND w -> BOOL_AND (w#add_comment comment)
| QUOTE    w -> QUOTE (w#add_comment comment)
| REV_APP  w -> REV_APP (w#add_comment comment)
| PLUS_EQ  w -> PLUS_EQ (w#add_comment comment)
| MINUS_EQ w -> MINUS_EQ (w#add_comment comment)
| TIMES_EQ w -> TIMES_EQ (w#add_comment comment)
| SLASH_EQ w -> SLASH_EQ (w#add_comment comment)
| VBAR_EQ  w -> VBAR_EQ (w#add_comment comment)

  (* Keywords *)

| Begin     w -> Begin (w#add_comment comment)
| Do        w -> Do (w#add_comment comment)
| Done      w -> Done (w#add_comment comment)
| Downto    w -> Downto (w#add_comment comment)
| Else      w -> Else (w#add_comment comment)
| End       w -> End (w#add_comment comment)
| For       w -> For (w#add_comment comment)
| Fun       w -> Fun (w#add_comment comment)
| If        w -> If (w#add_comment comment)
| In        w -> In (w#add_comment comment)
| Land      w -> Land (w#add_comment comment)
| Let       w -> Let (w#add_comment comment)
| Lor       w -> Lor (w#add_comment comment)
| Lsl       w -> Lsl (w#add_comment comment)
| Lsr       w -> Lsr (w#add_comment comment)
| Lxor      w -> Lxor (w#add_comment comment)
| Match     w -> Match (w#add_comment comment)
| Mod       w -> Mod (w#add_comment comment)
| Module    w -> Module (w#add_comment comment)
| Include   w -> Include (w#add_comment comment)
| Mut       w -> Mut (w#add_comment comment)
| Not       w -> Not (w#add_comment comment)
| Of        w -> Of (w#add_comment comment)
| Or        w -> Or (w#add_comment comment)
| Rec       w -> Rec (w#add_comment comment)
| Sig       w -> Sig (w#add_comment comment)
| Struct    w -> Struct (w#add_comment comment)
| Then      w -> Then (w#add_comment comment)
| Type      w -> Type (w#add_comment comment)
| Upto      w -> Upto (w#add_comment comment)
| Val       w -> Val (w#add_comment comment)
| While     w -> While (w#add_comment comment)
| With      w -> With (w#add_comment comment)
| Contract  w -> Contract (w#add_comment comment)
| Parameter w -> Parameter (w#add_comment comment)

  (* Virtual tokens *)

| ZWSP   w -> ZWSP (w#add_comment comment)

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
             "Running CameLIGO token self-pass: \
              Embedding comments in the next token.")
    | None -> ()
  in Ok (filter tokens)
