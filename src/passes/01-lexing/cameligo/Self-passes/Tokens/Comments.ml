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
| Nat      w -> Nat (add_wrap_comment comment w)
| Mutez    w -> Mutez (add_wrap_comment comment w)
| Ident    w -> Ident (add_wrap_comment comment w)
| UIdent   w -> UIdent (add_wrap_comment comment w)
| Lang     w -> Lang (add_wrap_comment comment w)
| Attr     w -> Attr (add_wrap_comment comment w)

  (* Symbols *)

| ARROW    w -> ARROW (add_wrap_comment comment w)
| ASS      w -> ASS (add_wrap_comment comment w)
| CONS     w -> CONS (add_wrap_comment comment w)
| CARET    w -> CARET (add_wrap_comment comment w)
| MINUS    w -> MINUS (add_wrap_comment comment w)
| PLUS     w -> PLUS (add_wrap_comment comment w)
| SLASH    w -> SLASH (add_wrap_comment comment w)
| TIMES    w -> TIMES (add_wrap_comment comment w)
| LPAR     w -> LPAR (add_wrap_comment comment w)
| RPAR     w -> RPAR (add_wrap_comment comment w)
| LBRACKET w -> LBRACKET (add_wrap_comment comment w)
| RBRACKET w -> RBRACKET (add_wrap_comment comment w)
| LBRACE   w -> LBRACE (add_wrap_comment comment w)
| RBRACE   w -> RBRACE (add_wrap_comment comment w)
| COMMA    w -> COMMA (add_wrap_comment comment w)
| SEMI     w -> SEMI (add_wrap_comment comment w)
| VBAR     w -> VBAR (add_wrap_comment comment w)
| COLON    w -> COLON (add_wrap_comment comment w)
| DOT      w -> DOT (add_wrap_comment comment w)
| WILD     w -> WILD (add_wrap_comment comment w)
| EQ       w -> EQ (add_wrap_comment comment w)
| NE       w -> NE (add_wrap_comment comment w)
| LT       w -> LT (add_wrap_comment comment w)
| GT       w -> GT (add_wrap_comment comment w)
| LE       w -> LE (add_wrap_comment comment w)
| BOOL_OR  w -> BOOL_OR (add_wrap_comment comment w)
| BOOL_AND w -> BOOL_AND (add_wrap_comment comment w)
| QUOTE    w -> QUOTE (add_wrap_comment comment w)
| REV_APP  w -> REV_APP (add_wrap_comment comment w)
| PLUS_EQ  w -> PLUS_EQ (add_wrap_comment comment w)
| MINUS_EQ w -> MINUS_EQ (add_wrap_comment comment w)
| TIMES_EQ w -> TIMES_EQ (add_wrap_comment comment w)
| SLASH_EQ w -> SLASH_EQ (add_wrap_comment comment w)
| VBAR_EQ  w -> VBAR_EQ (add_wrap_comment comment w)

  (* Keywords *)

| Begin     w -> Begin (add_wrap_comment comment w)
| Do        w -> Do (add_wrap_comment comment w)
| Done      w -> Done (add_wrap_comment comment w)
| Downto    w -> Downto (add_wrap_comment comment w)
| Else      w -> Else (add_wrap_comment comment w)
| End       w -> End (add_wrap_comment comment w)
| For       w -> For (add_wrap_comment comment w)
| Fun       w -> Fun (add_wrap_comment comment w)
| If        w -> If (add_wrap_comment comment w)
| In        w -> In (add_wrap_comment comment w)
| Land      w -> Land (add_wrap_comment comment w)
| Let       w -> Let (add_wrap_comment comment w)
| Lor       w -> Lor (add_wrap_comment comment w)
| Lsl       w -> Lsl (add_wrap_comment comment w)
| Lsr       w -> Lsr (add_wrap_comment comment w)
| Lxor      w -> Lxor (add_wrap_comment comment w)
| Match     w -> Match (add_wrap_comment comment w)
| Mod       w -> Mod (add_wrap_comment comment w)
| Module    w -> Module (add_wrap_comment comment w)
| Mut       w -> Mut (add_wrap_comment comment w)
| Not       w -> Not (add_wrap_comment comment w)
| Of        w -> Of (add_wrap_comment comment w)
| Or        w -> Or (add_wrap_comment comment w)
| Rec       w -> Rec (add_wrap_comment comment w)
| Struct    w -> Struct (add_wrap_comment comment w)
| Then      w -> Then (add_wrap_comment comment w)
| Type      w -> Type (add_wrap_comment comment w)
| Upto      w -> Upto (add_wrap_comment comment w)
| While     w -> While (add_wrap_comment comment w)
| With      w -> With (add_wrap_comment comment w)
| Contract  w -> Contract (add_wrap_comment comment w)
| Parameter w -> Parameter (add_wrap_comment comment w)

  (* Virtual tokens *)

| ZWSP   w -> ZWSP (add_wrap_comment comment w)

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
             "Running CameLIGO token self-pass: \
              Embedding comments in the next token.")
    | None -> ()
  in Ok (filter tokens)
