(* This module implements a filter on the lexical units of JsLIGO
   and produces tokens to be consumed by the parser. *)

[@@@warning "-42"]

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Utils     = Simple_utils.Utils
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive

module Snippet   = Simple_utils.Snippet

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list, message) result
  end

(* Utilities *)

let ok x = Stdlib.Ok x

let apply filter = function
  Stdlib.Ok tokens -> filter tokens |> ok
| Error _ as err   -> err

type message = string Region.reg

type token = Token.t

type lex_unit = token Core.lex_unit

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup (Markup.BlockCom {value; region}) ->
        Token.BlockCom (Token.wrap value region) :: tokens
    | Core.Markup (Markup.LineCom {value; region}) ->
        Token.LineCom (Token.wrap value region) :: tokens
    | Core.Markup _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left ~f:apply ~init:[] lex_units |> List.rev |> ok
| Error _ as err -> err


(* Automatic Semicolon Insertion *)

let automatic_semicolon_insertion tokens =
  let open! Token in
  let rec inner result = function
    (Directive _ as t) :: rest ->
    inner (t :: result) rest
  | (LineCom _ as t) :: rest ->
    inner (t :: result) rest
  | (BlockCom _ as t) :: rest ->
    inner (t :: result) rest
  | (_ as semi) :: (LineCom _ as t) :: rest
  | (_ as semi) :: (BlockCom _ as t) :: rest
  | (SEMI _ as semi) :: (Namespace _ as t)  :: rest
  | (SEMI _ as semi) :: (Export _ as t)  :: rest
  | (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (Namespace _ as t)  :: rest
  | (LBRACE _ as semi) :: (Export _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest ->
    inner (t:: semi :: result) rest
  | (SEMI _) :: (Else _ as t) :: rest ->
    inner (t :: result) rest
  | token :: (Namespace _ as t) :: rest
  | token :: (Export _ as t) :: rest
  | token :: (Let _ as t) :: rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest ->
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    if r#stop#line < r2#start#line  then (
      inner (t :: SEMI (Token.wrap ";" (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop)) :: token :: result) rest
    )
    else (
      match token with
        RBRACE _ as t ->
        inner (t :: SEMI (Token.wrap_semi (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop)) :: token :: result) rest
      | _ ->
        inner (t :: token :: result) rest
    )
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in
  inner [] tokens

let automatic_semicolon_insertion units =
  apply automatic_semicolon_insertion units

(* Attributes *)

let attribute_regexp = Str.regexp "@\\([a-zA-Z:0-9_]+\\)"

let collect_attributes str : string list =
  let x : Str.split_result list = Str.full_split attribute_regexp str in
  let f (acc : string list) = function
    Str.Text _ -> acc
  | Str.Delim string -> string :: acc
  in List.rev (List.fold_left ~f ~init:[] x)

let attributes tokens =
  let open! Token in
  let rec inner result = function
    LineCom c :: tl
  | BlockCom c :: tl ->
      let attrs = collect_attributes c#payload in
      let apply key = Token.mk_attr ~key c#region in
      let attrs = List.map ~f:apply attrs
      in inner (attrs @ result) tl
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in inner [] tokens

let attributes units = apply attributes units

(* Injection of Zero-Width Spaces *)

let inject_zwsp lex_units =
  let open! Token in
  let rec aux acc = function
    [] -> List.rev acc
  | (Core.Token GT _ as gt1) :: (Core.Token GT reg :: _ as units) ->
      aux (Core.Token (Token.mk_ZWSP reg#region) :: gt1 :: acc) units
  | unit::units -> aux (unit::acc) units
  in aux [] lex_units

let inject_zwsp units = apply inject_zwsp units

(* DEBUG *)

(* Printing lexical units *)

let print_unit = function
  Core.Token t ->
    Printf.printf "%s\n" (Token.to_string ~offsets:true `Point t)
| Core.Markup m ->
    Printf.printf "%s\n" (Markup.to_string ~offsets:true `Point m)
| Core.Directive d ->
    Printf.printf "%s\n" (Directive.to_string ~offsets:true `Point d)

let print_units units =
  apply (fun units -> List.iter ~f:print_unit units; units) units

(* Printing tokens *)

let print_token token =
  Printf.printf "%s\n" (Token.to_string ~offsets:true `Point token)

let print_tokens tokens =
  apply (fun tokens -> List.iter ~f:print_token tokens; tokens) tokens


(* insert vertical bar for sum type *)

let vertical_bar_insertion tokens =
  let open! Token in
  let rec aux acc insert_token = function
    (VBAR _ as hd) :: tl ->
    aux (hd::acc) false tl
  | (EQ _ as hd) :: tl ->
    if insert_token then (
      List.rev_append (hd :: VBAR (Token.wrap "|" Region.ghost) :: acc) tl
    )
    else (
      List.rev_append (hd :: acc) tl
    )
  | (RBRACKET _ as hd) :: tl ->
    aux (hd::acc) true tl
  | hd :: tl ->
    aux (hd::acc) insert_token tl
  | [] ->
    List.rev acc
  in
  aux [] false tokens

let vertical_bar_insertion tokens =
  let open! Token in
  let rec aux acc = function
    (VBAR _ as hd) :: tl -> 
      aux (vertical_bar_insertion (hd::acc)) tl
  | hd :: tl -> aux (hd::acc) tl
  | [] -> List.rev acc
  in aux [] tokens

let vertical_bar_insertion units = apply vertical_bar_insertion units

(* It's not a lexer *)
type window = <
  last_token    : token option;
  current_token : token           (* Including EOF *)
  >

let window     : window option ref    = ref None
let get_window : unit -> window option = fun () -> !window

let set_window ~current ~last : unit =
  window := Some (object
                    method last_token    = last
                    method current_token = current
                  end)

let fake_lexer: token list -> Lexing.lexbuf -> token = fun tokens ->
  let store = ref tokens in
  fun _ ->
    match !store with
      token::tokens ->
        let last =
          match !window with
            None -> None
          | Some window -> Some window#current_token in
        set_window ~current:token ~last;
        store := tokens;
        token
    | [] -> Token.mk_eof Region.ghost

let pre_parser tokens : (token list, string Region.reg) result  =
  let fake_lexer = fake_lexer tokens in
  let fake_lexbuf = Lexing.from_string "" in
  let module Inter = PreParser.MenhirInterpreter in
  let supplier     = Inter.lexer_lexbuf_to_supplier fake_lexer fake_lexbuf in
  let success a    = ok a
  in
  let failure = function
    Inter.Accepted s ->  Ok s
  | HandlingError _env -> 
    (* We don't handle this here, but let it go through *)
    Error Region.{value = "Parser error."; region = Region.ghost}
  | _ -> Error Region.{value = "Unhandled state."; region = Region.ghost}
  in
  let checkpoint = PreParser.Incremental.self_pass fake_lexbuf.lex_curr_p in
  let _buffer, supplier = MenhirLib.ErrorReports.wrap_supplier supplier in
  Inter.loop_handle success failure supplier checkpoint

let pre_parser = function
  Stdlib.Ok tokens -> 
    (match pre_parser tokens with 
      Ok t -> Ok t
    | Error _ -> Ok tokens)
| Error _ as err   -> err

(* COMPOSING FILTERS (exported) *)

let filter ~add_warning:_ =
    Utils.(
  attributes  
  <@ automatic_semicolon_insertion
  <@ vertical_bar_insertion
  (* <@ print_tokens *)
  <@ pre_parser
  (* <@ print_tokens   *)
  <@ tokens_of
  (*  @@ print_units*)
  <@ inject_zwsp
  <@ Style.check)