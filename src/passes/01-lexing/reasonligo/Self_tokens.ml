(* This module implements a filter on the lexical units of ReasonLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module List   = Core.List
module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
module Snippet = Simple_utils.Snippet

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list, message) result
  end

(* Filters *)

let ok x = Stdlib.Ok x

type message = string Region.reg

type token = Token.t
type lex_unit = token Core.lex_unit

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left ~f:apply ~init:[] lex_units |> List.rev |> ok
| Error _ as err -> err


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

let pre_parser tokens =
  let fake_lexer = fake_lexer tokens in
  let fake_lexbuf = Lexing.from_string "" in
  let module Inter = PreParser.MenhirInterpreter in
  let supplier     = Inter.lexer_lexbuf_to_supplier fake_lexer fake_lexbuf in
  let success a    = ok a
  in
  let failure = function
    Inter.Accepted s ->  Ok s
  | HandlingError env ->
    (match Inter.top env with
        Some (Inter.Element (s, _, _, _)) ->
          let window = get_window() in
          let prefix = (match window with
            Some window ->
              let region = Token.to_region window#current_token in
              Format.asprintf "%a\n" Snippet.pp_lift region
          | None ->
              "") in
          let state = Inter.number s in
          let msg = try PreParErr.message state with Not_found -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n" in
          let msg = if msg = "<YOUR SYNTAX ERROR MESSAGE HERE>\n" then
            "Syntax error " ^ string_of_int state ^ "."
          else
            msg
          in
          let error_msg = prefix ^ msg in
          Error Region.{value = error_msg; region = Region.ghost}
      | None ->
        Error Region.{value = "Parser error."; region = Region.ghost}
    )
  | _ -> Error Region.{value = "Unhandled state."; region = Region.ghost}
  in
  let checkpoint = PreParser.Incremental.self_pass fake_lexbuf.lex_curr_p in
  let _buffer, supplier = MenhirLib.ErrorReports.wrap_supplier supplier in
  Inter.loop_handle success failure supplier checkpoint

let apply filter = function
  Stdlib.Ok tokens -> filter tokens
| Error _ as err   -> err

let pre_parser units =
  apply pre_parser units

(* debug *)

let print_token token =
  Printf.printf "%s\n" (Token.to_string ~offsets:true `Point token)

let print_tokens (tokens: (token list, _) result) =
  apply (fun tokens -> List.iter ~f:print_token tokens; Ok tokens) tokens

(* Exported *)

let filter ~add_warning:_ =
  Utils.(
  (* print_tokens
  <@  *)
  pre_parser
  <@ tokens_of
  <@ Style.check
  )
