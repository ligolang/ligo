(* This module implements a filter on the lexical units of PascaLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
module Location  = Simple_utils.Location

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

let apply filter = function
  Stdlib.Ok tokens -> filter tokens |> ok
| Error _ as err   -> err

type message = string Region.reg

type token = Token.t
type lex_unit = token Core.lex_unit

let old_syntax_support ~add_warning tokens =
  let open! Token in

  let rec insert_lbracket indent result = function
    (Of of_ as o) :: rest when indent = 0 ->
      List.rev_append (o :: ghost_LBRACKET :: result) rest, of_#region
  | (End _  as hd) :: rest ->
      insert_lbracket (indent + 1) (hd :: result) rest
  | (Begin _ as hd) :: rest ->
      insert_lbracket (indent - 1) (hd :: result) rest
  | hd :: tl -> insert_lbracket indent (hd :: result) tl
  | [] -> List.rev result, Region.ghost
  in

  let rec inner indent possible_matches result = function
    End e :: rest when List.mem possible_matches (indent - 1) ~equal:Caml.(=) ->
      let result, start = insert_lbracket 0 [] result in
      let loc = Location.lift (Region.cover start e#region) in
      let ()  = add_warning (Main_warnings.pascaligo_deprecated_case loc) in
      inner (indent - 2) (List.filter ~f:(fun p -> p <> (indent -1)) possible_matches) (ghost_RBRACKET :: result) rest
  | (End _ as end_) :: rest ->
      inner (indent - 1) possible_matches (end_ :: result) rest
  | (Begin _ as b) :: rest when indent > 0 ->
      inner (indent + 1) possible_matches (b :: result) rest
  | (Case _ as c) :: rest ->
      inner (indent + 1) ((indent + 1) :: possible_matches) (c :: result) rest
  | (Of _ as o) :: rest when List.mem possible_matches indent ~equal:Caml.(=) ->
    inner (indent + 1) possible_matches (o :: result) rest
  | (LBRACKET _ as l) :: rest when List.mem possible_matches (indent - 1) ~equal:Caml.(=) ->
    inner (indent - 2) (List.filter ~f:(fun p -> p <> (indent -1)) possible_matches) (l :: result) rest
  | (LBRACKET _ as l) :: rest ->
    inner (indent + 1) possible_matches (l :: result) rest
  | (RBRACKET _ as r) :: rest when List.mem possible_matches indent ~equal:Caml.(=) ->
    inner (indent - 1) (List.filter ~f:(fun p -> p <> indent) possible_matches) (r :: result) rest
  | (RBRACKET _ as r) :: rest ->
    inner (indent - 1) possible_matches (r :: result) rest
  | (SEMI s) :: (Else _ as e ) :: rest ->
      let loc = Location.lift s#region in
      let ()  = add_warning (Main_warnings.pascaligo_deprecated_semi_before_else loc) in
      inner indent possible_matches (e :: result) rest
  | hd :: tl -> inner indent possible_matches (hd :: result) tl
  | [] -> List.rev result
  in
  inner 0 [] [] tokens

let old_syntax_support ~add_warning units =
  apply (old_syntax_support ~add_warning) units


(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup    _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left ~f:apply ~init:[] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Printing tokens *)

let print_token token =
  Printf.printf "%s\n" (Token.to_string ~offsets:true `Point token)

let print_tokens tokens =
  apply (fun tokens -> List.iter ~f:print_token tokens; tokens) tokens

(* Exported *)

let filter ~add_warning units = (
  (* print_tokens
  @@  *)
  old_syntax_support ~add_warning
  @@ tokens_of
  @@ Style.check units)
