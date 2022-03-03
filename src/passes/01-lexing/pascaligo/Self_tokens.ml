(* This module implements a filter on the lexical units of PascaLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

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

let old_syntax_support tokens =
  let open! Token in

  let rec insert_lbracket indent result = function
    (Of _ as o) :: rest when indent = 0 -> 
      List.rev_append (o :: ghost_LBRACKET :: result) rest
  | (End _  as hd) :: rest -> 
      insert_lbracket (indent + 1) (hd :: result) rest
  | (Begin _ as hd) :: rest ->
      insert_lbracket (indent - 1) (hd :: result) rest
  | hd :: tl -> insert_lbracket indent (hd :: result) tl
  | [] -> List.rev result
  in

  let rec inner indent result = function
    (End _ as end_) :: rest when indent > 0 -> 
      inner (indent - 1) (end_ :: result) rest
  | End _ :: rest -> 
      let result = insert_lbracket 0 [] result in
      (* Yes, this is ugly - however the other option would be way more invasive, while we don't want to keep this code. *)
      print_endline "Warning: deprecated case syntax. Cases are now expected to be wrapped inside brackets, without a closing `end` keyword. ";
      List.rev_append (ghost_RBRACKET :: result) rest
  | (Begin _ as b) :: rest ->
      inner (indent + 1) (b :: result) rest
  | (Case _ as c) :: rest ->
      inner indent (c :: result) rest
  | hd :: tl -> inner indent (hd :: result) tl
  | [] -> List.rev result
  in
  inner 0 [] tokens

let old_syntax_support units =
  apply old_syntax_support units
  

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup    _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left ~f:apply ~init:[] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Exported *)

let filter units = (
  old_syntax_support
  @@ tokens_of 
  @@ Style.check units)
