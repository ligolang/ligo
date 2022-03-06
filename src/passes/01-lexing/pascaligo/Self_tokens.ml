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

let add_warning: (Main_warnings.all -> unit) option ref  = ref None

let old_syntax_support tokens =
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

  let rec inner indent result = function
    (End _ as end_) :: rest when indent > 0 -> 
      inner (indent - 1) (end_ :: result) rest
  | End e :: rest ->
      let result, start = insert_lbracket 0 [] result in
      let loc = Location.lift (Region.cover start e#region) in
      (match !add_warning with 
        Some add_warning -> add_warning (Main_warnings.pascaligo_deprecated_case loc)
      | None -> ());
      List.rev_append (ghost_RBRACKET :: result) rest
  | (Begin _ as b) :: rest ->
      inner (indent + 1) (b :: result) rest
  | (Case _ as c) :: rest ->
      inner indent (c :: result) rest
  | (SEMI s) :: (Else _ as e ) :: rest ->
      let loc = Location.lift s#region in
      (match !add_warning with 
        Some add_warning -> add_warning (Main_warnings.pascaligo_deprecated_semi_before_else loc)
      | None -> ());
      inner indent (e :: result) rest
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
