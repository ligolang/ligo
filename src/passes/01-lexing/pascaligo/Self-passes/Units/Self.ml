(* Self-passes on the lexical units for PascaLIGO *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Unit    = LexerLib.Unit

(* Local dependencies *)

module Token = Lx_psc_self_tokens.Token
module Style = Lexing_shared.Style

module Config        = Preprocessing_pascaligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module Parameters    = LexerLib.CLI.Make (PreprocParams)
module Options       = Parameters.Options

(* Definition of a self-pass (a.k.a. filter) *)

type item = Token.t Unit.t

type items = item list

type message = string Region.reg

type filter =
  ?print_passes:Std.t ->
  add_warning:(Main_warnings.all -> unit) ->
  items ->
  (items, items * message) result

type t = filter list

(* Listing all self-passes on lexical units (resulting in
   [filters]) *)

module Style' = Style.Make (Token)

let filters = [
  Some Style'.filter;
  Some ZWSP.filter;
  Caml.(if Options.jsligo <> None then Some Comments.filter else None)
  (* Add more in this list. *)
]

let filters : t =
  let f opt acc =
    match opt with
      None        -> acc
    | Some filter -> filter :: acc
  in List.fold_right filters ~f ~init:[]
