(* Self-passes on the tokens for CameLIGO *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Unit   = LexerLib.Unit

(* Local dependencies *)

module Config        = Preprocessing_pascaligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module Parameters    = LexerLib.CLI.Make (PreprocParams)
module Options       = Parameters.Options

(* Definition of a self-pass (a.k.a. filter) *)

type item = Token.t

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

let jsligo =
  match Options.jsligo with
    None -> "None"
  | Some None -> "Some None"
  | Some Some file -> file

let filters = [
  Caml.(if Options.jsligo <> None then Some Comments.filter else None);
  Caml.(if Options.jsligo <> None then Some Directives.filter else None)
  (* Add more in this list. *)
]

let filters : t =
  let f opt acc =
    match opt with
      None        -> acc
    | Some filter -> filter :: acc
  in List.fold_right filters ~f ~init:[]
