(* Self-passes on the tokens for PyLIGO *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Options = LexerLib.Options

(* Definition of a self-pass (a.k.a. filter) *)

module Make (Options : Options.S) =
  struct
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

    let filters : t = []
  end
