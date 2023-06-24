(* Self-passes on the lexical units for PascaLIGO *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Unit    = LexerLib.Unit
module Options = LexerLib.Options

(* Local dependencies *)

module Token = Lx_psc_self_tokens.Token
module Style = Lexing_shared.Style

(* Definition of a self-pass (a.k.a. filter) *)

module Make (Options : Options.S) =
  struct
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

    let comments = Caml.(
      if Options.jsligo <> None then Some Comments.filter else None)

    let filters = [
      Some Style'.filter;
      Some ZWSP.filter;
      comments
      (* Add more in this list. *)
    ]

    let filters : t =
      let f opt acc =
        match opt with
          None        -> acc
        | Some filter -> filter :: acc
      in List.fold_right filters ~f ~init:[]
  end
