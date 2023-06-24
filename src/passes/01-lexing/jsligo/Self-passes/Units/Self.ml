(* Self-passes on the lexical units for CameLIGO *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Unit    = LexerLib.Unit
module Options = LexerLib.Options

(* Local dependencies *)

module Token = Lx_js_self_tokens.Token
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

    let filters : t = [
      Style'.filter;
      ZWSP.filter;
      LineCom.filter; (* Before [Comments.filter] *)
      Comments.filter
    ]
  end
