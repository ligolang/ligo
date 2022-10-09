(* Self-passes on the tokens for JsLIGO *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Unit   = LexerLib.Unit

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

let filters : t = [
  ES6FUN.filter;
  ES6FUN_hook.filter;
  VBAR.filter;
  VBAR_hook.filter;
  SEMI.filter;
  SEMI_hook.filter;
  Attributes.filter
]
