(* Composing self-passes to build a pipeline *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std

(* Self-passes *)

module type PASSES =
  sig
    type item

    type items = item list

    type message = string Region.reg

    type t =
      (?print_passes:Std.t ->
       add_warning:(Main_warnings.all -> unit) ->
       items ->
       (items, items * message) result) list

    val filters : t
  end

(* Functor making a pipeline from a list of passes and a rank *)

module Make (Passes : PASSES) :
  sig
    type item = Passes.item

    type items = Passes.items

    type error = {
      used_items : item list;
      message    : string Region.reg
    }

    (* The call [run ?last] is a pass that corresponds to the
       composition of [last] self-passes from [Passes.filters] (the
       functor parameter). By default, the first pass has index 1 and
       the last is [List.length Passes.filters]. For example, if there
       are three passes, then [run ~add_warning ~last:2 items] will
       compose and apply the first two passes. *)

    val run :
      ?last:int ->
      ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      items ->
      (items, error) result
  end
