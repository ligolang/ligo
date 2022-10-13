(* This module implements a filter on the lexical units and produces
   tokens to be consumed by the parser. *)

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

(* Composing self-passes *)

module Make (Passes : PASSES) =
  struct
    type item = Passes.item

    type items = Passes.items

    type error = {
      used_items : item list;
      message    : string Region.reg
    }

    let run ?(last = List.length Passes.filters)
            ?print_passes ~add_warning items =
      let rec aux index acc = function
        [] -> acc
      | pass::passes ->
          if index <= last then
            match acc with
              Ok items ->
                let acc = pass ?print_passes ~add_warning items
                in aux (index+1) acc passes
            | Error _ as err -> err
          else acc in
      match aux 1 (Ok items) Passes.filters with
        Ok _ as ok -> ok
      | Error (used_items, message) -> Error {used_items; message}
  end
