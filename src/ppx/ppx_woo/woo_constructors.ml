module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module SMap = W.SMap

module Make (Params : Woo_helpers.PARAMS) = struct
  module Helpers = Woo_helpers.Generate (Params)
  open Helpers

  let constructor_type polymorphic : W.labelled_types -> P.structure_item list =
   fun lts ->
    ignore polymorphic;
    let label, tys = lts in
    let body =
      match tys with
      | [] -> t_unit
      | [ single ] -> extract_core_type single
      | lst -> t_tuple @@ List.map ~f:extract_core_type lst
    in
    let name = label_to_variable label in
    [ abstract_type_declaration name body ]


  let constructor_types : W.variant -> P.structure_item list =
   fun v ->
    let W.{ polymorphic; constructor_declarations } = v in
    let ltss = SMap.to_kv_list constructor_declarations in
    let itemss = List.map ~f:(constructor_type polymorphic) ltss in
    let items = List.concat itemss in
    items


  let constructor ?wrap_constructor polymorphic
      : W.labelled_types -> P.structure_item list
    =
   fun lts ->
    let label, tys = lts in
    let l = List.length tys in
    let wrap body =
      match wrap_constructor with
      | None -> body
      | Some wrap_constructor -> e_apply wrap_constructor body
    in
    let body =
      if l = 0
      then wrap @@ e_constructor ~polymorphic label
      else (
        let rec aux vars_rev lst =
          match lst with
          | [] ->
            let body = e_tuple @@ List.map ~f:e_var @@ List.rev vars_rev in
            wrap @@ e_constructor ~polymorphic label ~body
          | _hd :: tl ->
            let name = "x" ^ string_of_int (List.length vars_rev) in
            e_fun name @@ aux (name :: vars_rev) tl
        in
        aux [] tys)
    in
    let name = label_to_variable label in
    [ declaration ~name ~body ]


  let constructors ?wrap_constructor : W.variant -> P.structure_item list =
   fun v ->
    let W.{ polymorphic; constructor_declarations } = v in
    let ltss = SMap.to_kv_list constructor_declarations in
    let itemss = List.map ~f:(constructor ?wrap_constructor polymorphic) ltss in
    let items = List.concat itemss in
    items
end
