module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module SMap = W.SMap

module Make (Params : Woo_helpers.PARAMS) = struct
  module Helpers = Woo_helpers.Generate (Params)
  open Helpers

  let property_type : W.labelled_record_field -> P.structure_item list =
   fun lt ->
    let label, W.{ type_expression = ty; default_value = _; index = _ } = lt in
    let body = extract_core_type ty in
    let name = label_to_variable label in
    [ abstract_type_declaration name body ]


  let property_types : W.record -> P.structure_item list =
   fun record_declarations ->
    let ltss = SMap.to_kv_list record_declarations in
    let itemss = List.map ~f:property_type ltss in
    let items = List.concat itemss in
    items


  let property : W.labelled_record_field -> P.structure_item list =
   fun lts ->
    let label, W.{ type_expression = ty; default_value = _; index = _ } = lts in
    ignore ty;
    let body = e_fun "x" (e_property ~label ~record:(e_var "x")) in
    let name = label_to_variable label in
    [ declaration ~name ~body ]


  let properties : W.record -> P.structure_item list =
   fun property_declarations ->
    let ltss = SMap.to_kv_list property_declarations in
    let itemss = List.map ~f:property ltss in
    let items = List.concat itemss in
    items


  let mapper : W.labelled_record_field -> P.structure_item list =
   fun lts ->
    let label, W.{ type_expression = ty; default_value = _; index = _ } = lts in
    ignore ty;
    let body =
      e_fun "f"
      @@ e_fun "x"
      @@ e_with "x" label (e_apply (e_var "f") (e_property ~label ~record:(e_var "x")))
    in
    let name = label_to_variable ("map_" ^ label) in
    [ declaration ~name ~body ]


  let mappers : W.record -> P.structure_item list =
   fun property_declarations ->
    let ltss = SMap.to_kv_list property_declarations in
    let itemss = List.map ~f:mapper ltss in
    let items = List.concat itemss in
    items


  let setter : W.labelled_record_field -> P.structure_item list =
   fun lts ->
    let label, W.{ type_expression = ty; default_value = _; index = _ } = lts in
    ignore ty;
    let body = e_fun "x" @@ e_fun "content" @@ e_with "x" label (e_var "content") in
    let name = label_to_variable ("set_" ^ label) in
    [ declaration ~name ~body ]


  let setters : W.record -> P.structure_item list =
   fun property_declarations ->
    let ltss = SMap.to_kv_list property_declarations in
    let itemss = List.map ~f:setter ltss in
    let items = List.concat itemss in
    items
end
