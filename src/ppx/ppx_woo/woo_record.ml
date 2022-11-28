module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module SMap = W.SMap

module Make (Params : Woo_helpers.PARAMS) = struct
  module Helpers = Woo_helpers.Generate (Params)
  open Helpers

  let make prefix : W.record -> P.structure_item list =
   fun property_declarations ->
    let lts = SMap.to_kv_list property_declarations in
    let f : W.labelled_record_field -> P.expression -> P.expression =
     fun lr expr ->
      let label, rf = lr in
      let W.{ default_value; _ } = rf in
      match default_value with
      | None -> e_named_fun label expr
      | Some default -> e_option_fun ~default label expr
    in
    let init =
      let aux : W.labelled_record_field -> string * P.expression =
       fun lr ->
        let label, _ = lr in
        label, e_var label
      in
      let lst = List.map ~f:aux lts in
      e_record lst
    in
    let body = Base.List.fold_right lts ~init ~f in
    let name = prefix ^ "make" in
    [ declaration ~name ~body ]


  let make_tpl prefix : W.record -> P.structure_item list =
   fun property_declarations ->
    let lts = SMap.to_kv_list property_declarations in
    let lts = List.sort ~compare:(fun (_, a) (_, b) -> a.W.index - b.W.index) lts in
    let f : W.labelled_record_field -> P.expression -> P.expression =
     fun lr expr ->
      let label, _rf = lr in
      e_fun label expr
    in
    let init =
      let aux : W.labelled_record_field -> string * P.expression =
       fun lr ->
        let label, _ = lr in
        label, e_var label
      in
      let lst = List.map ~f:aux lts in
      e_record lst
    in
    let body = Base.List.fold_right lts ~init ~f in
    let name = prefix ^ "make_tpl" in
    [ declaration ~name ~body ]


  let destruct_tpl prefix : W.record -> P.structure_item list =
   fun property_declarations ->
    let lts = SMap.to_kv_list property_declarations in
    let lts = List.sort ~compare:(fun (_, a) (_, b) -> a.W.index - b.W.index) lts in
    let lts = List.map ~f:(fun (name, _) -> name) lts in
    let record = p_record_pun lts in
    let tuple = e_tuple @@ List.map ~f:e_var lts in
    let body = e_fun_pat record tuple in
    let name = prefix ^ "destruct" in
    [ declaration ~name ~body ]
end
