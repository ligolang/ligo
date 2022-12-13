module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module SMap = W.SMap

module Make (Params : Woo_helpers.PARAMS) = struct
  module Helpers = Woo_helpers.Generate (Params)
  open Helpers

  let destruct prefix : W.variant -> P.structure_item list =
   fun variant ->
    let W.{ polymorphic; constructor_declarations } = variant in
    let lts = SMap.to_kv_list constructor_declarations in
    let vars = lts |> List.map ~f:(fun (l, _tes) -> l) |> List.map ~f:String.lowercase in
    let single_case
        : W.label * W.type_expression list -> string * string list * P.expression
      =
     fun (lbl, tes) ->
      let l = List.length tes in
      let body = e_apply (e_var @@ String.lowercase lbl) @@ e_tuple @@ e_vars "x" l in
      lbl, var_names "x" l, body
    in
    let body =
      lts
      |> List.map ~f:single_case
      |> e_match ~polymorphic (e_var "_matchee")
      |> e_named_funs vars
      |> e_fun "_matchee"
    in
    let name = prefix ^ "destruct" in
    [ declaration ~name ~body ]


  let destruct_tpl prefix : W.variant -> P.structure_item list =
   fun variant ->
    let W.{ polymorphic; constructor_declarations } = variant in
    let lts = SMap.to_kv_list constructor_declarations in
    let vars = lts |> List.map ~f:(fun (l, _tes) -> l) |> List.map ~f:String.lowercase in
    let single_case
        : W.label * W.type_expression list -> string * string list * P.expression
      =
     fun (lbl, tes) ->
      let l = List.length tes in
      let body = e_apply (e_var @@ String.lowercase lbl) @@ e_tuple @@ e_vars "x" l in
      lbl, var_names "x" l, body
    in
    let body =
      lts
      |> List.map ~f:single_case
      |> e_match ~polymorphic (e_var "_matchee")
      |> e_funs vars
      |> e_fun "_matchee"
    in
    let name = prefix ^ "destruct_tpl" in
    [ declaration ~name ~body ]
end
