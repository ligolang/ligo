module P = Ppxlib
module A = P.Ast_builder.Default
module W = Woo_types
module SMap = W.SMap

module Make (Params : Woo_helpers.PARAMS) = struct
  module Helpers = Woo_helpers.Generate (Params)
  open Helpers

  let matching ?wrap_get ~namer case : W.variant -> W.labelled_types -> P.structure_item =
   fun variant (base_label, _ty) ->
    let function_name = p_var @@ namer @@ label_to_variable base_label in
    let W.{ constructor_declarations; polymorphic } = variant in
    let constructor_declarations' = SMap.to_kv_list constructor_declarations in
    let cases = List.map ~f:(case ~polymorphic ~base_label) constructor_declarations' in
    let body =
      match wrap_get with
      | None -> A.pexp_function ~loc cases
      | Some wrap ->
        let lhs =
          A.pexp_apply
            ~loc
            wrap
            [ Nolabel, A.pexp_ident ~loc @@ Location.mkloc (Longident.Lident "x") loc ]
        in
        A.pexp_fun ~loc Nolabel None (A.ppat_var ~loc @@ Location.mkloc "x" loc)
        @@ A.pexp_match ~loc lhs cases
    in
    let declaration = A.value_binding ~loc ~expr:body ~pat:function_name in
    let declarations = A.pstr_value ~loc Nonrecursive [ declaration ] in
    declarations


  let generic_case ~polymorphic ~base_label rhs
      : string * W.type_expression list -> P.case
    =
   fun (current_label, tys) ->
    let variable_name = "_ppx_match_variable" in
    let e_variable = e_var variable_name in
    let p_variable = p_var variable_name in
    let p_tuple_name i = "_" ^ Int.to_string i in
    let l = List.length tys in
    let lhs =
      if l = 0
      then p_constructor ~polymorphic current_label
      else (
        let pattern =
          if polymorphic
          then p_var variable_name
          else (
            let pattern_names = Base.List.init ~f:p_tuple_name l in
            let patterns = List.map ~f:(fun name -> A.pvar ~loc name) pattern_names in
            A.ppat_tuple ~loc patterns)
        in
        p_constructor ~polymorphic ~pattern current_label)
    in
    let rhs =
      let rhs = rhs ~base_label ~current_label ~variable:e_variable in
      let local_lhs_expr =
        if l = 0
        then e_unit
        else if polymorphic
        then e_variable
        else (
          let var_names = Base.List.init ~f:p_tuple_name l in
          let vars = List.map ~f:e_var var_names in
          A.pexp_tuple ~loc vars)
      in
      let lhs =
        let pat = p_variable in
        let expr = local_lhs_expr in
        A.value_binding ~loc ~pat ~expr
      in
      A.pexp_let ~loc P.Nonrecursive [ lhs ] rhs
    in
    A.case ~lhs ~guard:None ~rhs


  let matching_full ?wrap_get ~namer rhs : W.variant -> P.structure_item list =
   fun variant ->
    let matching_single =
      let case = generic_case rhs in
      matching ?wrap_get ~namer case
    in
    let W.{ constructor_declarations; polymorphic = _ } = variant in
    let constructor_declarations = SMap.to_kv_list constructor_declarations in
    let matching_lst = List.map ~f:(matching_single variant) constructor_declarations in
    matching_lst


  let matching_is_full ?wrap_get : W.variant -> P.structure_item list =
    let rhs ~base_label ~current_label ~variable:_ =
      e_bool @@ String.equal base_label current_label
    in
    let namer x = "is_" ^ x in
    matching_full ?wrap_get ~namer rhs


  let matching_exn_full ?wrap_get : W.variant -> P.structure_item list =
    let namer x = "get_" ^ x ^ "_exn" in
    let rhs ~base_label ~current_label ~variable =
      let failure =
        let match_exn_name = namer base_label in
        let str_constant = e_string match_exn_name in
        P.([%expr raise (Failure [%e str_constant])])
      in
      if String.equal base_label current_label then variable else failure
    in
    matching_full ?wrap_get ~namer rhs


  let matching_opt_full ?wrap_get : W.variant -> P.structure_item list =
    let namer x = "get_" ^ x ^ "_opt" in
    let rhs ~base_label ~current_label ~variable =
      if String.equal base_label current_label
      then P.([%expr Some [%e variable]])
      else P.([%expr None])
    in
    matching_full ~namer ?wrap_get rhs


  let matching_default ?(default_get = "Option") variant : P.structure_item list =
    let namer x = "get_" ^ x in
    let string =
      match default_get with
      | "Exception" -> "_exn"
      | _ -> "_opt"
    in
    let namer_default x = "get_" ^ x ^ string in
    let matching_single (base_label, _ty) =
      let body =
        A.pexp_ident ~loc
        @@ Location.mkloc
             (Longident.Lident (namer_default @@ label_to_variable base_label))
             loc
      in
      let function_name = p_var @@ namer @@ label_to_variable base_label in
      let declaration = A.value_binding ~loc ~expr:body ~pat:function_name in
      let declarations = A.pstr_value ~loc Nonrecursive [ declaration ] in
      declarations
    in
    let W.{ constructor_declarations; polymorphic = _ } = variant in
    let constructor_declarations = SMap.to_kv_list constructor_declarations in
    let matching_lst = List.map ~f:matching_single constructor_declarations in
    matching_lst
end
