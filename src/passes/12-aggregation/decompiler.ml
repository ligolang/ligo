open Ligo_prim

let rec decompile ~raise : Ast_aggregated.expression -> Ast_typed.expression =
  fun exp ->
  let module I = Ast_aggregated in
  let module O = Ast_typed in
   let decompile_value_attr : I.ValueAttr.t -> O.ValueAttr.t =
      fun {inline;no_mutation;view;public;hidden;thunk} -> {inline;no_mutation;view;public;hidden;thunk}
   in

  let return expression_content : O.expression =
    { expression_content ; location = exp.location ; type_expression = decompile_type ~raise exp.type_expression } in
  match exp.expression_content with
    | E_literal l ->
       return (O.E_literal l)
    | E_constant { cons_name ; arguments } ->
       let arguments = List.map ~f:(decompile ~raise) arguments in
       return (O.E_constant { cons_name ; arguments })
    | E_variable v ->
       return (O.E_variable v)
    | E_application { lamb ; args } ->
       let args = decompile ~raise args in
       let lamb = decompile ~raise lamb in
       return (O.E_application { lamb ; args })
    | E_lambda { binder ; output_type ; result } ->
       let binder = Param.map (decompile_type ~raise) binder in
       let output_type = decompile_type ~raise output_type in
       let result = decompile ~raise result in
       return (O.E_lambda { binder ; output_type ; result })
    | E_type_abstraction { type_binder ; result } ->
       let result = decompile ~raise result in
       return (O.E_type_abstraction { type_binder ; result })
    | E_recursive { fun_name ; fun_type ; lambda = { binder ; output_type ; result } } ->
       let fun_type = decompile_type ~raise fun_type in
       let result = decompile ~raise result in
       let output_type = decompile_type ~raise output_type in
       let binder = Param.map (decompile_type ~raise) binder in
       return (O.E_recursive { fun_name ; fun_type ; lambda = { binder ; output_type ; result } })
    | E_let_in { let_binder ; rhs ; let_result ; attr } ->
       let rhs = decompile ~raise rhs in
       let let_result = decompile ~raise let_result in
       let let_binder = Binder.map (decompile_type ~raise) let_binder in
         let attr = decompile_value_attr attr in
       return (O.E_let_in { let_binder ; rhs ; let_result ; attr })
    | E_raw_code { language ; code } ->
       let code = decompile ~raise code in
       return (O.E_raw_code { language ; code })
    | E_type_inst { forall ; type_ } ->
       let type_ = decompile_type ~raise type_ in
       let forall = decompile ~raise forall in
       return (O.E_type_inst { forall ; type_ })
    (* Variant *)
    | E_constructor { constructor ; element } ->
       let element = decompile ~raise element in
       return (O.E_constructor { constructor ; element })
    | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
      let matchee = decompile ~raise matchee in
      let aux : _ I.matching_content_case -> _ Ast_typed.Match_expr.match_case =
         fun { constructor ; pattern ; body } -> (
           let pattern =
            (* Note copied from Ast_typed untyper:
              If one day this code is actually executed, and if the list type is still not a tuple type.
              A special case for lists might be required here
            *)
            let tv = decompile_type ~raise tv in
            let proj = Location.wrap @@ Ast_typed.Pattern.P_var (Binder.make pattern tv) in
            Location.wrap @@ Ast_typed.Pattern.P_variant (constructor, proj)
           in
           let body = decompile ~raise body in
           ({pattern ; body } : (O.expression, O.type_expression) Ast_typed.Match_expr.match_case)
         )
       in
       let cases = List.map ~f:aux cases in
      return (O.E_matching { matchee ; cases })
    | E_matching { matchee ; cases = Match_record { fields ; body ; tv=_ } } ->
        let matchee = decompile ~raise matchee in
        let aux : (I.type_expression Binder.t) -> O.type_expression Ast_typed.Pattern.t =
          fun binder -> (
            let proj = Location.wrap 
              @@ Ast_typed.Pattern.P_var (Binder.map (decompile_type ~raise) binder) in
            proj
          )
        in
        let body = decompile ~raise body in
        let case = match Record.is_tuple fields with
          | false ->
            let fields = Record.map ~f:aux fields in
            let pattern = Location.wrap (O.Pattern.P_record fields) in
            ({ pattern ; body } : _ O.Match_expr.match_case)
          | true ->
            let patterns = Record.map ~f:aux fields in
            let patterns = Record.LMap.values patterns in
            let pattern = Location.wrap (O.Pattern.P_tuple patterns) in
            ({ pattern ; body } : _ O.Match_expr.match_case)
        in
        let cases = [case] in
       return (O.E_matching { matchee ; cases })
    (* Record *)
    | E_record map ->
       let map = Record.map ~f:(decompile ~raise) map in
       return (O.E_record map)
    | E_accessor { struct_ ; path } ->
       let struct_ = decompile ~raise struct_ in
       return (O.E_accessor { struct_ ; path })
    | E_update { struct_ ; path ; update } ->
       let struct_ = decompile ~raise struct_ in
       let update = decompile ~raise update in
       return (O.E_update { struct_ ; path ; update })
   (* Imperative *)
   | I.E_let_mut_in { let_binder ; rhs ; let_result ; attr } ->
      let rhs = decompile ~raise rhs in
      let let_result = decompile ~raise let_result in
      let let_binder = Binder.map (decompile_type ~raise) let_binder in
      let attr = decompile_value_attr attr in
      return (O.E_let_mut_in { let_binder ; rhs ; let_result ; attr })
   | I.E_deref var -> return (O.E_deref var)
   | I.E_assign {binder;expression} ->
      let binder = Binder.map (decompile_type ~raise) binder in
      let expression = decompile ~raise expression in
      return @@ O.E_assign {binder;expression}
   | I.E_for for_loop ->
      let for_loop = For_loop.map (decompile ~raise) for_loop in
      return @@ O.E_for for_loop
   | I.E_for_each for_each_loop ->
      let for_each_loop = For_each_loop.map (decompile ~raise) for_each_loop in
      return @@ O.E_for_each for_each_loop
   | I.E_while while_loop ->
      let while_loop = While_loop.map (decompile ~raise) while_loop in
      return @@ O.E_while while_loop

and decompile_type ~raise : Ast_aggregated.type_expression -> Ast_typed.type_expression =
  fun ty ->
  let module I = Ast_aggregated in
  let module O = Ast_typed in
  let return type_content : O.type_expression =
    { type_content ; location = ty.location ; orig_var = ty.orig_var ; type_meta = None } in
  match ty.type_content with
  | T_variable v ->
     return (O.T_variable v)
  | T_constant { language ; injection ; parameters } ->
     let parameters = List.map ~f:(decompile_type ~raise) parameters in
     return (O.T_constant { language ; injection ; parameters })
  | T_sum { fields ; layout } ->
     let f ({ associated_type ; michelson_annotation ; decl_pos } : I.row_element) : O.row_element =
       let associated_type = decompile_type ~raise associated_type in
       { associated_type ; michelson_annotation ; decl_pos } in
     let fields = Record.map ~f fields in
     return (O.T_sum { fields ; layout })
  | T_record { fields ; layout } ->
     let f ({ associated_type ; michelson_annotation ; decl_pos } : I.row_element) : O.row_element =
       let associated_type = decompile_type ~raise associated_type in
       { associated_type ; michelson_annotation ; decl_pos } in
     let fields = Record.map ~f fields in
     return (O.T_record { fields ; layout })
  | T_arrow { type1 ; type2 } ->
     let type1 = decompile_type ~raise type1 in
     let type2 = decompile_type ~raise type2 in
     return (O.T_arrow { type1 ; type2 })
  | T_singleton l ->
     return (O.T_singleton l)
  | T_for_all { ty_binder ; kind ; type_ } ->
     let type_ = decompile_type ~raise type_ in
     return (O.T_for_all { ty_binder ; kind ; type_ })
