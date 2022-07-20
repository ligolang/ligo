let rec decompile ~raise : Ast_aggregated.expression -> Ast_typed.expression =
  fun exp ->
  let module I = Ast_aggregated in
  let module O = Ast_typed in
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
    | E_lambda { binder ; result } ->
       let binder = Stage_common.Maps.binder (decompile_type ~raise) binder in
       let result = decompile ~raise result in
       return (O.E_lambda { binder ; result })
    | E_type_abstraction { type_binder ; result } ->
       let result = decompile ~raise result in
       return (O.E_type_abstraction { type_binder ; result })
    | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
       let fun_type = decompile_type ~raise fun_type in
       let result = decompile ~raise result in
       let binder = Stage_common.Maps.binder (decompile_type ~raise) binder in
       return (O.E_recursive { fun_name ; fun_type ; lambda = { binder ; result } })
    | E_let_in { let_binder ; rhs ; let_result ; attr } ->
       let rhs = decompile ~raise rhs in
       let let_result = decompile ~raise let_result in
       let let_binder = Stage_common.Maps.binder (decompile_type ~raise) let_binder in
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
       let tv = decompile_type ~raise tv in
       let f { I.constructor ; pattern ; body } =
         let body = decompile ~raise body in
         { O.constructor ; pattern ; body } in
       let cases = List.map ~f cases in
       return (O.E_matching { matchee ; cases = Match_variant { cases ; tv } })
    | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
       let matchee = decompile ~raise matchee in
       let tv = decompile_type ~raise tv in
       let body = decompile ~raise body in
       let f (b: _ I.binder) = {b with ascr = Option.map ~f:(decompile_type ~raise) (b.ascr) } in
       let fields = I.LMap.map f fields in
       return (O.E_matching { matchee ; cases = Match_record { fields ; body ; tv } })
    (* Record *)
    | E_record map ->
       let map = I.LMap.map (decompile ~raise) map in
       return (O.E_record map)
    | E_record_accessor { record ; path } ->
       let record = decompile ~raise record in
       return (O.E_record_accessor { record ; path })
    | E_record_update { record ; path ; update } ->
       let record = decompile ~raise record in
       let update = decompile ~raise update in
       return (O.E_record_update { record ; path ; update })
   | I.E_assign {binder;expression} ->
      let binder = Stage_common.Maps.binder (decompile_type ~raise) binder in
      let expression = decompile ~raise expression in
      return @@ O.E_assign {binder;expression}

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
  | T_sum { content ; layout } ->
     let f { I.associated_type ; michelson_annotation ; decl_pos } =
       let associated_type = decompile_type ~raise associated_type in
       { O.associated_type ; michelson_annotation ; decl_pos } in
     let content = I.LMap.map f content in
     return (O.T_sum { content ; layout })
  | T_record { content ; layout } ->
     let f { I.associated_type ; michelson_annotation ; decl_pos } =
       let associated_type = decompile_type ~raise associated_type in
       { O.associated_type ; michelson_annotation ; decl_pos } in
     let content = I.LMap.map f content in
     return (O.T_record { content ; layout })
  | T_arrow { type1 ; type2 } ->
     let type1 = decompile_type ~raise type1 in
     let type2 = decompile_type ~raise type2 in
     return (O.T_arrow { type1 ; type2 })
  | T_singleton l ->
     return (O.T_singleton l)
  | T_for_all { ty_binder ; kind ; type_ } ->
     let type_ = decompile_type ~raise type_ in
     return (O.T_for_all { ty_binder ; kind ; type_ })
