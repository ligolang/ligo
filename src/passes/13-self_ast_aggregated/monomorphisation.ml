module PP_helpers = Simple_utils.PP_helpers
module AST = Ast_aggregated

let fold_map_expression = Helpers.fold_map_expression

let to_name_safe v =
  fst (AST.ValueVar.internal_get_name_and_counter v)
let poly_counter = ref 0
let poly_counter_reset () = poly_counter := 0
let poly_name v = poly_counter := ! poly_counter + 1 ;
                  AST.ValueVar.of_input_var ("poly_" ^ (to_name_safe v) ^ "_" ^ string_of_int (! poly_counter))

module Instance = struct
  (* This is a polymorphic instance of the polymorphic function (or value) lid *)
  type t = { vid : AST.expression_variable ; type_instances : AST.type_expression list ; type_ : AST.type_expression }
  let pp ppf { vid ; type_instances ; type_ } =
    Format.fprintf ppf "{ vid = %a ; type_ = %a ; type_instances = [ %a ] }"
      AST.PP.expression_variable vid AST.PP.type_expression type_ (PP_helpers.list_sep_d AST.PP.type_expression) type_instances
end

module Data = struct
   module LIMap = Simple_utils.Map.Make(AST.ValueVar)

   type t = (Instance.t list) LIMap.t
   let empty : t = LIMap.empty

   let pp ppf instances =
      let f (lid, instances_of_lid) =
         Format.fprintf ppf "{ lid = %a ~> %a }"
            AST.PP.expression_variable lid
            (PP_helpers.list_sep_d Instance.pp) instances_of_lid
      in
      List.iter (LIMap.to_kv_list instances) ~f

   let instances_lookup (ev : AST.expression_variable) (data : t) =
      Option.value ~default:[] @@ LIMap.find_opt ev data

   let instance_add (lid : AST.expression_variable) (instance : Instance.t) (data : t) =
      let lid_instances = instance :: (Option.value ~default:[] @@ LIMap.find_opt lid data) in
      LIMap.add lid lid_instances data
end

let apply_table_expr table (expr : AST.expression) =
   let apply_table_type u = List.fold_right table ~f:(fun (v, t) u -> AST.Helpers.subst_type v t u) ~init:u in
   let (), e = fold_map_expression (fun () e ->
      let e = { e with type_expression = apply_table_type e.type_expression } in
      let return expression_content = (true, (), { e with expression_content }) in
      match e.expression_content with
      | E_type_inst { forall ; type_ } ->
         return @@ E_type_inst { forall ; type_ = apply_table_type type_ }
      | E_lambda { binder = { var ; ascr ; attributes } ; result } ->
         let ascr = Option.map ~f:apply_table_type ascr in
         return @@ E_lambda { binder = { var ; ascr ; attributes } ; result }
      | E_recursive { fun_name ; fun_type ; lambda } ->
         let fun_type = apply_table_type fun_type in
         return @@ E_recursive { fun_name ; fun_type ; lambda }
      | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
         return @@ E_matching { matchee ; cases = Match_variant { cases ; tv = apply_table_type tv } }
      | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
         let fields = AST.LMap.map (fun (b : _ AST.binder) -> {b with ascr = Option.map ~f:apply_table_type b.ascr}) fields in
         return @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv = apply_table_type tv } }
      | E_assign { binder = { var ; ascr ; attributes } ; expression } ->
         let ascr = Option.map ~f:apply_table_type ascr in
         return @@ E_assign { binder = { var ; ascr ; attributes } ; expression }
      | E_variable _var when AST.Compare.expression e expr = 0 ->
         let _,types = List.fold_map ~init:(e.type_expression) table ~f:(fun (te) (v,t) -> let te = AST.Helpers.subst_type v t te in te,te) in
         let expr = List.fold2_exn ~init:(e) ~f:(fun e (_v,t) u -> AST.e_a_type_inst e t u) (List.rev table) types in
         false, (), expr
      | E_literal _ | E_constant _ | E_variable _ | E_application _ | E_type_abstraction _
      | E_let_in _ | E_raw_code _ | E_constructor _ | E_record _
      | E_record_accessor _ | E_record_update _ -> return e.expression_content) () expr in
   e

let rec subst_external_type et t (u : AST.type_expression) =
   let self = subst_external_type in
   match u.type_content with
   | T_arrow {type1;type2} ->
      let type1 = self et t type1 in
      let type2 = self et t type2 in
      { u with type_content = T_arrow {type1;type2} }
   | T_for_all {ty_binder;kind;type_} ->
      let type_ = self et t type_ in
      { u with type_content = T_for_all {ty_binder;kind;type_} }
   | T_constant { injection = External _ ; parameters = _ ; _ } when AST.Helpers.type_expression_eq (et, u) ->
      t
   | T_constant {language;injection;parameters} ->
      let parameters = List.map ~f:(self et t) parameters in
      { u with type_content = T_constant {language;injection;parameters} }
   | T_sum {content; layout} ->
      let content = AST.(LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                        {associated_type = self et t associated_type; michelson_annotation;decl_pos}) content) in
      { u with type_content = T_sum {content; layout} }
   | T_record {content; layout} ->
      let content = AST.(LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                        {associated_type = self et t associated_type; michelson_annotation;decl_pos}) content) in
      { u with type_content = T_record {content; layout} }
   | _ -> u


let subst_external_term et t (e : AST.expression) =
   let (), e = fold_map_expression (fun () e ->
      let e = { e with type_expression = subst_external_type et t e.type_expression } in
      let return expression_content = (true, (), { e with expression_content }) in
      match e.expression_content with
      | E_type_inst { forall ; type_ } ->
         return @@ E_type_inst { forall ; type_ = subst_external_type et t type_ }
      | E_lambda { binder = { var ; ascr ; attributes } ; result } ->
         let ascr = Option.map ~f:(subst_external_type et t) ascr in
         return @@ E_lambda { binder = { var ; ascr ; attributes } ; result }
      | E_recursive { fun_name ; fun_type ; lambda } ->
         let fun_type =  subst_external_type et t fun_type in
         return @@ E_recursive { fun_name ; fun_type ; lambda }
      | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
         return @@ E_matching { matchee ; cases = Match_variant { cases ; tv =  subst_external_type et t tv } }
      | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
         let fields = AST.LMap.map (fun binder -> AST.{ binder with ascr = Option.map ~f:(subst_external_type et t) binder.ascr }) fields in
         return @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv = subst_external_type et t tv } }
      | E_assign { binder = { var ; ascr ; attributes } ; expression } ->
         let ascr = Option.map ~f:(subst_external_type et t) ascr in
         return @@ E_assign { binder = { var ; ascr ; attributes } ; expression }
      | E_literal _ | E_constant _ | E_variable _ | E_application _ | E_type_abstraction _
      | E_let_in _ | E_raw_code _ | E_constructor _ | E_record _
      | E_record_accessor _ | E_record_update _ -> return e.expression_content) () e in
   e

(* A term might have remaining external typer usages around. If the
   type we are monomorphising is of the form
     `t1 -> t2 -> ... -> _ external_type`
   then we will replaced `_ external_type` from the value `u` we get
   from the `typed` argument, which is of the form
     `u1 -> u2 -> ... -> u` *)
let evaluate_external_typer typed rhs =
   let l, external_type = AST.Helpers.destruct_arrows rhs.AST.type_expression in
   match external_type.type_content with
   | T_constant { injection = External _ ; parameters = _ ; _ } ->
      let _, external_type_for = AST.Helpers.destruct_arrows_n typed (List.length l) in
      subst_external_term external_type external_type_for rhs
   | _ -> rhs

let rec mono_polymorphic_expression ~raise : Data.t -> AST.expression -> Data.t * AST.expression = fun data expr ->
   let self = mono_polymorphic_expression ~raise in
   let return ec = { expr with expression_content = ec } in
   match expr.expression_content with
   | E_variable _ | E_literal _ | E_raw_code _ -> data, expr
   | E_constant { cons_name ; arguments } ->
      let data, arguments = List.fold_right arguments ~init:(data, [])
                              ~f:(fun arg (data, args) -> let data, arg = self data arg in data, arg :: args) in
      data, return (E_constant { cons_name ; arguments })
   | E_application { lamb ; args } ->
      let data, lamb = self data lamb in
      let data, args = self data args in
      data, return (E_application { lamb; args })
   | E_lambda { binder ; result } ->
      let data, result = self data result in
      data, return (E_lambda { binder ; result })
   | E_type_abstraction { type_binder ; result } ->
      ignore (type_binder,result);
      raise.Trace.error (Errors.corner_case "Monomorphisation: E_type_abstraction found in unexpected position")
   | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
      let data, result = self data result in
      data, return (E_recursive { fun_name ; fun_type ; lambda = { binder ; result } })
   | E_let_in { let_binder ; rhs ; let_result ; attr } -> (
      let type_vars, rhs = AST.Combinators.get_type_abstractions rhs in
      let data, let_result = self data let_result in
      let binder_instances = Data.instances_lookup let_binder.var data in
      let build_let (lid : AST.expression_variable) Instance.{ vid ; type_instances ; type_ } (data, let_result) =
        let let_binder = vid in
        let table = List.zip_exn type_vars type_instances in
        let data, rhs = match rhs.expression_content with
          | E_recursive { fun_name ; fun_type = _ ; lambda = { binder ; result } } ->
            let lambda = { AST.binder ; result = Subst.replace (Subst.replace result fun_name vid) lid vid } in
            let data = Data.instance_add lid { vid ; type_instances ; type_ } data in
            data, { rhs with expression_content = E_recursive { fun_name = vid ; fun_type = rhs.type_expression ; lambda } }
          | _ -> data, rhs in
        let rhs = apply_table_expr table rhs in
        let data, rhs = self data rhs in
        let rhs = evaluate_external_typer type_ rhs in
        let rhs = { rhs with type_expression = type_ } in
        data, (AST.e_a_let_in {var=let_binder;ascr=Some rhs.type_expression;attributes=Stage_common.Helpers.empty_attribute} rhs let_result {attr with hidden = false}) in
      let data, expr = match type_vars with
        | [] -> let data, rhs = self data rhs in
                data, return (E_let_in { let_binder ; rhs ; let_result ; attr })
        | _ -> List.fold_right binder_instances ~f:(build_let @@ let_binder.var) ~init:(data, let_result) in
      data, expr
   )
   | E_constructor { constructor ; element } ->
      let data, element  = self data element in
      data, return (E_constructor { constructor ; element })
   | E_matching { matchee ; cases } ->
      let data, cases = mono_polymorphic_cases ~raise data cases in
      let data, matchee = self data matchee in
      data, return (E_matching { matchee ; cases })
   | E_record lmap ->
      let data, lmap = AST.LMap.fold_map ~f:(fun _ expr data -> self data expr) lmap ~init:data in
      data, return (E_record lmap)
   | E_record_accessor { record ; path } ->
      let data, record = self data record in
      data, return (E_record_accessor { record ; path })
   | E_record_update { record ; path ; update } ->
      let data, update = self data update in
      let data, record = self data record in
      data, return (E_record_update { record ; path ; update })
   | E_type_inst _ ->
      let rec aux type_insts (e : AST.expression) = match e.expression_content with
         | E_type_inst {forall; type_} ->
            aux (type_ :: type_insts) forall
         | E_variable variable -> (
            (List.rev type_insts, variable))
         | _ -> raise.Trace.error (Errors.corner_case "Monomorphisation: cannot resolve non-variables with instantiations") in
      let type_instances, lid = aux [] expr in
      let type_ = expr.type_expression in
      let vid, data = let vid = poly_name lid in
                      vid, Data.instance_add lid { vid ; type_instances ; type_ } data in
      data, AST.e_a_variable vid type_
   | E_assign {binder;expression} ->
      let data, expression = self data expression in
      data, return (E_assign {binder;expression})

and mono_polymorphic_cases ~raise : Data.t -> AST.matching_expr -> Data.t * AST.matching_expr = fun data m ->
   match m with
   | Match_variant { tv ; cases } ->
      let aux { AST.constructor ; pattern ; body } (data, r) =
         let data, body = mono_polymorphic_expression ~raise data body in
         data, { AST.constructor ; pattern ; body} :: r in
      let data, cases = List.fold_right cases ~f:aux ~init:(data, []) in
      data, Match_variant { tv ; cases }
   | Match_record { tv ; body ; fields } ->
      let data, body = mono_polymorphic_expression ~raise data body in
      data, Match_record { tv ; body ; fields }

let check_if_polymorphism_present ~raise e =
   let show_error loc =
      raise.Trace.error @@ Errors.polymorphism_unresolved loc
   in
   let rec check_type_expression ~loc (te : AST.type_expression) =
      match te.type_content with
        T_variable _ -> show_error loc;
      | T_constant { parameters ; _ } ->
         List.fold_left parameters ~init:()
            ~f:(fun () te -> ignore @@ check_type_expression ~loc te)
      | T_record { content ; _ }
      | T_sum { content ; _ } ->
         AST.LMap.fold (fun _ (re : AST.row_element) () ->
            check_type_expression ~loc re.associated_type) content ()
      | T_arrow _ -> ()
      | T_singleton _ -> ()
      | T_for_all _ -> show_error loc;
   in
   let (), e = fold_map_expression (fun _ e ->
      match e.expression_content with
         AST.E_application { args ; lamb } when not (AST.is_e_raw_code lamb) ->
            check_type_expression ~loc:args.location args.type_expression;
            (true, (), e)
      | AST.E_constant _ ->
         check_type_expression ~loc:e.location e.type_expression;
         (true, (), e)
      | _ -> (true, (), e)) () e in
   e

let mono_polymorphic_expr ~raise e =
  let e = Deduplicate_binders.program e in
  let _, m = mono_polymorphic_expression ~raise Data.empty e in
  let m = check_if_polymorphism_present ~raise m in
  m
