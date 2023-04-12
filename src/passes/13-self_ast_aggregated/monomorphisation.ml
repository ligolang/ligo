module PP_helpers = Simple_utils.PP_helpers
module AST = Ast_aggregated
open Ligo_prim
module Row = AST.Row

let fold_map_expression = AST.Helpers.fold_map_expression
let poly_name ~loc v = Value_var.fresh_like ~loc v

module Instance = struct
  (* This is a polymorphic instance of the polymorphic function (or value) lid *)
  type t =
    { vid : Value_var.t
    ; type_instances : AST.type_expression list
    ; type_ : AST.type_expression
    }

  let pp ppf { vid; type_instances; type_ } =
    Format.fprintf
      ppf
      "{ vid = %a ; type_ = %a ; type_instances = [ %a ] }"
      Value_var.pp
      vid
      AST.PP.type_expression
      type_
      (PP_helpers.list_sep_d AST.PP.type_expression)
      type_instances
end

module Data = struct
  module LIMap = Simple_utils.Map.Make (Value_var)

  type t = Instance.t list LIMap.t

  let empty : t = LIMap.empty

  let pp ppf instances =
    let f (lid, instances_of_lid) =
      Format.fprintf
        ppf
        "{ lid = %a ~> %a }"
        Value_var.pp
        lid
        (PP_helpers.list_sep_d Instance.pp)
        instances_of_lid
    in
    List.iter (LIMap.to_kv_list instances) ~f


  let instances_lookup (ev : Value_var.t) (data : t) =
    Option.value ~default:[] @@ LIMap.find_opt ev data


  let instance_add (lid : Value_var.t) (instance : Instance.t) (data : t) =
    let lid_instances =
      instance :: (Option.value ~default:[] @@ LIMap.find_opt lid data)
    in
    LIMap.add lid lid_instances data
end

(* This is not a proper substitution, it might capture variables: it should be used only with v' a fresh variable *)
let apply_table_expr table (expr : AST.expression) =
  let apply_table_type u =
    List.fold_right table ~f:(fun (v, t) u -> AST.Helpers.subst_type v t u) ~init:u
  in
  let (), e =
    fold_map_expression
      (fun () e ->
        let loc = e.location in
        let e = { e with type_expression = apply_table_type e.type_expression } in
        let return expression_content = true, (), { e with expression_content } in
        match e.expression_content with
        | E_type_inst { forall; type_ } ->
          return @@ E_type_inst { forall; type_ = apply_table_type type_ }
        | E_lambda { binder; output_type; result } ->
          let binder = Param.map apply_table_type binder in
          let output_type = apply_table_type output_type in
          return @@ E_lambda { binder; output_type; result }
        | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
          let fun_type = apply_table_type fun_type in
          let lambda = Lambda.map Fn.id apply_table_type lambda in
          return @@ E_recursive { fun_name; fun_type; lambda; force_lambdarec }
        | E_matching { matchee; cases } ->
          let f : _ AST.Match_expr.match_case -> _ AST.Match_expr.match_case =
           fun { pattern; body } ->
            { pattern = AST.Pattern.map apply_table_type pattern; body }
          in
          return @@ E_matching { matchee; cases = List.map cases ~f }
        | E_assign { binder; expression } ->
          let binder = Binder.map apply_table_type binder in
          return @@ E_assign { binder; expression }
        | E_variable _var when AST.equal_expression e expr ->
          let _, types =
            List.fold_map ~init:e.type_expression table ~f:(fun te (v, t) ->
                let te = AST.Helpers.subst_type v t te in
                te, te)
          in
          let expr =
            List.fold2_exn
              ~init:e
              ~f:(fun e (_v, t) u -> AST.e_a_type_inst ~loc e t u)
              (List.rev table)
              types
          in
          false, (), expr
        | E_let_in { let_binder; rhs; let_result; attributes } ->
          let let_binder = AST.Pattern.map apply_table_type let_binder in
          return @@ E_let_in { let_binder; rhs; let_result; attributes }
        | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
          let let_binder = AST.Pattern.map apply_table_type let_binder in
          return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
        | E_raw_code { language; code = { expression_content = m; _ } as expr }
          when Option.is_some (AST.get_e_tuple m) ->
          let tuple = Option.value ~default:[] (AST.get_e_tuple m) in
          let code, args =
            match tuple with
            | [] -> failwith "expected non-empty tuple in %Michelson"
            | hd :: tl -> hd, tl
          in
          let args =
            List.map
              ~f:(fun t ->
                let te = t.type_expression in
                let te =
                  List.fold ~init:te table ~f:(fun te (v, t) ->
                      AST.Helpers.subst_type v t te)
                in
                { t with type_expression = te })
              args
          in
          let tuple =
            { expr with
              expression_content = E_record (Record.record_of_tuple (code :: args))
            }
          in
          return @@ E_raw_code { language; code = tuple }
        | E_deref _
        | E_while _
        | E_for _
        | E_for_each _
        | E_literal _
        | E_constant _
        | E_variable _
        | E_application _
        | E_type_abstraction _
        | E_raw_code _
        | E_constructor _
        | E_record _
        | E_accessor _
        | E_update _ -> return e.expression_content)
      ()
      expr
  in
  e


let rec subst_external_type et t (u : AST.type_expression) =
  let self = subst_external_type in
  match u.type_content with
  | T_arrow { type1; type2 } ->
    let type1 = self et t type1 in
    let type2 = self et t type2 in
    { u with type_content = T_arrow { type1; type2 } }
  | T_for_all { ty_binder; kind; type_ } ->
    let type_ = self et t type_ in
    { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_constant { injection = External _; parameters = _; _ }
    when AST.equal_type_expression et u -> t
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(self et t) parameters in
    { u with type_content = T_constant { language; injection; parameters } }
  | T_sum row ->
    let row = Row.map (self et t) row in
    { u with type_content = T_sum row }
  | T_record row ->
    let row = Row.map (self et t) row in
    { u with type_content = T_record row }
  | _ -> u


let subst_external_term et t (e : AST.expression) =
  let (), e =
    fold_map_expression
      (fun () e ->
        let e = { e with type_expression = subst_external_type et t e.type_expression } in
        let return expression_content = true, (), { e with expression_content } in
        match e.expression_content with
        | E_type_inst { forall; type_ } ->
          return @@ E_type_inst { forall; type_ = subst_external_type et t type_ }
        | E_lambda { binder; output_type; result } ->
          let binder = Param.map (subst_external_type et t) binder in
          return @@ E_lambda { binder; output_type; result }
        | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
          let fun_type = subst_external_type et t fun_type in
          return @@ E_recursive { fun_name; fun_type; lambda; force_lambdarec }
        | E_matching { matchee; cases } ->
          let f : _ AST.Match_expr.match_case -> _ AST.Match_expr.match_case =
           fun { pattern; body } ->
            { pattern = AST.Pattern.map (subst_external_type et t) pattern; body }
          in
          return @@ E_matching { matchee; cases = List.map cases ~f }
        | E_assign { binder; expression } ->
          let binder = Binder.map (subst_external_type et t) binder in
          return @@ E_assign { binder; expression }
        (* Again not applying to E_let_in ?? What? *)
        | E_let_in _
        | E_let_mut_in _
        | E_deref _
        | E_for _
        | E_while _
        | E_for_each _
        | E_literal _
        | E_constant _
        | E_variable _
        | E_application _
        | E_type_abstraction _
        | E_raw_code _
        | E_constructor _
        | E_record _
        | E_accessor _
        | E_update _ -> return e.expression_content)
      ()
      e
  in
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
  | T_constant { injection = External _; parameters = _; _ } ->
    let _, external_type_for = AST.Helpers.destruct_arrows_n typed (List.length l) in
    subst_external_term external_type external_type_for rhs
  | _ -> rhs


let rec mono_polymorphic_expression ~raise
    : Data.t -> AST.expression -> Data.t * AST.expression
  =
 fun data expr ->
  let self = mono_polymorphic_expression ~raise in
  let return ec = { expr with expression_content = ec } in
  let loc = expr.location in
  match expr.expression_content with
  | E_variable _ | E_literal _ | E_raw_code _ | E_deref _ -> data, expr
  | E_constant { cons_name; arguments } ->
    let data, arguments =
      List.fold_right arguments ~init:(data, []) ~f:(fun arg (data, args) ->
          let data, arg = self data arg in
          data, arg :: args)
    in
    data, return (E_constant { cons_name; arguments })
  | E_application { lamb; args } ->
    let data, lamb = self data lamb in
    let data, args = self data args in
    data, return (E_application { lamb; args })
  | E_lambda { binder; output_type; result } ->
    let data, result = self data result in
    data, return (E_lambda { binder; output_type; result })
  | E_type_abstraction { type_binder = _; result } ->
    raise.Trace.error
      (Errors.monomorphisation_unexpected_type_abs expr.type_expression result)
  | E_recursive
      { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec } ->
    let data, result = self data result in
    ( data
    , return
        (E_recursive
           { fun_name
           ; fun_type
           ; lambda = { binder; output_type; result }
           ; force_lambdarec
           }) )
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let () =
      match AST.Combinators.get_t_arrow rhs.type_expression with
      | Some { type1 = _; type2 } when AST.Combinators.is_t_for_all type2 ->
        raise.Trace.error
          (Errors.monomorphisation_unexpected_type_abs rhs.type_expression expr)
      | _ -> ()
    in
    let rhs =
      Trace.trace_option ~raise (Errors.monomorphisation_non_for_all rhs)
      @@ AST.Combinators.forall_expand_opt ~loc rhs
    in
    let type_vars, rhs = AST.Combinators.get_type_abstractions rhs in
    let data, let_result = self data let_result in
    let binder_instances =
      List.concat
      @@ List.map (AST.Pattern.binders let_binder) ~f:(fun binder ->
             Data.instances_lookup (Binder.get_var binder) data)
    in
    let build_let
        (lid : Value_var.t)
        Instance.{ vid; type_instances; type_ }
        (data, let_result)
      =
      let let_binder = vid in
      let table = List.zip_exn type_vars type_instances in
      let data, rhs =
        match rhs.expression_content with
        | E_recursive
            { fun_name
            ; fun_type = _
            ; lambda = { binder; output_type; result }
            ; force_lambdarec
            } ->
          let lambda =
            Lambda.
              { binder
              ; output_type
              ; result = Subst.replace (Subst.replace result fun_name vid) lid vid
              }
          in
          let data = Data.instance_add lid { vid; type_instances; type_ } data in
          ( data
          , { rhs with
              expression_content =
                E_recursive
                  { fun_name = vid
                  ; fun_type = rhs.type_expression
                  ; lambda
                  ; force_lambdarec
                  }
            } )
        | _ -> data, rhs
      in
      let rhs = apply_table_expr table rhs in
      let data, rhs = self data rhs in
      let rhs = evaluate_external_typer type_ rhs in
      let rhs = { rhs with type_expression = type_ } in
      ( data
      , AST.e_a_let_in
          ~loc
          (AST.Pattern.var ~loc (Binder.make let_binder rhs.type_expression))
          rhs
          let_result
          { attributes with hidden = false } )
    in
    let data, expr =
      match type_vars with
      | [] ->
        let data, rhs = self data rhs in
        data, return (E_let_in { let_binder; rhs; let_result; attributes })
      | _ ->
        AST.Pattern.fold_pattern
          (fun (data, instances) pattern ->
            match pattern.wrap_content with
            | P_var binder ->
              List.fold_right
                binder_instances
                ~f:(build_let (Binder.get_var binder))
                ~init:(data, let_result)
            | _ -> data, instances)
          (data, let_result)
          let_binder
    in
    data, expr
  | E_constructor { constructor; element } ->
    let data, element = self data element in
    data, return (E_constructor { constructor; element })
  | E_matching { matchee; cases } ->
    let data, cases = mono_polymorphic_cases ~raise data cases in
    let data, matchee = self data matchee in
    data, return (E_matching { matchee; cases })
  | E_record lmap ->
    let data, lmap = Record.fold_map ~f:self ~init:data lmap in
    data, return (E_record lmap)
  | E_accessor { struct_; path } ->
    let data, struct_ = self data struct_ in
    data, return (E_accessor { struct_; path })
  | E_update { struct_; path; update } ->
    let data, struct_ = self data struct_ in
    let data, update = self data update in
    data, return (E_update { struct_; path; update })
  | E_type_inst _ ->
    let rec aux type_insts (e : AST.expression) =
      match e.expression_content with
      | E_type_inst { forall; type_ } -> aux (type_ :: type_insts) forall
      | E_variable variable -> List.rev type_insts, variable
      | _ -> raise.Trace.error (Errors.monomorphisation_non_var expr)
    in
    let type_instances, lid = aux [] expr in
    let type_ = expr.type_expression in
    let vid, data =
      let vid = poly_name ~loc lid in
      vid, Data.instance_add lid { vid; type_instances; type_ } data
    in
    data, AST.e_a_variable ~loc vid type_
  | E_assign { binder; expression } ->
    let data, expression = self data expression in
    data, return (E_assign { binder; expression })
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let data, rhs = self data rhs in
    let data, let_result = self data let_result in
    data, return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_while while_loop ->
    let data, while_loop = While_loop.fold_map self data while_loop in
    data, return @@ E_while while_loop
  | E_for for_loop ->
    let data, for_loop = For_loop.fold_map self data for_loop in
    data, return @@ E_for for_loop
  | E_for_each for_each_loop ->
    let data, for_each_loop = For_each_loop.fold_map self data for_each_loop in
    data, return @@ E_for_each for_each_loop


and mono_polymorphic_cases ~raise
    :  Data.t -> _ AST.Match_expr.match_case list
    -> Data.t * _ AST.Match_expr.match_case list
  =
 fun data cases ->
  List.fold_map cases ~init:data ~f:(fun data { pattern; body } ->
      let data, body = mono_polymorphic_expression ~raise data body in
      data, ({ pattern; body } : _ AST.Match_expr.match_case))


let check_if_polymorphism_present ~raise e =
  let show_error loc = raise.Trace.error @@ Errors.polymorphism_unresolved loc in
  let rec check_type_expression ~loc (te : AST.type_expression) =
    match te.type_content with
    | T_variable _ -> show_error loc
    | T_constant { parameters; _ } ->
      List.fold_left parameters ~init:() ~f:(fun () te ->
          ignore @@ check_type_expression ~loc te)
    | T_record row | T_sum row -> Row.iter (check_type_expression ~loc) row
    | T_arrow _ -> ()
    | T_singleton _ -> ()
    | T_for_all _ -> show_error loc
  in
  let (), e =
    fold_map_expression
      (fun _ e ->
        match e.expression_content with
        | AST.E_application { args; lamb } when not (AST.is_e_raw_code lamb) ->
          check_type_expression ~loc:args.location args.type_expression;
          true, (), e
        | AST.E_constant _ ->
          check_type_expression ~loc:e.location e.type_expression;
          true, (), e
        | _ -> true, (), e)
      ()
      e
  in
  e


let mono_polymorphic_expr ~raise e =
  let e = Deduplicate_binders.program e in
  let _, m = mono_polymorphic_expression ~raise Data.empty e in
  let m = check_if_polymorphism_present ~raise m in
  m
