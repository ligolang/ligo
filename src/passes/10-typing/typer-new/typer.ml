open Trace
module I = Ast_core
module O = Ast_typed
module O' = Solver
open O.Combinators
module Environment = O.Environment
module Solver = Solver
type environment = Environment.t
module Errors = Typer_common.Errors
open Errors

(* TODO : find a better name for fonction that are called "type_something".
They are not typing per say, just add a type variable to all expression and make the appropriate constraints *)

let assert_type_expression_eq ((tv',tv):O.type_expression * O.type_expression) : (unit,typer_error) result =
  Compare_types.assert_type_expression_eq (tv' , tv)

let cast_var (orig: 'a Var.t Location.wrap) = { orig with wrap_content = Var.todo_cast orig.wrap_content}


module Check : sig
  val check_expression_has_no_unification_vars : O.expression -> (unit, 'a) Simple_utils.Trace.result

  val check_has_no_unification_vars : O.module_with_unification_vars -> (O.module_fully_typed, 'a) Simple_utils.Trace.result
end = struct
  let rec expression : O.expression -> _ = function ({ expression_content; location=_; type_expression } as e) ->
    let where () = Format.asprintf "expression %a which was assigned the type %a.\nHere is the annotated expression following inference:\n%a"
        O.PP.expression_content expression_content
        O.PP.type_expression type_expression
        O.PP_annotated.expression e
    in
    let%bind () = ec where expression_content in
    te where type_expression
  and ec where : O.expression_content -> _ = function
      O.E_literal (O.Literal_unit)|O.E_literal (O.Literal_int _)|O.E_literal
        (O.Literal_nat _)|O.E_literal (O.Literal_timestamp _)|O.E_literal
        (O.Literal_mutez _)|O.E_literal (O.Literal_string _)|O.E_literal
        (O.Literal_bytes _)|O.E_literal (O.Literal_address _)|O.E_literal
        (O.Literal_signature _)|O.E_literal (O.Literal_key _)|O.E_literal
        (O.Literal_key_hash _)|O.E_literal (O.Literal_chain_id _)|O.E_literal
        (O.Literal_operation _) -> ok ()
    | O.E_constant        { cons_name=_; arguments } -> bind_fold_list (fun () e -> expression e) () arguments
    | O.E_variable        _ -> ok ()
    | O.E_application     { lamb; args } -> let%bind () = expression lamb in expression args
    | O.E_lambda          { binder=_; result } -> expression result
    | O.E_recursive       { fun_name=_; fun_type; lambda={ binder=_; result } } -> let%bind () = expression result in te where fun_type
    | O.E_let_in          { let_binder=_; rhs; let_result; inline=_ } -> let%bind () = expression rhs in expression let_result
    | O.E_type_in         { type_binder=_; rhs=_; let_result} -> expression let_result
    | O.E_mod_in          { module_binder=_; rhs=_; let_result} -> expression let_result
    | O.E_mod_alias       { alias=_; binders=_; result} -> expression result
    | O.E_raw_code        { language=_; code } -> expression code
    | O.E_constructor     { constructor=_; element } -> expression element
    | O.E_matching        { matchee; cases } -> let%bind () = expression matchee in
      (match cases with
         O.Match_list    { match_nil; match_cons = { hd = _; tl = _; body; tv } } ->
         let%bind () = expression match_nil in
         let%bind () = expression body in
         te where tv
       | O.Match_option  { match_none; match_some = { opt=_; body; tv } } ->
         let%bind () = expression match_none in
         let%bind () = expression body in
         te where tv
       | O.Match_variant { cases; tv } ->
         let%bind () = bind_fold_list (fun () ({ constructor = _ ; pattern = _ ; body } : Ast_typed.matching_content_case) -> expression body) () cases in
         te where tv
       | O.Match_record { fields=_; body; tv } ->
         let%bind () = expression body in
         te where tv
      )
    | O.E_record          m -> bind_fold_list (fun () (_key, e) -> expression e) () @@ Ast_typed.LMap.bindings m
    | O.E_record_accessor { record; path=_ } -> expression record
    | O.E_record_update   { record; path=_; update } -> let%bind () = expression record in expression update
    | O.E_module_accessor { module_name=_; element} -> expression element
  and re where : O.row_element -> _ = function { associated_type; michelson_annotation=_; decl_pos=_ } ->
    te where associated_type
  and tc where : O.type_content -> _ = function
      O.T_sum      m ->
      bind_fold_list (fun () (_key, row_element) -> re where row_element) () @@ Ast_typed.LMap.bindings m.content
    | O.T_record   m ->
      bind_fold_list (fun () (_key, row_element) -> re where row_element) () @@ Ast_typed.LMap.bindings m.content
    | O.T_arrow    { type1; type2 } ->
      let%bind () = te where type1 in
      te where type2
    | O.T_variable tv -> failwith (Format.asprintf "Unassigned type variable %a cann't be generalized (LIGO does not support generalization of variables in user code for now). You can try to annotate the expression. The type variable occurred in the %s" Var.pp tv (where ()))
    | O.T_constant { parameters ; _ } ->
      bind_fold_list (fun () texpr -> te where texpr) () parameters
    | O.T_module_accessor {module_name=_; element} -> te where element
    | O.T_singleton _ -> failwith "TODO: singleton?"
  and te where : O.type_expression -> _ = function { type_content; type_meta=_; location=_ } -> tc where type_content

  let check_expression_has_no_unification_vars (expr : O.expression) =
    let print_checked p =
      Format.printf "{ \"CHECKING_EXPR\": %s\n},\n"
        (Yojson.Safe.to_string (O.Yojson.expression p)) in
    let () = (if Ast_typed.Debug.debug_new_typer || Ast_typed.Debug.json_new_typer then print_checked expr) in
    expression expr

  let check_has_no_unification_vars ((O.Module_With_Unification_Vars p) as pp) =
    let print_checked p =
      Format.printf "{ \"CHECKING\": %s\n},\n"
        (Yojson.Safe.to_string (O.Yojson.module_with_unification_vars p)) in
    let () = (if Ast_typed.Debug.debug_new_typer || Ast_typed.Debug.json_new_typer then print_checked pp) in
    let decl : O.declaration -> _ = fun d -> match d with
        O.Declaration_constant { binder=_; expr; inline=_ } -> check_expression_has_no_unification_vars expr
      | O.Declaration_type { type_binder=_; type_expr } ->
        let where () = Format.asprintf "type declaration %a" O.PP.declaration d in
        te where type_expr
      | O.Declaration_module { module_binder=_; module_=_ } ->
        let _where () = Format.asprintf "module declaration %a" O.PP.declaration d in
        ok @@ ()
      | O.Module_alias { alias=_; binders=_} ->
        let _where () = Format.asprintf "module alias %a" O.PP.declaration d in
        ok @@ () in
    let%bind () = bind_fold_list (fun () Location.{wrap_content;location=_} -> decl wrap_content) () p in
    ok @@ O.Module_Fully_Typed p
end

(*
  Extract pairs of (name,type) in the declaration and add it to the environment
*)
let rec type_declaration env state : I.declaration Location.wrap -> (environment * _ O'.typer_state * O.declaration Location.wrap, typer_error) result = fun d ->
  let return : _ -> _ -> _ O'.typer_state -> _ (* return of type_expression *) = fun expr e state constraints ->
    Format.printf "Solving expression : %a\n%!" O.PP_annotated.declaration expr ;
    let%bind state = Solver.main state constraints in
    Format.printf "Leaving type declaration\n\n%!";
    let () = Pretty_print_variables.flush_pending_print state in
    ok @@ (e,state, Location.wrap ~loc:d.location expr ) in
  Format.printf "Type_declaration : %a\n%!" I.PP.declaration (Location.unwrap d);
  match Location.unwrap d with
  | Declaration_type {type_binder; type_expr} ->
    let type_binder = Var.todo_cast type_binder in
    let%bind type_expr = evaluate_type env type_expr in
    let env' = Environment.add_type (type_binder) type_expr env in
    let c = Wrap.type_decl () in
    return (O.Declaration_type {type_binder; type_expr}) env' state c
  | Declaration_constant {name; binder; attr; expr} -> (
    (*
      Determine the type of the expression and add it to the environment
    *)
    let%bind tv_opt = bind_map_option (evaluate_type env) binder.ascr in
    Format.printf "const_decl: tv_opt : %a\n%!" (PP_helpers.option O.PP.type_expression) tv_opt ;
    let%bind (e, state', expr),constraints =
      trace (constant_declaration_tracer binder.var expr tv_opt) @@
      type_expression' env state expr in
    let binder = Location.map Var.todo_cast binder.var in
    Format.printf "Binder is %a\n%!" Ast_typed.PP.expression_variable binder ;
    let post_env = Environment.add_ez_declaration binder expr e in
    let c = Wrap.const_decl expr.type_expression tv_opt in
    return (Declaration_constant { name; binder ; expr ; inline=attr.inline}) post_env state' (constraints@c)
    )
  | Declaration_module {module_binder;module_} -> (
    let%bind (e,module_,state) = type_module ~init_env:env module_ in
    let post_env = Environment.add_module module_binder e env in
    return (Declaration_module { module_binder; module_}) post_env state @@ Wrap.mod_decl ()
  )
  | Module_alias {alias;binders} -> (
    let aux env binder =
      trace_option (unbound_module_variable env binder d.location)
      @@ Environment.get_module_opt binder env in
    let%bind e = bind_fold_ne_list aux env binders in
    let post_env = Environment.add_module alias e env in
    return (Module_alias { alias; binders}) post_env state @@ Wrap.mod_al ()
  )

(*
  Recursively search the type_expression and return a result containing the
  type_value at the leaves
*)
and evaluate_type : environment -> I.type_expression -> (O.type_expression, typer_error) result = fun e t ->
  let return tv' = ok (make_t ~loc:t.location tv' (Some t)) in
  match t.type_content with
  | T_sum {fields ; layout} ->
    let aux v =
      let {associated_type ; michelson_annotation ; decl_pos} : _ I.row_element_mini_c = v in
      let%bind associated_type = evaluate_type e associated_type in
      ok @@ ({associated_type ; michelson_annotation ; decl_pos}:_ O.row_element_mini_c)
    in
    let%bind content = Stage_common.Helpers.bind_map_lmap aux fields in
    let%bind () = trace_assert_fail_option (variant_redefined_error t.location) @@
      Environment.get_sum content e in
    let layout = Option.unopt ~default:default_layout layout in
    return (T_sum {content ; layout})
  | T_record {fields ; layout} ->
    let aux v =
      let {associated_type ; michelson_annotation ; decl_pos} : _ I.row_element_mini_c = v in
      let%bind associated_type = evaluate_type e associated_type in
      ok @@ ({associated_type ; michelson_annotation ; decl_pos}:_ O.row_element_mini_c)
    in
    let%bind content = Stage_common.Helpers.bind_map_lmap aux fields in
    (* let%bind () = trace_assert_fail_option (record_redefined_error t.location) @@
      Environment.get_record content e in *)
    let layout = Option.unopt ~default:default_layout layout in
    return (T_record {content ; layout})
  | T_variable variable ->
    (* Check that the variable is in the environment *)
    let name : O.type_variable = Var.todo_cast variable in
    trace_option (unbound_type_variable e name t.location)
      @@ Environment.get_type_opt (name) e
  | T_arrow {type1;type2} ->
    let%bind type1 = evaluate_type e type1 in
    let%bind type2 = evaluate_type e type2 in
    return (T_arrow {type1;type2})
  | T_app {type_operator;arguments} -> (
    let name : O.type_variable = Var.todo_cast type_operator in
    let%bind v = trace_option (unbound_type_variable e name t.location) @@
      Environment.get_type_opt name e in
    let aux : O.type_injection -> (O.type_expression, typer_error) result = fun inj ->
      (*handles converters*)
      let open Stage_common.Constant in
      let {language=_ ; injection ; parameters} : O.type_injection = inj in
      match Ligo_string.extract injection, parameters with
      | (i, [t]) when String.equal i michelson_pair_right_comb_name ->
        let%bind lmap = match t.type_content with
            | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap.content)) -> ok lmap
            | _ -> fail (michelson_comb_no_record t.location) in
        let record = Typer_common.Michelson_type_converter.convert_pair_to_right_comb (Ast_typed.LMap.to_kv_list_rev lmap.content) in
        return @@ record
      | (i, [t]) when String.equal i  michelson_pair_left_comb_name ->
          let%bind lmap = match t.type_content with
            | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap.content)) -> ok lmap
            | _ -> fail (michelson_comb_no_record t.location) in
          let record = Typer_common.Michelson_type_converter.convert_pair_to_left_comb (Ast_typed.LMap.to_kv_list_rev lmap.content) in
          return @@ record
      | (i, [t]) when String.equal i michelson_or_right_comb_name ->
        let%bind cmap = match t.type_content with
            | T_sum cmap -> ok cmap.content
            | _ -> fail (michelson_comb_no_variant t.location) in
          let pair = Typer_common.Michelson_type_converter.convert_variant_to_right_comb (Ast_typed.LMap.to_kv_list_rev cmap) in
          return @@ pair
      | (i, [t]) when String.equal i michelson_or_left_comb_name ->
        let%bind cmap = match t.type_content with
            | T_sum cmap -> ok cmap.content
            | _ -> fail (michelson_comb_no_variant t.location) in
          let pair = Typer_common.Michelson_type_converter.convert_variant_to_left_comb (Ast_typed.LMap.to_kv_list_rev cmap) in
          return @@ pair
      | _ -> return (T_constant inj)
    in
    match get_param_inj v with
    | Some (language,injection,parameters) ->
      let arg_env = List.length parameters in
      let arg_actual = List.length arguments in
      let%bind parameters =
        if arg_env <> arg_actual then fail @@ type_constant_wrong_number_of_arguments type_operator arg_env arg_actual t.location
        else bind_map_list (evaluate_type e) arguments
      in
      let inj : O.type_injection = {language ; injection ; parameters } in
      aux inj
    | None -> failwith "variable with parameters is not an injection"
  )
  | T_module_accessor {module_name; element} ->
    let%bind module_ = match Environment.get_module_opt module_name e with
      Some m -> ok m
    | None   -> fail @@ unbound_module_variable e module_name t.location
    in
    evaluate_type module_ element
  | T_singleton x -> return (T_singleton x)


and type_expression : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> (environment * _ O'.typer_state * O.expression, typer_error) result = fun ?tv_opt e state ae ->
  let return : _ -> _ -> _ O'.typer_state -> _ (* return of type_expression *) = fun expr e state constraints ->
    let%bind new_state = Solver.main state constraints in
    let () = Pretty_print_variables.flush_pending_print state in
    ok @@ (e,new_state, expr) in
  let%bind ((e,state,expr),constraints) = type_expression' ?tv_opt e state ae in
  return expr e state constraints

and type_expression' : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> ((environment * _ O'.typer_state * O.expression) * _, typer_error) result = fun ?tv_opt e state ae ->
  let self = type_expression' in
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  let module L = Logger.Stateful() in
  let return : _ -> _ -> _ O'.typer_state -> _ -> _ (* return of type_expression *) = fun expr e state constraints type_name ->
    let tv = t_variable type_name in
    let location = ae.location in
    let expr' = make_e ~location expr tv in
    ok @@ ((e,state, expr'),constraints) in
  let return_wrapped expr e state constraints (c , expr_type) = return expr e state (c@constraints) expr_type in
  Format.printf "Type_expression : %a\n%!" Ast_core.PP.expression ae ;
  trace (expression_tracer ae) @@
  match ae.content with

  (* TODO: this file should take care only of the order in which module fragments
     are translated by Wrap.xyz

     TODO: produce an ordered list of sub-fragments, and use a common piece of code
     to actually perform the recursive calls *)

  (* Basic *)
  | E_variable name -> (
    (* Check that the variable exist in the environment and add a new constraint *)
    let%bind (tv' : Environment.element) =
      trace_option (unbound_variable e name ae.location)
      @@ Environment.get_opt name e in
    let (constraints , expr_type) = Wrap.variable name tv'.type_value in
    let expr' = e_variable name in
    return expr' e state constraints expr_type
  )

  | E_literal (Literal_string s) -> (
      return_wrapped (e_string s) e state [] @@ Wrap.literal "string" (t_string ())
    )
  | E_literal (Literal_signature s) -> (
      return_wrapped (e_signature s) e state [] @@ Wrap.literal "signature" (t_signature ())
    )
  | E_literal (Literal_key s) -> (
      return_wrapped (e_key s) e state [] @@ Wrap.literal "key" (t_key ())
    )
  | E_literal (Literal_key_hash s) -> (
      return_wrapped (e_key_hash s) e state [] @@ Wrap.literal "key_hash" (t_key_hash ())
    )
  | E_literal (Literal_chain_id s) -> (
      return_wrapped (e_chain_id s) e state [] @@ Wrap.literal "key_hash" (t_chain_id ())
    )
  | E_literal (Literal_bytes b) -> (
      return_wrapped (e_bytes b) e state [] @@ Wrap.literal "bytes" (t_bytes ())
    )
  | E_literal (Literal_int i) -> (
      return_wrapped (e_int i) e state [] @@ Wrap.literal "int" (t_int ())
    )
  | E_literal (Literal_nat n) -> (
      return_wrapped (e_nat n) e state [] @@ Wrap.literal "nat" (t_nat ())
    )
  | E_literal (Literal_mutez t) -> (
      return_wrapped (e_mutez t) e state [] @@ Wrap.literal "mutez" (t_mutez ())
    )
  | E_literal (Literal_address a) -> (
      return_wrapped (e_address a) e state [] @@ Wrap.literal "address" (t_address ())
    )
  | E_literal (Literal_timestamp t) -> (
      return_wrapped (e_timestamp t) e state [] @@ Wrap.literal "timestamp" (t_timestamp ())
    )
  | E_literal (Literal_operation o) -> (
      return_wrapped (e_operation o) e state [] @@ Wrap.literal "operation" (t_operation ())
    )
  | E_literal (Literal_unit) -> (
      return_wrapped (e_unit ()) e state [] @@ Wrap.literal "unit" (t_unit ())
    )

  | E_constant {cons_name; arguments=lst} ->
    let%bind t = Typer_common.Constant_typers_new.Operators_types.constant_type cons_name in
    let%bind (e,state,constraints),lst = bind_fold_map_list
      (fun (e,state, c) l ->
        let%bind (e,state,l), constraints = self e state l in
        ok ((e,state, c @ constraints),l)
      ) (e,state,[]) lst
    in
    let lst_annot = List.map get_type_expression lst in
    let wrapped = Wrap.constant cons_name t lst_annot in
    return_wrapped (E_constant {cons_name;arguments=lst}) e state constraints wrapped

  | E_lambda lambda ->
    let%bind lambda,e,state,constraints,wrapped = type_lambda e state lambda in
    return_wrapped (E_lambda lambda) e state constraints wrapped

  | E_application {lamb;args} ->
    let%bind (e,state,lamb'),c1 = self e state lamb in
    let%bind (e,state,args),c2 = self e state args in
    let wrapped = Wrap.application lamb lamb'.type_expression args.type_expression in
    return_wrapped (E_application {lamb=lamb';args}) e state (c1@c2) wrapped

  (* Sum *)
  | E_constructor {constructor;element} ->
    (* Check that the constructor is from an existing variant *)
    let%bind (c_tv, sum_tv) = trace_option (unbound_constructor e constructor ae.location) @@
      Environment.get_constructor constructor e in
    let%bind (e,state, element),constraints = self e state element in
    (* Check that the element in the variant as the proper type *)
    (* TODO: infer the variant or the type of the element ?*)
    let wrapped = Wrap.constructor element.type_expression c_tv sum_tv in
    return_wrapped (E_constructor {constructor; element}) e state constraints wrapped

  | E_matching {matchee;cases} -> (
      let%bind (e,state,ex'),constraints = self e state matchee in
      let%bind (e,state,m'),c_match  = type_match e state ex'.type_expression cases ae ae.location in
      let tvs =
        let aux (cur : O.matching_expr) =
          match cur with
          | Match_list { match_nil ; match_cons = { hd=_ ; tl=_ ; body ; tv=_} } -> [ match_nil ; body ]
          | Match_option { match_none ; match_some = {opt=_; body; tv=_} } -> [ match_none ; body ]
          | Match_variant { cases ; tv=_ } -> List.map (fun ({constructor=_; pattern=_; body} : O.matching_content_case) -> body) cases
          | Match_record { fields=_; body; tv=_ } -> [ body ] in
        List.map get_type_expression @@ aux m' in
      (* constraints:
         all the items of tvs should be equal to the first one
         result = first item of tvs
      *)
      (* ?TODO?: Check that the type of the matching has the variant type corresponding to the case *)
      let wrapped = Wrap.matching tvs in
      return_wrapped (O.E_matching {matchee=ex';cases=m'}) e state (constraints@c_match) wrapped
    )

  (* Record *)
  | E_record m ->
    let aux (e,state,c) _ expr =
      let%bind (e,state,expr),constraints = self e state expr in
      ok ((e,state,constraints@c), expr)
    in
    let%bind (e,state',constraints), m' = Stage_common.Helpers.bind_fold_map_lmap aux (e,state,[]) m in
    (* Do we need row_element for AST_typed ? *)
    let lmap = O.LMap.map (fun e -> ({associated_type = get_type_expression e ; michelson_annotation = None ; decl_pos = 0}: O.row_element)) m' in
    let record_type = match Environment.get_record lmap e with
      | None -> O.{content=lmap;layout=default_layout}
      | Some (_,r) -> r
    in
    let wrapped = Wrap.record record_type in
    return_wrapped (E_record m') e state' constraints wrapped

  | E_record_accessor {record;path} -> (
      let%bind (e,state,base),constraints = self e state record in
      let wrapped = Wrap.access_label ~base:base.type_expression ~label:path in
      return_wrapped (E_record_accessor {record=base;path}) e state constraints wrapped
    )

  | E_record_update {record; path; update} ->
    let%bind (e,state,record),c1 = self e state record in
    let%bind (e,state,update),c2 = self e state update in
    let t_record = get_type_expression record in
    (* TODO: wrap.record_update *)
    let wrapped = Wrap.record_update  ~base:t_record ~label:path @@ get_type_expression update in
    return_wrapped (E_record_update {record; path; update}) e state (c1@c2) wrapped

  (* Advanced *)
  | E_let_in {let_binder ; rhs ; let_result; inline} ->
    let%bind rhs_tv_opt = bind_map_option (evaluate_type e) (let_binder.ascr) in
    let%bind (e,state,rhs),c1 = self e state rhs in
    let let_binder = cast_var let_binder.var in
    let e = Environment.add_ez_declaration let_binder rhs e in
    let%bind (e,state,let_result),c2 = self e state let_result in
    let wrapped = Wrap.let_in rhs.type_expression rhs_tv_opt let_result.type_expression in
    return_wrapped (E_let_in {let_binder; rhs; let_result; inline}) e state (c1@c2) wrapped

  | E_type_in {type_binder; rhs ; let_result} ->
    let%bind rhs = evaluate_type e rhs in
    let e = Environment.add_type type_binder rhs e in
    let%bind (e,state,let_result),constraints = self e state let_result in
    let wrapped = Wrap.type_in let_result.type_expression in
    return_wrapped (E_type_in {type_binder; rhs; let_result}) e state constraints wrapped

  | E_mod_in {module_binder; rhs ; let_result} ->
    let%bind (env,rhs,_) = type_module ~init_env:e rhs in
    let e = Environment.add_module module_binder env e in
    let%bind (e,state,let_result),constraints = self e state let_result in
    let wrapped =
      Wrap.mod_in let_result.type_expression in
    return_wrapped (E_mod_in {module_binder; rhs; let_result}) e state constraints wrapped

  | E_mod_alias {alias; binders; result} ->
    let aux e binder =
      trace_option (unbound_module_variable e binder ae.location) @@
      Environment.get_module_opt binder e in
    let%bind env = bind_fold_ne_list aux e binders in
    let e = Environment.add_module alias env e in
    let%bind (e,state,result),constraints = self e state result in
    let wrapped =
      Wrap.mod_alias result.type_expression in
    return_wrapped (E_mod_alias {alias; binders; result}) e state constraints wrapped

  | E_recursive {fun_name;fun_type;lambda} ->
    (* Add the function name to the environment before evaluating the lambda*)
    let fun_name = cast_var fun_name in
    let%bind fun_type = evaluate_type e fun_type in
    let e = Environment.add_ez_binder fun_name fun_type e in
    let%bind lambda,e,state,constraints,_ = type_lambda e state lambda in
    let wrapped = Wrap.recursive fun_type in
    return_wrapped (E_recursive {fun_name;fun_type;lambda}) e state constraints wrapped

  | E_raw_code {language ; code} ->
    (* The code is a string with an annotation*)
    let%bind (e,state,code),constraints = self e state code in
    let c,tv = Wrap.raw_code code.type_expression in
    return (E_raw_code {language; code}) e state (c@constraints) tv
  | E_ascription {anno_expr;type_annotation} ->
    let%bind tv = evaluate_type e type_annotation in
    let%bind (e,state,expr'),constraints = self e state anno_expr in
    let wrapped = Wrap.annotation expr'.type_expression tv
    (* TODO: we're probably discarding too much by using expr'.expression.
       Previously: {expr' with type_annotation = the_explicit_type_annotation}
       but then this case is not like the others and doesn't call return_wrapped,
       which might do some necessary work *)
    in return_wrapped expr'.expression_content e state constraints wrapped
  | E_module_accessor {module_name; element} ->
    let%bind module_env = match Environment.get_module_opt module_name e with
      Some m -> ok m
    | None   -> fail @@ unbound_module_variable e module_name ae.location
    in
    let%bind (e,state,element),constraints = self ?tv_opt module_env state element in
    let wrapped = Wrap.module_access element.type_expression in
    return_wrapped (E_module_accessor {module_name; element}) e state constraints wrapped

and type_lambda e state {
      binder ;
      output_type ;
      result ;
    } =
      let%bind input_type'  = bind_map_option (evaluate_type e) binder.ascr in
      let%bind output_type' = bind_map_option (evaluate_type e) output_type in
      let binder = cast_var binder.var in

      let fresh : O.type_expression = t_variable (Wrap.fresh_binder ()) in
      let e' = Environment.add_ez_binder (binder) fresh e in

      let%bind (e, state', result),constraints = type_expression' e' state result in
      let wrapped = Wrap.lambda fresh input_type' output_type' result.type_expression in
      ok (({binder;result}:O.lambda),e,state',constraints,wrapped)

(* TODO: Rewrite this entire function *)
and type_match : environment -> _ O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> ((environment * _ O'.typer_state * O.matching_expr) * _, typer_error) result =
  fun e state t i _ae loc ->
  let self = type_expression' in
  let return e state c m = ok @@ ((e,state, m),c) in
  match i with
    | Match_option {match_none ; match_some} ->
      let%bind tv =
        trace_option (match_error ~expected:i ~actual:t loc)
        @@ get_t_option t in
      let%bind (e,state, match_none),c1 = self e state match_none in
      let {opt; body}:I.match_some = match_some in
      let opt = cast_var opt in
      (* Add the binder just for typing the case *)
      let e = Environment.add_ez_binder opt tv e in
      let%bind (e,state, body),c2 = type_expression' e state body in
      return e state (c1@c2) @@ O.Match_option {match_none ; match_some = { opt; body; tv}}
    | Match_list {match_nil ; match_cons} ->
      let%bind t_elt =
        trace_option (match_error ~expected:i ~actual:t loc)
        @@ get_t_list t in
      let%bind (e,state,match_nil),c1 = self e state match_nil in
      let {hd; tl; body} : I.match_cons = match_cons in
      let hd = cast_var hd in
      let tl = cast_var tl in
      (* Add the binder just for typing the case *)
      let e = Environment.add_ez_binder hd t_elt e in
      let e = Environment.add_ez_binder tl t e in
      let%bind (e,state,body),c2 = self e state body in
      return e state (c1@c2) @@ O.Match_list {match_nil ; match_cons = {hd; tl; body;tv=t}}
    | Match_variant lst ->
      (* Compile the expression in the matching and check for type equality*)
      let%bind variant_opt =
        let aux acc ({constructor;_ }: I.match_variant) =
          let%bind (_ , variant) =
            trace_option (unbound_constructor e constructor loc) @@
            Environment.get_constructor constructor e in
          let%bind acc = match acc with
            | None -> ok (Some variant)
            | Some variant' ->
                let%bind () =
                  assert_type_expression_eq (variant , variant') in
                ok (Some variant)
            in
          ok acc in
        trace (in_match_variant_tracer i) @@
        bind_fold_list aux None lst in
      let variant =
        (* TODO : Front-end should check that the list is not empty.
        Rewrite the check without the option. *)
        Option.unopt_exn @@ variant_opt in
      (* Check that the matching contains the same number of case as the number of constructors of the variants
        and that all the constructors belong to the variants. *)
      (* TODO : check that all given constructors belongs to a variant. Throw an error If one constructor is not part of
      the variant, a warning if the variant is not complete or multiple variant are infered *)
      let%bind () =
        let%bind variant_cases' =
          trace_option (match_error ~expected:i ~actual:t loc)
          @@ Ast_typed.Combinators.get_t_sum variant in
        let variant_cases = List.map fst @@ O.LMap.to_kv_list_rev variant_cases'.content in
        let match_cases = List.map (fun ({constructor;_} : I.match_variant) -> constructor) lst in
        let test_case = fun c ->
          Assert.assert_true (corner_case "match case") (List.mem c match_cases)
        in
        let%bind () =
          trace_strong (match_missing_case variant_cases match_cases loc) @@
          bind_iter_list test_case variant_cases in
        let%bind () = Assert.assert_true (match_extra_case variant_cases match_cases loc) @@
          List.(length variant_cases = length match_cases) in
        ok ()
      in
      (* Convert constructors *)
      let%bind (e, state, c), cases =
        let aux (e,state,c) ({constructor; proj; body}: I.match_variant) =
          let%bind (constructor_type , _) =
            trace_option (unbound_constructor e constructor loc) @@
            Environment.get_constructor constructor e in
          let pattern = cast_var proj in
          let e = Environment.add_ez_binder pattern constructor_type e in
          let%bind (e,state,body),constraints = self e state body in
          let constructor = constructor in
          ok ((e, state,constraints@c) , ({constructor ; pattern ; body} : O.matching_content_case))
        in
        bind_fold_map_list aux (e,state,[]) lst in
      return e state c @@ O.Match_variant {cases ; tv=variant }
  (* This one seems to be working *)
  | Match_record {fields ; body } ->
    let aux e _ ({var;ascr} : I.type_expression I.binder) =
      let%bind ty_opt = bind_map_option (evaluate_type e)  ascr in
      let ty = Option.unopt ~default:(O.t_variable @@ Var.fresh ()) ty_opt in
      let e  = Environment.add_ez_binder var ty e in
      ok @@ (e,(var,ty)) 
    in
    let%bind (e', fields) = Stage_common.Helpers.bind_fold_map_lmap aux e fields in
    let%bind (e,state,body),c = self e' state body in
    return e state c @@ O.Match_record {fields ; body ; tv = t}

(* Apply type_declaration on every node of the AST_core from the root p *)
and type_module_returns_env ((env, state, p) : environment * _ O'.typer_state * I.module_) : (environment * _ O'.typer_state * O.module_with_unification_vars, Typer_common.Errors.typer_error) result =
  let aux ((e : environment), (s : _ O'.typer_state) , (ds : O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let%bind (e , s' , d') = type_declaration_subst e s d in
    (* TODO: Move this filter to the spiller *)
    let ds' = match Location.unwrap d' with
      | O.Declaration_type _ -> ds
      | _ -> d' :: ds
    in
    ok (e , s' , ds')
  in
  let%bind (env' , state , declarations) =
    trace (module_error_tracer p) @@
    bind_fold_list aux (env , state , []) p in
  let declarations = List.rev declarations in (* Common hack to have O(1) append: prepend and then reverse *)
  ok (env', state, O.Module_With_Unification_Vars declarations)

and print_env_state_node : type a. (Format.formatter -> a -> unit) -> (environment * _ O'.typer_state * a) -> unit =
  fun node_printer (env,state,node) ->
  ignore node; (* TODO *)
  Printf.printf "%s" @@
    Format.asprintf "{ \"ENV\": %s,\n\"STATE\": %s,\n\"NODE\": %a\n},\n"
      (Yojson.Safe.to_string (Ast_typed.Yojson.environment env))
      (Yojson.Safe.to_string (Solver.json_typer_state state))
      node_printer node

and _get_alias variable aliases =
  trace_option (corner_case (Format.asprintf "can't find alias root of variable %a" Var.pp variable)) @@
  (* TODO: after upgrading UnionFind, this will be an option, not an exception. *)
  try Some (Solver.UF.repr variable aliases) with Not_found -> None

and type_and_subst : type a b.
      (Format.formatter -> a -> unit) ->
      (Format.formatter -> b -> unit) ->
      (environment * _ O'.typer_state * a) ->
      ((b , Typer_common.Errors.typer_error) Typesystem.Misc.Substitution.Pattern.w) ->
      ((environment * _ O'.typer_state * a) -> (environment * _ O'.typer_state * b , typer_error) Trace.result) ->
      (b * _ O'.typer_state * environment , typer_error) result =
  fun in_printer out_printer env_state_node apply_substs types_and_returns_env ->
  let () = (if Ast_typed.Debug.json_new_typer then Printf.printf "%!\n###############################START_OF_JSON\n[%!") in
  let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%!\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env_state_node here.\n\n%!") in
  let () = (if Ast_typed.Debug.debug_new_typer && Ast_typed.Debug.json_new_typer then print_env_state_node in_printer env_state_node) in
  let%bind (env, state, node) = types_and_returns_env env_state_node in
  let%bind node,env =
    Format.printf "Substitutions ongoing\n%!";
    let aliases = state.aliases in
    let assignments = state.plugin_states#assignments in
    let substs : variable: O.type_variable -> _ = fun ~variable ->
      to_option @@
      let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%s" @@ Format.asprintf "Looking up var  %a\n" Var.pp variable) in
      let%bind root = Solver.get_alias variable aliases in
      let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%s" @@ Format.asprintf "Looking up var  %a (its root is %a)\n" Var.pp variable Var.pp root) in
      let%bind assignment =
        trace_option (corner_case (Format.asprintf "can't find assignment for root %a%!" Var.pp root)) @@
          (Database_plugins.All_plugins.Assignments.find_opt root assignments) in
      match assignment with
      | `Constructor { tv ; c_tag ; tv_list } ->
        let%bind tv_root = Solver.get_alias tv aliases in
        (* let () = Format.printf "\ncstr : %a(was %a) %a(was %a)\n" Ast_typed.PP.type_variable tv_root Ast_typed.PP.type_variable tv Ast_typed.PP.type_variable root Ast_typed.PP.type_variable variable in *)
        let () = assert (Var.equal tv_root root) in
        let%bind (expr : O.type_content) = trace_option (corner_case "wrong constant tag") @@
        Typesystem.Core.type_expression'_of_simple_c_constant (c_tag , (List.map O.t_variable tv_list)) in
        let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%s%!" @@ Format.asprintf "Substituing var %a (%a is %a)\n%!" Var.pp variable Var.pp root Ast_typed.PP.type_content expr) in
        ok @@ expr
      | `Row { tv ; r_tag ; tv_map ; reason_row_simpl=_ } ->
        let%bind tv_root = Solver.get_alias tv aliases in
        (* let () = Format.printf "\ncstr : %a(was %a) %a(was %a)\n" Ast_typed.PP.type_variable tv_root Ast_typed.PP.type_variable tv Ast_typed.PP.type_variable root Ast_typed.PP.type_variable variable in *)
        let () = assert (Var.equal tv_root root) in
        let (expr : O.type_content) = Typesystem.Core.type_expression'_of_simple_c_row (r_tag , tv_map) in
        let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%s%!" @@ Format.asprintf "Substituing var %a (%a is %a)\n%!" Var.pp variable Var.pp root Ast_typed.PP.type_content expr) in
        ok @@ expr
    in
    Format.printf "substituting node\n%!";
    let%bind node = apply_substs ~substs node in
    Format.printf "substituting env %a\n%!" Ast_typed.PP.environment env;
    let%bind env  = Typesystem.Misc.Substitution.Pattern.s_environment ~substs env in
    ok (node,env)
  in
  Format.printf "Substritutions done\n%!";
  let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env,state,node here again.\n\n") in
  let () = (if Ast_typed.Debug.debug_new_typer && Ast_typed.Debug.json_new_typer then print_env_state_node out_printer (env, state, node)) in
  ok (node, state, env)

and type_declaration_subst env _state decl = 
  let empty_state = Solver.initial_state in
  let%bind (d, state, e) = type_and_subst
      (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.declaration\"")
      (fun ppf p -> Format.fprintf ppf "%s" (Yojson.Safe.to_string (Ast_typed.Yojson.declaration @@ Location.unwrap p)))
      (env , empty_state , decl)
      Typesystem.Misc.Substitution.Pattern.s_declaration_wrap
      (fun (a,b,c) -> type_declaration a b c) in
  ok @@ (e, state, d)

and type_module ~init_env (p : I.module_) : (environment * O.module_fully_typed * _ O'.typer_state, typer_error) result =
  let empty_state = Solver.initial_state in
  let%bind (p, state, env) = type_and_subst
    (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.module\"")
    (fun ppf p -> Format.fprintf ppf "%s" (Yojson.Safe.to_string (Ast_typed.Yojson.module_with_unification_vars p)))
    (init_env , empty_state , p)
    Typesystem.Misc.Substitution.Pattern.s_module
    type_module_returns_env in
  Format.printf "Charcking for uni_vars\n%!";
  let%bind p = Check.check_has_no_unification_vars p in
  let () = (if Ast_typed.Debug.json_new_typer then Printf.printf "%!\"end of JSON\"],\n###############################END_OF_JSON\n%!") in
  let () = Pretty_print_variables.flush_pending_print state in
  Format.printf "module typed\n\n%!";
  ok (env, p, state)

and type_expression_subst (env : environment) (state : _ O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (O.environment * O.expression * _ O'.typer_state , typer_error) result =
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  let%bind (expr, state, env) = type_and_subst
      (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.expression\"")
      (fun ppf p -> Format.fprintf ppf "%s" (Yojson.Safe.to_string (Ast_typed.Yojson.expression p)))
      (env , state , e)
      Typesystem.Misc.Substitution.Pattern.s_expression
      (fun (a,b,c) -> type_expression a b c) in
  let%bind () = Check.check_expression_has_no_unification_vars expr in
  let () = (if Ast_typed.Debug.json_new_typer then Printf.printf "%!\"end of JSON\"],\n###############################END_OF_JSON\n%!") in
  let () = Pretty_print_variables.flush_pending_print state in
  ok (env, expr, state)

let untype_expression       = Untyper.untype_expression

(* These aliases are just here for quick navigation during debug, and can safely be removed later *)
let [@warning "-32"] (*rec*) type_declaration _env _state : I.declaration Location.wrap -> (environment * _ O'.typer_state * O.declaration Location.wrap, typer_error) result = type_declaration _env _state
and [@warning "-32"] type_match : environment -> _ O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> ((environment * _ O'.typer_state * O.matching_expr) * _, typer_error) result = type_match
and [@warning "-32"] evaluate_type (e:environment) (t:I.type_expression) : (O.type_expression, typer_error) result = evaluate_type e t
and [@warning "-32"] type_expression : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> (environment * _ O'.typer_state * O.expression, typer_error) result = type_expression
and [@warning "-32"] type_lambda e state lam = type_lambda e state lam
let [@warning "-32"] type_module_returns_env ((env, state, p) : environment * _ O'.typer_state * I.module_) : (environment * _ O'.typer_state * O.module_with_unification_vars, typer_error) result = type_module_returns_env (env, state, p)
let [@warning "-32"] type_and_subst (in_printer : (Format.formatter -> 'a -> unit)) (out_printer : (Format.formatter -> 'b -> unit)) (env_state_node : environment * _ O'.typer_state * 'a) (apply_substs : ('b,typer_error) Typesystem.Misc.Substitution.Pattern.w) (types_and_returns_env : (environment * _ O'.typer_state * 'a) -> (environment * _ O'.typer_state * 'b, typer_error) result) : ('b * _ O'.typer_state, typer_error) result =
  let%bind (_,s,p) = type_and_subst in_printer out_printer env_state_node apply_substs types_and_returns_env in
  ok (p,s)
let [@warning "-32"] type_module ~init_env (p : I.module_) : (environment * O.module_fully_typed * _ O'.typer_state, typer_error) result = type_module ~init_env p
let [@warning "-32"] type_expression_subst (env : environment) (state : _ O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (environment * O.expression * _ O'.typer_state, typer_error) result = type_expression_subst env state ?tv_opt e
