open Trace
module I = Ast_core
module O = Ast_core
module O' = Solver
open O.Combinators
module Environment = O.Environment
module Solver = Solver
type environment = Environment.t
module Errors = Typer_common.Errors
open Errors

(* TODO : find a better name for fonction that are called "type_something".
They are not typing per say, just add a type variable to all expression and make the appropriate constraints *)

let _assert_type_expression_eq ((tv',tv):O.type_expression * O.type_expression) : (unit,typer_error) result =
  Compare_types.assert_type_expression_eq (tv' , tv)

let cast_var (orig: 'a Var.t Location.wrap) = { orig with wrap_content = Var.todo_cast orig.wrap_content}


module Check : sig
  val check_expression_has_no_unification_vars : O.expression -> (unit, 'a) Simple_utils.Trace.result

  val check_has_no_unification_vars : O.module_with_unification_vars -> (O.module_, 'a) Simple_utils.Trace.result
end = struct
  let rec expression : O.expression -> _ = function ({ expression_content; location=_; sugar=_ }) ->
    let where () = Format.asprintf "expression %a which was assigned the type TODO.\nHere is the annotated expression following inference:\n TODO"
        O.PP.expression_content expression_content
    in
    ec where expression_content
  and ec where : O.expression_content -> _ = function
      O.E_literal (O.Literal_unit)|O.E_literal (O.Literal_int _)|O.E_literal
        (O.Literal_nat _)|O.E_literal (O.Literal_timestamp _)|O.E_literal
        (O.Literal_mutez _)|O.E_literal (O.Literal_string _)|O.E_literal
        (O.Literal_bytes _)|O.E_literal (O.Literal_address _)|O.E_literal
        (O.Literal_signature _)|O.E_literal (O.Literal_key _)|O.E_literal
        (O.Literal_key_hash _)|O.E_literal (O.Literal_chain_id _)|O.E_literal
        (O.Literal_operation _) -> ok ()
    | O.E_constant        { cons_name = _; arguments } -> 
      bind_fold_list (fun () e -> expression e) () arguments
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
         O.Match_list    { match_nil; match_cons = { hd = _; tl = _; body} } ->
         let%bind () = expression match_nil in
         expression body
       | O.Match_option  { match_none; match_some = { opt=_; body } } ->
         let%bind () = expression match_none in
         expression body
       | O.Match_variant cases ->
         bind_fold_list (fun () ({ constructor = _ ; proj = _ ; body } : Ast_core.match_variant) -> expression body) () cases
       | O.Match_record { fields=_; body } ->
         expression body
      )
    | O.E_record          m -> bind_fold_list (fun () (_key, e) -> expression e) () @@ Ast_core.LMap.bindings m
    | O.E_record_accessor { record; path=_ } -> expression record
    | O.E_record_update   { record; path=_; update } -> let%bind () = expression record in expression update
    | O.E_module_accessor { module_name=_; element} -> expression element
    | O.E_ascription      { anno_expr; type_annotation} -> let%bind () = expression anno_expr in te where type_annotation
  and re where : O.row_element -> _ = function { associated_type; michelson_annotation=_; decl_pos=_ } ->
    te where associated_type
  and tc where : O.type_content -> _ = function
      O.T_sum      m ->
      bind_fold_list (fun () (_key, row_element) -> re where row_element) () @@ Ast_core.LMap.bindings m.fields
    | O.T_record   m ->
      bind_fold_list (fun () (_key, row_element) -> re where row_element) () @@ Ast_core.LMap.bindings m.fields
    | O.T_arrow    { type1; type2 } ->
      let%bind () = te where type1 in
      te where type2
    | O.T_variable tv -> failwith (Format.asprintf "Unassigned type variable %a cann't be generalized (LIGO does not support generalization of variables in user code for now). You can try to annotate the expression. The type variable occurred in the %s" Var.pp tv (where ()))
    | O.T_app { arguments ; _ } ->
      bind_fold_list (fun () texpr -> te where texpr) () arguments
    | O.T_module_accessor {module_name=_; element} -> te where element
    | O.T_singleton _ -> failwith "TODO: singleton?"
  and te where : O.type_expression -> _ = function { type_content; sugar=_; location=_ } -> tc where type_content

  let check_expression_has_no_unification_vars (expr : O.expression) =
    let print_checked p =
      Format.eprintf "{ \"CHECKING_EXPR\": %s\n},\n"
        (Yojson.Safe.to_string (O.Yojson.expression p)) in
    let () = (if Ast_core.Debug.debug_new_typer || Ast_core.Debug.json_new_typer then print_checked expr) in
    expression expr

  let check_has_no_unification_vars ((O.Module_With_Unification_Vars p) as pp) =
    let print_checked p =
      Format.eprintf "{ \"CHECKING\": %s\n},\n"
        (Yojson.Safe.to_string (O.Yojson.module_with_unification_vars p)) in
    let () = (if Ast_core.Debug.debug_new_typer || Ast_core.Debug.json_new_typer then print_checked pp) in
    let decl : O.declaration -> _ = fun d -> match d with
        O.Declaration_constant { binder=_; expr; attr=_ } -> check_expression_has_no_unification_vars expr
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
    ok @@ p
end

(*
  Extract pairs of (name,type) in the declaration and add it to the environment
*)
let rec type_declaration env state : I.declaration Location.wrap -> (environment * _ O'.typer_state * O.declaration Location.wrap * O.type_expression, typer_error) result = fun d ->
  let return : O.declaration -> _ -> _ -> _ O'.typer_state -> _ (* return of type_expression *) = fun expr ty e state constraints ->
    (* Format.eprintf "Solving expression : %a\n%!" O.PP_annotated.declaration expr ; *)
    let%bind state = Solver.main state constraints in
    Format.eprintf "Leaving type declaration\n\n%!";
    let () = Pretty_print_variables.flush_pending_print state in
    ok @@ (e,state, Location.wrap ~loc:d.location expr, ty ) in
  Format.eprintf "Type_declaration : %a\n%!" I.PP.declaration (Location.unwrap d);
  Format.eprintf "env : %a\n" O.PP.environment env ;
  match Location.unwrap d with
  | Declaration_type {type_binder; type_expr} ->
    let type_binder = Var.todo_cast type_binder in
    let%bind type_expr = evaluate_type env type_expr in
    let env' = Environment.add_type (type_binder) type_expr env in
    let c = Wrap.type_decl () in
    return (O.Declaration_type {type_binder; type_expr}) type_expr env' state c
  | Declaration_constant {name; binder; attr={inline}; expr} -> (
    (*
      Determine the type of the expression and add it to the environment
    *)
    let%bind tv_opt = bind_map_option (evaluate_type env) binder.ascr in
    Format.eprintf "const_decl: tv_opt : %a\n%!" (PP_helpers.option O.PP.type_expression) tv_opt ;
    let%bind (e, state', expr, t),constraints =
      trace (constant_declaration_tracer binder.var expr tv_opt) @@
      type_expression' env state expr in
    let%bind binder = Stage_common.Maps.binder (evaluate_type env)  binder in
    let post_env = Environment.add_ez_declaration binder.var expr t e in
    let c = Wrap.const_decl t tv_opt in
    return (Declaration_constant { name; binder ; expr ; attr={inline}}) t post_env state' (constraints@c)
    )
  | Declaration_module {module_binder;module_} -> (
    let%bind (e,module_,t,state) = type_module ~init_env:env module_ in
    let post_env = Environment.add_module module_binder e env in
    return (Declaration_module { module_binder; module_}) t post_env state @@ Wrap.mod_decl ()
  )
  | Module_alias {alias;binders} -> (
    let aux env binder =
      trace_option (unbound_module_variable env binder d.location)
      @@ Environment.get_module_opt binder env in
    let%bind e = bind_fold_ne_list aux env binders in
    let post_env = Environment.add_module alias e env in
    return (Module_alias { alias; binders}) (t_unit ()) post_env state @@ Wrap.mod_al ()
  )

(*
  Recursively search the type_expression and return a result containing the
  type_value at the leaves
*)
and evaluate_type : environment -> I.type_expression -> (O.type_expression, typer_error) result = fun e t ->
  let return tv' = ok (make_t ~loc:t.location ?sugar:t.sugar tv') in
  match t.type_content with
  | T_sum {fields ; layout} ->
    let aux v =
      let {associated_type ; michelson_annotation ; decl_pos} : _ I.row_element_mini_c = v in
      let%bind associated_type = evaluate_type e associated_type in
      ok @@ ({associated_type ; michelson_annotation ; decl_pos}:_ O.row_element_mini_c)
    in
    let%bind fields = Stage_common.Helpers.bind_map_lmap aux fields in
    let%bind () = trace_assert_fail_option (variant_redefined_error t.location) @@
      Environment.get_sum fields e in
    return (T_sum {fields ; layout})
  | T_record {fields ; layout} ->
    let aux v =
      let {associated_type ; michelson_annotation ; decl_pos} : _ I.row_element_mini_c = v in
      let%bind associated_type = evaluate_type e associated_type in
      ok @@ ({associated_type ; michelson_annotation ; decl_pos}:_ O.row_element_mini_c)
    in
    let%bind fields = Stage_common.Helpers.bind_map_lmap aux fields in
    (* let%bind () = trace_assert_fail_option (record_redefined_error t.location) @@
      Environment.get_record content e in *)
    return (T_record {fields ; layout})
  | T_variable variable ->
    (* Check that the variable is in the environment *)
    let name : O.type_variable = Var.todo_cast variable in
    trace_option (unbound_type_variable e name t.location)
      @@ Environment.get_type_opt (name) e
  | T_arrow {type1;type2} ->
    let%bind type1 = evaluate_type e type1 in
    let%bind type2 = evaluate_type e type2 in
    return (T_arrow {type1;type2})
  | T_app {type_operator;arguments} ->
    let%bind arguments = bind_map_list (evaluate_type e) arguments in
    return @@ T_app {type_operator;arguments}
  | T_module_accessor {module_name; element} ->
    let%bind module_ = match Environment.get_module_opt module_name e with
      Some m -> ok m
    | None   -> fail @@ unbound_module_variable e module_name t.location
    in
    evaluate_type module_ element
  | T_singleton x -> return (T_singleton x)


and type_expression : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> (environment * _ O'.typer_state * O.expression * O.type_expression, typer_error) result = fun ?tv_opt e state ae ->
  let return : _ -> _ -> _ -> _ O'.typer_state -> _ (* return of type_expression *) = fun expr ty_expr e state constraints ->
    let%bind new_state = Solver.main state constraints in
    let () = Pretty_print_variables.flush_pending_print state in
    ok @@ (e,new_state, expr, ty_expr) in
  let%bind ((e,state,expr,ty_expr),constraints) = type_expression' ?tv_opt e state ae in
  return expr ty_expr e state constraints

and type_expression' : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> ((environment * _ O'.typer_state * O.expression * O.type_expression) * _, typer_error) result = fun ?tv_opt e state ae ->
  let self = type_expression' in
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  let module L = Logger.Stateful() in
  let return : _ -> _ -> _ O'.typer_state -> _ -> _ -> _ (* return of type_expression *) = fun expr e state new_constraints constraints type_name ->
    let tv = t_variable type_name in
    let loc = ae.location in
    let expr' = e_ascription ~loc expr tv in
    Format.eprintf "Returning expr : %a \nwith new_constraints: %a\n"
      Ast_core.PP.expression expr'
      Ast_core.PP.(list_sep_d type_constraint_short) new_constraints;
    ok @@ ((e,state, expr',tv),new_constraints@constraints) in
  let return_wrapped expr e state constraints (c , expr_type) = return expr e state c constraints expr_type in
  Format.eprintf "Type_expression : %a\n%!" Ast_core.PP.expression ae ;
  Format.eprintf "Env : %a\n%!" Ast_core.PP.environment e;
  trace (expression_tracer ae) @@
  match ae.expression_content with

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
    Format.eprintf "wrap variable : %a, %a\n%!"
      O.PP.expression_variable name
      O.PP.environment_element tv'
      ;
    let wrapped = Wrap.variable name tv'.type_value in
    let expr' = e_variable name in
    return_wrapped expr' e state [] wrapped
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
      return_wrapped (e_bytes_raw b) e state [] @@ Wrap.literal "bytes" (t_bytes ())
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
        let%bind (e,state,l, t), constraints = self e state l in
        ok ((e,state, c @ constraints),(l,t))
      ) (e,state,[]) lst
    in
    let lst,lst_annot = List.split lst in
    let wrapped = Wrap.constant cons_name t lst_annot in
    return_wrapped (e_constant cons_name lst) e state constraints wrapped

  | E_lambda lambda ->
    let%bind lambda,_,state,constraints,wrapped = type_lambda e state lambda in
    return_wrapped (make_e @@ E_lambda lambda) e state constraints wrapped

  | E_application {lamb;args} ->
    let%bind (e,state,lamb', t_l),c1 = self e state lamb in
    let%bind (e,state,args, t_a),c2 = self e state args in
    let wrapped = Wrap.application lamb t_l t_a in
    return_wrapped (e_application lamb' args) e state (c1@c2) wrapped

  (* Sum *)
  | E_constructor {constructor;element} ->
    (* Check that the constructor is from an existing variant *)
    let%bind (c_tv, sum_tv) = trace_option (unbound_constructor e constructor ae.location) @@
      Environment.get_constructor constructor e in
    let%bind (e,state, element,t_e),constraints = self e state element in
    (* Check that the element in the variant as the proper type *)
    (* TODO: infer the variant or the type of the element ?*)
    let wrapped = Wrap.constructor constructor t_e c_tv sum_tv in
    return_wrapped (e_constructor constructor element) e state constraints wrapped

  | E_matching {matchee;cases} -> (
      let%bind (e,state,ex',t_e),constraints = self e state matchee in
      let%bind (e,state,m',tvs),c_match  = type_match e state t_e cases ae ae.location in
      let wrapped = Wrap.matching tvs in
      return_wrapped (e_matching ex' m') e state (constraints@c_match) wrapped
    )

  (* Record *)
  | E_record m ->
    let aux (e,state,c) _ expr =
      let%bind (e,state,expr,t),constraints = self e state expr in
      ok ((e,state,constraints@c), (expr,t))
    in
    let%bind (e,state',constraints), m' = Stage_common.Helpers.bind_fold_map_lmap aux (e,state,[]) m in
    (* Do we need row_element for Ast_core ? *)
    let lmap = O.LMap.map (fun (_e,t) -> ({associated_type = t ; michelson_annotation = None ; decl_pos = 0}: O.row_element)) m' in
    let record_type = match Environment.get_record lmap e with
      | None -> O.{fields=lmap;layout= Some default_layout}
      | Some r -> r
    in
    let wrapped = Wrap.record record_type in
    return_wrapped (e_record @@ O.LMap.map fst m') e state' constraints wrapped

  | E_record_accessor {record;path} -> (
      let%bind (e,state,base,t),constraints = self e state record in
      let wrapped = Wrap.access_label ~base:t ~label:path in
      return_wrapped (e_record_accessor base path) e state constraints wrapped
    )

  | E_record_update {record; path; update} ->
    let%bind (e,state,record,t_r),c1 = self e state record in
    let%bind (e,state,update,t_u),c2 = self e state update in
    (* TODO: wrap.record_update *)
    let wrapped = Wrap.record_update  ~base:t_r ~label:path @@ t_u in
    return_wrapped (e_record_update record path update) e state (c1@c2) wrapped

  (* Advanced *)
  | E_let_in {let_binder ; rhs ; let_result; inline} ->
    let%bind rhs_tv_opt = bind_map_option (evaluate_type e) (let_binder.ascr) in
    let%bind (e,state,rhs,t_r),c1 = self e state rhs in
    let%bind let_binder = Stage_common.Maps.binder (evaluate_type e) let_binder in
    let fresh = Typesystem.Core.fresh_for_expr_var let_binder.var in
    let e = Environment.add_ez_binder (let_binder.var) (t_variable fresh) e in
    let%bind (_,state,let_result,l_let),c2 = self e state let_result in
    let wrapped = Wrap.let_in fresh t_r rhs_tv_opt l_let in
    return_wrapped (e_let_in let_binder rhs let_result inline) e state (c1@c2) wrapped

  | E_type_in {type_binder; rhs ; let_result} ->
    let%bind rhs = evaluate_type e rhs in
    let e = Environment.add_type type_binder rhs e in
    let%bind (e,state,let_result,t),constraints = self e state let_result in
    let wrapped = Wrap.type_in t in
    return_wrapped (e_type_in type_binder rhs let_result) e state constraints wrapped

  | E_mod_in {module_binder; rhs ; let_result} ->
    let%bind (env,rhs,_,_) = type_module ~init_env:e rhs in
    let e = Environment.add_module module_binder env e in
    let%bind (e,state,let_result,t),constraints = self e state let_result in
    let wrapped =
      Wrap.mod_in t in
    return_wrapped (e_mod_in module_binder rhs let_result) e state constraints wrapped

  | E_mod_alias {alias; binders; result} ->
    let aux e binder =
      trace_option (unbound_module_variable e binder ae.location) @@
      Environment.get_module_opt binder e in
    let%bind env = bind_fold_ne_list aux e binders in
    let e = Environment.add_module alias env e in
    let%bind (e,state,result,t),constraints = self e state result in
    let wrapped =
      Wrap.mod_alias t in
    return_wrapped (e_mod_alias alias binders result) e state constraints wrapped

  | E_recursive {fun_name;fun_type;lambda} ->
    (* Add the function name to the environment before evaluating the lambda*)
    let fun_name = cast_var fun_name in
    let%bind fun_type = evaluate_type e fun_type in
    let e = Environment.add_ez_binder fun_name fun_type e in
    let%bind lambda,e,state,c1,(c2,t_var) = type_lambda e state lambda in
    let wrapped = Wrap.recursive t_var fun_type in
    return_wrapped (e_recursive fun_name fun_type lambda) e state (c1@c2) wrapped

  | E_raw_code {language ; code} ->
    (* The code is a string with an annotation*)
    let%bind (code,type_expression) = trace_option (expected_ascription code) @@
      I.get_e_ascription code.expression_content in
    let%bind (e,state,code,t),constraints = self e state code in
    let%bind type_expression = evaluate_type e type_expression in
    let wrapped = Wrap.raw_code type_expression t in
    let code = e_ascription code type_expression in
    return_wrapped (e_raw_code language code) e state constraints wrapped
  | E_ascription {anno_expr;type_annotation} ->
    let%bind tv = evaluate_type e type_annotation in
    let%bind (e,state,expr',t),constraints = self e state anno_expr in
    let wrapped = Wrap.annotation t tv
    (* TODO: we're probably discarding too much by using expr'.expression.
       Previously: {expr' with type_annotation = the_explicit_type_annotation}
       but then this case is not like the others and doesn't call return_wrapped,
       which might do some necessary work *)
    in return_wrapped expr' e state constraints wrapped
  | E_module_accessor {module_name; element} ->
    let%bind module_env = match Environment.get_module_opt module_name e with
      Some m -> ok m
    | None   -> fail @@ unbound_module_variable e module_name ae.location
    in
    let%bind (e,state,element,t),constraints = self ?tv_opt module_env state element in
    let wrapped = Wrap.module_access t in
    return_wrapped (e_module_accessor module_name element) e state constraints wrapped

and type_lambda e state {
      binder ;
      output_type ;
      result ;
    } =
      let%bind input_type'  = bind_map_option (evaluate_type e) binder.ascr in
      let%bind output_type' = bind_map_option (evaluate_type e) output_type in
      let%bind binder = Stage_common.Maps.binder (evaluate_type e) binder in

      let fresh : O.type_expression = t_variable (Typesystem.Core.fresh_for_expr_var binder.var) in
      let e' = Environment.add_ez_binder (binder.var) fresh e in

      let%bind (e, state', result,t),constraints = type_expression' e' state result in
      let wrapped = Wrap.lambda fresh input_type' output_type' t in
      ok (({binder;output_type = Some t ;result}:_ O.lambda),e,state',constraints,wrapped)

(* TODO: Rewrite this entire function *)
and type_match : environment -> _ O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> ((environment * _ O'.typer_state * O.matching_expr * O.type_expression list) * _, typer_error) result =
  fun e state t i _ae loc ->
  let self = type_expression' in
  let return e state t c m = ok @@ ((e,state, m,t),c) in
  match i with
    | Match_option {match_none ; match_some} ->
      let%bind (e,state, match_none, t_none),c1 = self e state match_none in
      let {opt; body}:I.match_some = match_some in
      let opt = cast_var opt in
      (* Add the binder just for typing the case *)
      let tv = t_variable @@ Var.fresh ~name:"match_some" () in
      let e = Environment.add_ez_binder opt tv e in
      let c = Wrap.match_opt tv t in
      let%bind (e,state, body, t_body),c2 = type_expression' e state body in
      return e state [t_none;t_body] (c@c1@c2) @@ O.Match_option {match_none ; match_some = { opt; body}}
    | Match_list {match_nil ; match_cons} ->
      let%bind (e,state,match_nil, t_nil),c1 = self e state match_nil in
      let {hd; tl; body} : I.match_cons = match_cons in
      let hd = cast_var hd in
      let tl = cast_var tl in
      (* Add the binder just for typing the case *)
      let t_elt = t_variable @@ Var.fresh ~name:"elt" () in
      let e = Environment.add_ez_binder hd t_elt e in
      let e = Environment.add_ez_binder tl t e in
      let%bind (e,state,body, t_body),c2 = self e state body in
      let c = Wrap.match_lst t_elt t in
      return e state [t_nil;t_body] (c@c1@c2) @@ O.Match_list {match_nil ; match_cons = {hd; tl; body}}
    | Match_record {fields ; body } ->
      (*
      let aux e _ ({var;ascr} : I.type_expression I.binder) =
        let%bind ty_opt = bind_map_option (evaluate_type e)  ascr in
        let ty = Option.unopt ~default:(O.t_variable @@ Var.fresh ()) ty_opt in
        let e  = Environment.add_ez_binder var ty e in
        ok @@ (e,(var,ty)) 
      in
      *)
      let aux e _ binder =
        let%bind binder = Stage_common.Maps.binder (evaluate_type e) binder in
        let ty = Option.unopt ~default:(O.t_variable @@ Var.fresh ()) binder.ascr in
        let e = Environment.add_ez_binder binder.var ty e in
        ok @@ (e,(binder,ty))
      in 
      let%bind (e', fields) = Stage_common.Helpers.bind_fold_map_lmap aux e fields in
      let%bind (e,state,body,t_body),c = self e' state body in
      let c2 = Wrap.match_record (O.LMap.map snd fields) t in
      return e state [t_body] (c@c2) @@ O.Match_record {fields= O.LMap.map fst fields ; body }
    | Match_variant lst ->
      let%bind (e, state, c,ty), cases =
        let aux (e,state,c,ty) ({constructor; proj; body}: I.match_variant) =
          let%bind (constructor_type , _variant) =
            trace_option (unbound_constructor e constructor loc) @@
            Environment.get_constructor constructor e in
          let e = Environment.add_ez_binder proj constructor_type e in
          let%bind (e,state,body, t_body),c1 = self e state body in
          let c2 = Wrap.match_variant constructor ~case:constructor_type t in
          ok ((e, state,c1@c2@c, t_body :: ty) , ({constructor ; proj ; body} : O.match_variant))
        in
        bind_fold_map_list aux (e,state,[],[]) lst in
      (* TODO: check that the variant is complete, has to do with the unused variant *)
      return e state ty c @@ O.Match_variant cases

(* Apply type_declaration on every node of the AST_core from the root p *)
and type_module_returns_env ((env, state, p) : environment * _ O'.typer_state * I.module_) : (environment * _ O'.typer_state * O.module_with_unification_vars * O.type_expression, Typer_common.Errors.typer_error) result =
  let aux ((e : environment), (s : _ O'.typer_state) , (ds : O.declaration Location.wrap list), tys) (d:I.declaration Location.wrap) =
    let%bind (e , s' , d',t) = type_declaration_subst e s d in
    (* TODO: Move this filter to the spiller *)
    let ds',tys' = match Location.unwrap d' with
      | O.Declaration_type _ -> ds,tys
      | O.Declaration_constant {binder = {var= {wrap_content = n}};_}
        ->  let n = Var.to_name n in d' :: ds, (n,t)::tys
      | O.Declaration_module {module_binder = n;_}
      | O.Module_alias {alias = n;_} -> d' :: ds, (n,t)::tys
    in
    ok (e , s' , ds', tys')
  in
  let%bind (env' , state , declarations, types) =
    trace (module_error_tracer p) @@
    bind_fold_list aux (env , state , [],[]) p in
  let declarations = List.rev declarations in (* Common hack to have O(1) append: prepend and then reverse *)
  let ty = make_t_ez_record types in
  ok (env', state, O.Module_With_Unification_Vars declarations,ty)

and print_env_state_node : type a. (Format.formatter -> a -> unit) -> (environment * _ O'.typer_state * a) -> unit =
  fun node_printer (env,state,node) ->
  ignore node; (* TODO *)
  Printf.eprintf "%s" @@
    Format.asprintf "{ \"ENV\": %s,\n\"STATE\": %s,\n\"NODE\": %a\n},\n"
      (Yojson.Safe.to_string (Ast_core.Yojson.environment env))
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
      ((environment * _ O'.typer_state * a) -> (environment * _ O'.typer_state * b * 'c, typer_error) Trace.result) ->
      (b * 'c * _ O'.typer_state * environment , typer_error) result =
  fun in_printer out_printer env_state_node apply_substs types_and_returns_env ->
  let () = (if Ast_core.Debug.json_new_typer then Printf.eprintf "%!\n###############################START_OF_JSON\n[%!") in
  let () = (if Ast_core.Debug.debug_new_typer then Printf.fprintf stderr "%!\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env_state_node here.\n\n%!") in
  let () = (if Ast_core.Debug.debug_new_typer && Ast_core.Debug.json_new_typer then print_env_state_node in_printer env_state_node) in
  let%bind (env, state, node, ty) = types_and_returns_env env_state_node in
  let%bind node,ty,env =
    Format.eprintf "Substitutions ongoing\n%!";
    let aliases = state.aliases in
    let assignments = state.plugin_states#assignments in
    let substs : variable: O.type_variable -> O.type_content option = fun ~variable ->
      to_option @@
      let () = (if Ast_core.Debug.debug_new_typer then Printf.fprintf stderr "%s" @@ Format.asprintf "Looking up var  %a\n" Var.pp variable) in
      let%bind root = Solver.get_alias variable aliases in
      let () = (if Ast_core.Debug.debug_new_typer then Printf.fprintf stderr "%s" @@ Format.asprintf "Looking up var  %a (its root is %a)\n" Var.pp variable Var.pp root) in
      let%bind assignment =
        trace_option (corner_case (Format.asprintf "can't find assignment for root %a%!" Var.pp root)) @@
          (Database_plugins.All_plugins.Assignments.find_opt root assignments) in
      match assignment with
      | `Constructor { tv ; c_tag ; tv_list } ->
        let%bind tv_root = Solver.get_alias tv aliases in
        (* let () = Format.eprintf "\ncstr : %a(was %a) %a(was %a)\n" Ast_core.PP.type_variable tv_root Ast_core.PP.type_variable tv Ast_core.PP.type_variable root Ast_core.PP.type_variable variable in *)
        let () = assert (Var.equal tv_root root) in
        let%bind (expr : O.type_content) = trace_option (corner_case "wrong constant tag") @@
        Typesystem.Core.type_expression'_of_simple_c_constant (c_tag , (List.map O.t_variable tv_list)) in
        let () = (if Ast_core.Debug.debug_new_typer then Printf.fprintf stderr "%s%!" @@ Format.asprintf "Substituing var %a (%a is %a)\n%!" Var.pp variable Var.pp root Ast_core.PP.type_content expr) in
        ok @@ expr
      | `Row { tv ; r_tag ; tv_map ; reason_row_simpl=_ } ->
        let%bind tv_root = Solver.get_alias tv aliases in
        (* let () = Format.eprintf "\ncstr : %a(was %a) %a(was %a)\n" Ast_core.PP.type_variable tv_root Ast_core.PP.type_variable tv Ast_core.PP.type_variable root Ast_core.PP.type_variable variable in *)
        let () = assert (Var.equal tv_root root) in
        let (expr : O.type_content) = Typesystem.Core.type_expression'_of_simple_c_row (r_tag , tv_map) in
        let () = (if Ast_core.Debug.debug_new_typer then Printf.fprintf stderr "%s%!" @@ Format.asprintf "Substituing var %a (%a is %a)\n%!" Var.pp variable Var.pp root Ast_core.PP.type_content expr) in
        ok @@ expr
    in
    Format.eprintf "substituting node\n%!";
    let%bind node = apply_substs ~substs node in
    Format.eprintf "substituting env %a\n%!" Ast_core.PP.environment env;
    let%bind ty = Typesystem.Misc.Substitution.Pattern.s_type_expression ~substs ty in
    Format.eprintf "substituting ty %a\n%!" Ast_core.PP.type_expression ty;
    let%bind env  = Typesystem.Misc.Substitution.Pattern.s_environment ~substs env in
    Format.eprintf "New env %a\n%!" O.PP.environment env;
    ok (node,ty,env)
  in
  Format.eprintf "Substritutions done\n%!";
  let () = (if Ast_core.Debug.debug_new_typer then Printf.fprintf stderr "\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env,state,node here again.\n\n") in
  let () = (if Ast_core.Debug.debug_new_typer && Ast_core.Debug.json_new_typer then print_env_state_node out_printer (env, state, node)) in
  ok (node, ty, state, env)

and type_declaration_subst env _state decl = 
  let empty_state = Solver.initial_state in
  let%bind (d,t, state, e) = type_and_subst
      (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.declaration\"")
      (fun ppf p -> Format.fprintf ppf "%s" (Yojson.Safe.to_string (Ast_core.Yojson.declaration @@ Location.unwrap p)))
      (env , empty_state , decl)
      Typesystem.Misc.Substitution.Pattern.s_declaration_wrap
      (fun (a,b,c) -> type_declaration a b c) in
  ok @@ (e, state, d, t)

and type_module ~init_env (p : I.module_) : (environment * O.module_ * O.type_expression * _ O'.typer_state, typer_error) result =
  let empty_state = Solver.initial_state in
  let%bind (p, t, state, env) = type_and_subst
    (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.module\"")
    (fun ppf p -> Format.fprintf ppf "%s" (Yojson.Safe.to_string (Ast_core.Yojson.module_with_unification_vars p)))
    (init_env , empty_state , p)
    Typesystem.Misc.Substitution.Pattern.s_module
    type_module_returns_env in
  Format.eprintf "Charcking for uni_vars\n%!";
  let%bind p = Check.check_has_no_unification_vars p in
  let () = (if Ast_core.Debug.json_new_typer then Printf.eprintf "%!\"end of JSON\"],\n###############################END_OF_JSON\n%!") in
  let () = Pretty_print_variables.flush_pending_print state in
  Format.eprintf "module typed\n\n%!";
  ok (env, p,t, state)

and type_expression_subst (env : environment) (state : _ O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (O.environment * O.expression * O.type_expression * _ O'.typer_state , typer_error) result =
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  let%bind (expr, t, state, env) = type_and_subst
      (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.expression\"")
      (fun ppf p -> Format.fprintf ppf "%s" (Yojson.Safe.to_string (Ast_core.Yojson.expression p)))
      (env , state , e)
      Typesystem.Misc.Substitution.Pattern.s_expression
      (fun (a,b,c) -> type_expression a b c) in
  let%bind () = Check.check_expression_has_no_unification_vars expr in
  let () = (if Ast_core.Debug.json_new_typer then Printf.eprintf "%!\"end of JSON\"],\n###############################END_OF_JSON\n%!") in
  let () = Pretty_print_variables.flush_pending_print state in
  ok (env, expr, t, state)

let untype_expression       = Untyper.untype_expression

(* These aliases are just here for quick navigation during debug, and can safely be removed later *)
let [@warning "-32"] (*rec*) type_declaration _env _state : I.declaration Location.wrap -> (environment * _ O'.typer_state * O.declaration Location.wrap * O.type_expression, typer_error) result = type_declaration _env _state
and [@warning "-32"] type_match : environment -> _ O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> ((environment * _ O'.typer_state * O.matching_expr * _) * _, typer_error) result = type_match
and [@warning "-32"] evaluate_type (e:environment) (t:I.type_expression) : (O.type_expression, typer_error) result = evaluate_type e t
and [@warning "-32"] type_expression : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> (environment * _ O'.typer_state * O.expression * O.type_expression, typer_error) result = type_expression
and [@warning "-32"] type_lambda e state lam = type_lambda e state lam
let [@warning "-32"] type_module_returns_env ((env, state, p) : environment * _ O'.typer_state * I.module_) : (environment * _ O'.typer_state * O.module_with_unification_vars * O.type_expression, typer_error) result = type_module_returns_env (env, state, p)
let [@warning "-32"] type_and_subst (in_printer : (Format.formatter -> 'a -> unit)) (out_printer : (Format.formatter -> 'b -> unit)) (env_state_node : environment * _ O'.typer_state * 'a) (apply_substs : ('b,typer_error) Typesystem.Misc.Substitution.Pattern.w) (types_and_returns_env : (environment * _ O'.typer_state * 'a) -> (environment * _ O'.typer_state * 'b * 'c, typer_error) result) : ('b * 'c * _ O'.typer_state, typer_error) result =
  let%bind (n,t,s,_) = type_and_subst in_printer out_printer env_state_node apply_substs types_and_returns_env in
  ok (n,t,s)
let [@warning "-32"] type_module ~init_env (p : I.module_) : (environment * O.module_ * O.type_expression * _ O'.typer_state, typer_error) result = type_module ~init_env p
let [@warning "-32"] type_expression_subst (env : environment) (state : _ O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (environment * O.expression * O.type_expression * _ O'.typer_state, typer_error) result = type_expression_subst env state ?tv_opt e
