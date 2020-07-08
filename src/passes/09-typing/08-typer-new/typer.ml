open Trace
module I = Ast_core
module O = Ast_typed
module O' = Typesystem.Solver_types
open O.Combinators
module DEnv = Environment
module Environment = O.Environment
module Solver = Solver
type environment = Environment.t
module Errors = Typer_common.Errors
open Errors
module Map = RedBlackTrees.PolyMap

open Todo_use_fold_generator

let assert_type_expression_eq ((tv',tv):O.type_expression * O.type_expression) : (unit,typer_error) result = 
  Compare_types.assert_type_expression_eq (tv' , tv)

(*
  Extract pairs of (name,type) in the declaration and add it to the environment
*)
let rec type_declaration env state : I.declaration -> (environment * O'.typer_state * O.declaration option, typer_error) result = function
  | Declaration_type (type_name , type_expression) ->
    let%bind tv = evaluate_type env type_expression in
    let env' = Environment.add_type (type_name) tv env in
    ok (env', state , None)
  | Declaration_constant (binder , tv_opt , attr, expression) -> (
    (*
      Determine the type of the expression and add it to the environment
    *)
      let%bind tv'_opt = bind_map_option (evaluate_type env) tv_opt in
      let%bind (expr , state') =
        trace (constant_declaration_tracer binder expression tv'_opt) @@
        type_expression env state expression in
      let post_env = Environment.add_ez_declaration binder expr env in
      ok (post_env, state' , Some (O.Declaration_constant { binder ; expr ; inline=attr.inline} ))
    )

and type_match : environment -> O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> (O.matching_expr * O'.typer_state, typer_error) result =
  fun e state t i _ae loc -> match i with
    | Match_option {match_none ; match_some} ->
      let%bind tv =
        trace_option (match_error ~expected:i ~actual:t loc)
        @@ get_t_option t in
      let%bind (match_none , state') = type_expression e state match_none in
      let (opt, b) = match_some in
      let e' = Environment.add_ez_binder opt tv e in
      let%bind (body , state'') = type_expression e' state' b in
      ok (O.Match_option {match_none ; match_some = { opt; body; tv}} , state'')
    | Match_list {match_nil ; match_cons} ->
      let%bind t_elt =
        trace_option (match_error ~expected:i ~actual:t loc)
        @@ get_t_list t in
      let%bind (match_nil , state') = type_expression e state match_nil in
      let (hd, tl, b) = match_cons in
      let e' = Environment.add_ez_binder hd t_elt e in
      let e' = Environment.add_ez_binder tl t e' in
      let%bind (body , state'') = type_expression e' state' b in
      ok (O.Match_list {match_nil ; match_cons = {hd; tl; body;tv=t}} , state'')
    | Match_variant lst ->
      let%bind variant_opt =
        let aux acc ((constructor_name , _) , _) =
          let%bind (_ , variant) =
            trace_option (unbound_constructor e constructor_name loc) @@
            Environment.get_constructor constructor_name e in
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
      let%bind variant =
        trace_option (match_empty_variant i loc) @@
        variant_opt in
      let%bind () =
        let%bind variant_cases' =
          trace_option (match_error ~expected:i ~actual:t loc)
          @@ Ast_typed.Combinators.get_t_sum variant in
        let variant_cases = List.map fst @@ O.CMap.to_kv_list variant_cases' in
        let match_cases = List.map (fun x -> convert_constructor' @@ fst @@ fst x) lst in
        let test_case = fun c ->
          Assert.assert_true (corner_case "match case") (List.mem c match_cases)
        in
        let%bind () =
          trace_strong (match_missing_case i loc) @@
          bind_iter_list test_case variant_cases in
        let%bind () = Assert.assert_true (match_redundant_case i loc) @@
          List.(length variant_cases = length match_cases) in
        ok ()
      in
      let%bind (state'' , cases) =
        let aux state ((constructor_name , pattern) , b) =
          let%bind (constructor , _) =
            trace_option (unbound_constructor e constructor_name loc) @@
            Environment.get_constructor constructor_name e in
          let e' = Environment.add_ez_binder pattern constructor e in
          let%bind (body , state') = type_expression e' state b in
          let constructor = convert_constructor' constructor_name in
          ok (state' , ({constructor ; pattern ; body = body} : O.matching_content_case))
        in
        bind_fold_map_list aux state lst in
      ok (O.Match_variant {cases ; tv=variant } , state'')

(*
  Recursively search the type_expression and return a result containing the
  type_value at the leaves
*)
and evaluate_type (e:environment) (t:I.type_expression) : (O.type_expression, typer_error) result =
  let return tv' = ok (make_t ~loc:t.location tv' (Some t)) in
  match t.content with
  | T_arrow {type1;type2} ->
    let%bind type1 = evaluate_type e type1 in
    let%bind type2 = evaluate_type e type2 in
    return (T_arrow {type1;type2})
  | T_sum m ->
    let aux k v prev =
      let%bind prev' = prev in
      let {ctor_type ; michelson_annotation ; ctor_decl_pos} : I.ctor_content = v in
      let%bind ctor_type = evaluate_type e ctor_type in
      ok @@ O.CMap.add (convert_constructor' k) ({ctor_type ; michelson_annotation ; ctor_decl_pos}:O.ctor_content) prev'
    in
    let%bind m = I.CMap.fold aux m (ok O.CMap.empty) in
    return (T_sum m)
  | T_record m ->
    let aux k v prev =
      let%bind prev' = prev in
      let {field_type ; field_annotation ; field_decl_pos} : I.field_content = v in
      let%bind field_type = evaluate_type e field_type in
      ok @@ O.LMap.add (convert_label k) ({field_type ; michelson_annotation=field_annotation ; field_decl_pos}:O.field_content) prev'
    in
    let%bind m = I.LMap.fold aux m (ok O.LMap.empty) in
    return (T_record m)
  | T_variable name ->
    let%bind tv =
      trace_option (unbound_type_variable e name t.location)
      @@ Environment.get_type_opt (name) e in
    ok tv
  | T_constant cst ->
      return (T_constant (convert_type_constant cst))
  | T_operator (op, lst) ->
      ( match op,lst with
        | TC_set, [s] ->
            let%bind s = evaluate_type e s in
            return @@ T_operator (O.TC_set s)
        | TC_option, [o] ->
            let%bind o = evaluate_type e o in
            return @@ T_operator  (O.TC_option o)
        | TC_list, [l] ->
            let%bind l = evaluate_type e l in
            return @@ T_operator  (O.TC_list l)
        | TC_map, [k;v] ->
            let%bind k = evaluate_type e k in
            let%bind v = evaluate_type e v in
            return @@ T_operator (O.TC_map {k;v})
        | TC_big_map, [k;v] ->
            let%bind k = evaluate_type e k in
            let%bind v = evaluate_type e v in
            return @@ T_operator (O.TC_big_map {k;v})
        | TC_map_or_big_map, [k;v] ->
            let%bind k = evaluate_type e k in
            let%bind v = evaluate_type e v in
            return @@ T_operator (O.TC_map_or_big_map {k;v})
        | TC_contract, [c] ->
            let%bind c = evaluate_type e c in
            return @@ T_operator (O.TC_contract c)
        | TC_michelson_pair_right_comb, [c] ->
            let%bind c' = evaluate_type e c in
            let%bind lmap = match c'.type_content with
              | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap)) -> ok lmap
              | _ -> fail (michelson_comb_no_record t.location) in
            let record = Typer_common.Michelson_type_converter.convert_pair_to_right_comb (Ast_typed.LMap.to_kv_list lmap) in
            return @@ record
        | TC_michelson_pair_left_comb, [c] ->
            let%bind c' = evaluate_type e c in
            let%bind lmap = match c'.type_content with
              | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap)) -> ok lmap
              | _ -> fail (michelson_comb_no_record t.location) in
            let record = Typer_common.Michelson_type_converter.convert_pair_to_left_comb (Ast_typed.LMap.to_kv_list lmap) in
            return @@ record
        | TC_michelson_or_right_comb, [c] ->
            let%bind c' = evaluate_type e c in
            let%bind cmap = match c'.type_content with
              | T_sum cmap -> ok cmap
              | _ -> fail (michelson_comb_no_variant t.location) in
            let pair = Typer_common.Michelson_type_converter.convert_variant_to_right_comb (Ast_typed.CMap.to_kv_list cmap) in
            return @@ pair
        | TC_michelson_or_left_comb, [c] ->
            let%bind c' = evaluate_type e c in
            let%bind cmap = match c'.type_content with
              | T_sum cmap -> ok cmap
              | _ -> fail (michelson_comb_no_variant t.location) in
            let pair = Typer_common.Michelson_type_converter.convert_variant_to_left_comb (Ast_typed.CMap.to_kv_list cmap) in
            return @@ pair
        | _ -> fail @@ unrecognized_type_op t
      )

and type_expression : environment -> O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * O'.typer_state, typer_error) result = fun e state ?tv_opt ae ->
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  let open Solver in
  let module L = Logger.Stateful() in
  let return : _ -> O'.typer_state -> _ -> _ (* return of type_expression *) = fun expr state constraints type_name ->
    let%bind new_state = aggregate_constraints state constraints in
    let tv = t_variable type_name () in
    let location = ae.location in
    let expr' = make_e ~location expr tv in
    ok @@ (expr' , new_state) in
  let return_wrapped expr state (constraints , expr_type) = return expr state constraints expr_type in
  trace (expression_tracer ae) @@
  match ae.content with

  (* TODO: this file should take care only of the order in which program fragments
     are translated by Wrap.xyz

     TODO: produce an ordered list of sub-fragments, and use a common piece of code
     to actually perform the recursive calls *)

  (* Basic *)
  | E_variable name -> (
      let name'= name in
      let%bind (tv' : Environment.element) =
        trace_option (unbound_variable e name ae.location)
        @@ Environment.get_opt name' e in
      let (constraints , expr_type) = Wrap.variable name tv'.type_value in
      let expr' = e_variable name' in
      return expr' state constraints expr_type
    )

  | E_literal (Literal_string s) -> (
      return_wrapped (e_string s) state @@ Wrap.literal (t_string ())
    )
  | E_literal (Literal_signature s) -> (
      return_wrapped (e_signature s) state @@ Wrap.literal (t_signature ())
    )
  | E_literal (Literal_key s) -> (
      return_wrapped (e_key s) state @@ Wrap.literal (t_key ())
    )
  | E_literal (Literal_key_hash s) -> (
      return_wrapped (e_key_hash s) state @@ Wrap.literal (t_key_hash ())
    )
  | E_literal (Literal_chain_id s) -> (
      return_wrapped (e_chain_id s) state @@ Wrap.literal (t_chain_id ())
    )
  | E_literal (Literal_bytes b) -> (
      return_wrapped (e_bytes b) state @@ Wrap.literal (t_bytes ())
    )
  | E_literal (Literal_int i) -> (
      return_wrapped (e_int i) state @@ Wrap.literal (t_int ())
    )
  | E_literal (Literal_nat n) -> (
      return_wrapped (e_nat n) state @@ Wrap.literal (t_nat ())
    )
  | E_literal (Literal_mutez t) -> (
      return_wrapped (e_mutez t) state @@ Wrap.literal (t_mutez ())
    )
  | E_literal (Literal_address a) -> (
      return_wrapped (e_address a) state @@ Wrap.literal (t_address ())
    )
  | E_literal (Literal_timestamp t) -> (
      return_wrapped (e_timestamp t) state @@ Wrap.literal (t_timestamp ())
    )
  | E_literal (Literal_operation o) -> (
      return_wrapped (e_operation o) state @@ Wrap.literal (t_operation ())
    )
  | E_literal (Literal_unit) -> (
      return_wrapped (e_unit ()) state @@ Wrap.literal (t_unit ())
    )

  | E_record_accessor {record;path} -> (
      let%bind (base' , state') = type_expression e state record in
      let path = convert_label path in
      let wrapped = Wrap.access_label ~base:base'.type_expression ~label:path in
      return_wrapped (E_record_accessor {record=base';path}) state' wrapped
    )

  (* Sum *)
  | E_constructor {constructor;element} ->
    let%bind (c_tv, sum_tv) = trace_option (unbound_constructor e constructor ae.location) @@
      Environment.get_constructor constructor e in
    let%bind (expr' , state') = type_expression e state element in
    let%bind _assert = assert_type_expression_eq (expr'.type_expression, c_tv) in
    let wrapped = Wrap.constructor expr'.type_expression c_tv sum_tv in
    let constructor = convert_constructor' constructor in
    return_wrapped (E_constructor {constructor; element=expr'}) state' wrapped

  (* Record *)
  | E_record m ->
    let aux (acc, state) k expr =
      let%bind (expr' , state') = type_expression e state expr in
      ok (O.LMap.add (convert_label k) expr' acc , state')
    in
    let%bind (m' , state') = Stage_common.Helpers.bind_fold_lmap aux (ok (O.LMap.empty , state)) m in
    let wrapped = Wrap.record (O.LMap.map (fun e -> ({field_type = get_type_expression e ; michelson_annotation = None ; field_decl_pos = 0}: O.field_content)) m') in
    return_wrapped (E_record m') state' wrapped
  | E_record_update {record; path; update} ->
    let%bind (record, state) = type_expression e state record in
    let%bind (update,state) = type_expression e state update in
    let wrapped = get_type_expression record in
    let path = convert_label path in
    let%bind (wrapped,tv) = 
      match wrapped.type_content with 
      | T_record record -> (
          let field_op = O.LMap.find_opt path record in
          match field_op with
          | Some {field_type=tv;_} -> ok (record,tv)
          | None -> failwith @@ Format.asprintf "field %a is not part of record" O.PP.label path
      )
      | _ -> failwith "Update an expression which is not a record"
    in
    let%bind () = assert_type_expression_eq (tv, get_type_expression update) in
    return_wrapped (E_record_update {record; path; update}) state (Wrap.record wrapped)
  (* Data-structure *)
  | E_application {lamb;args} ->
    let%bind (f' , state') = type_expression e state lamb in
    let%bind (args , state'') = type_expression e state' args in
    let wrapped = Wrap.application f'.type_expression args.type_expression in
    return_wrapped (E_application {lamb=f';args}) state'' wrapped

  (* Advanced *)
  | E_let_in {let_binder ; rhs ; let_result; inline} ->
    let%bind rhs_tv_opt = bind_map_option (evaluate_type e) (snd let_binder) in
    (* TODO: the binder annotation should just be an annotation node *)
    let%bind (rhs , state') = type_expression e state rhs in
    let let_binder = fst let_binder in 
    let e' = Environment.add_ez_declaration (let_binder) rhs e in
    let%bind (let_result , state'') = type_expression e' state' let_result in
    let wrapped =
      Wrap.let_in rhs.type_expression rhs_tv_opt let_result.type_expression in
    return_wrapped (E_let_in {let_binder; rhs; let_result; inline}) state'' wrapped
  | E_raw_code {language ; code} ->
    let%bind (code,state') = type_expression e state code in
    let wrapped = Wrap.raw_code code.type_expression in
    return_wrapped (E_raw_code {language; code}) state' wrapped
  | E_ascription {anno_expr;type_annotation} ->
    let%bind tv = evaluate_type e type_annotation in
    let%bind (expr' , state') = type_expression e state anno_expr in
    let wrapped = Wrap.annotation expr'.type_expression tv
    (* TODO: we're probably discarding too much by using expr'.expression.
       Previously: {expr' with type_annotation = the_explicit_type_annotation}
       but then this case is not like the others and doesn't call return_wrapped,
       which might do some necessary work *)
    in return_wrapped expr'.expression_content state' wrapped

  | E_matching {matchee;cases} -> (
      let%bind (ex' , state') = type_expression e state matchee in
      let%bind (m' , state'') = type_match e state' ex'.type_expression cases ae ae.location in
      let tvs =
        let aux (cur : O.matching_expr) =
          match cur with
          | Match_list { match_nil ; match_cons = { hd=_ ; tl=_ ; body ; tv=_} } -> [ match_nil ; body ]
          | Match_option { match_none ; match_some = {opt=_; body; tv=_} } -> [ match_none ; body ]
          | Match_variant { cases ; tv=_ } -> List.map (fun ({constructor=_; pattern=_; body} : O.matching_content_case) -> body) cases in
        List.map get_type_expression @@ aux m' in
      let%bind () = match tvs with
          [] -> fail @@ match_empty_variant cases ae.location
        | _ -> ok () in
      (* constraints:
         all the items of tvs should be equal to the first one
         result = first item of tvs
      *)
      let wrapped = Wrap.matching tvs in
      return_wrapped (O.E_matching {matchee=ex';cases=m'}) state'' wrapped
    )

  | E_lambda lambda -> 
    let%bind (lambda,state',wrapped) = type_lambda e state lambda in
    return_wrapped (E_lambda lambda) (* TODO: is the type of the entire lambda enough to access the input_type=fresh; ? *)
        state' wrapped

  | E_recursive {fun_name;fun_type;lambda} ->
    let%bind fun_type = evaluate_type e fun_type in
    let e = Environment.add_ez_binder fun_name fun_type e in
    let%bind (lambda,state,_) = type_lambda e state lambda in
    let wrapped = Wrap.recursive fun_type in
    return_wrapped (E_recursive {fun_name;fun_type;lambda}) state wrapped

  | E_constant {cons_name=name; arguments=lst} ->
    let name = convert_constant' name in
    let%bind t = Typer_common.Constant_typers_new.Operators_types.constant_type name in
    let aux acc expr =
      let (lst , state) = acc in
      let%bind (expr, state') = type_expression e state expr in
      ok (expr::lst , state') in
    let%bind (lst , state') = bind_fold_list aux ([], state) lst in
    let lst_annot = List.map (fun (x : O.expression) -> x.type_expression) lst in
    let wrapped = Wrap.constant t lst_annot in
    return_wrapped
      (E_constant {cons_name=name;arguments=lst})
      state' wrapped
      (*
      let%bind lst' = bind_list @@ List.map (type_expression e) lst in
      let tv_lst = List.map get_type_annotation lst' in
      let%bind (name', tv) =
        type_constant name tv_lst tv_opt ae.location in
      return (E_constant (name' , lst')) tv
    *)

and type_lambda e state {
      binder ;
      input_type ;
      output_type ;
      result ;
    } =
      let%bind input_type' = bind_map_option (evaluate_type e) input_type in
      let%bind output_type' = bind_map_option (evaluate_type e) output_type in

      let fresh : O.type_expression = t_variable (Wrap.fresh_binder ()) () in
      let e' = Environment.add_ez_binder (binder) fresh e in

      let%bind (result , state') = type_expression e' state result in
      let wrapped = Wrap.lambda fresh input_type' output_type' result.type_expression in
      ok (({binder;result}:O.lambda),state',wrapped)

and type_constant (name:I.constant') (lst:O.type_expression list) (tv_opt:O.type_expression option) : (O.constant' * O.type_expression, Typer_common.Errors.typer_error) result =
  let name = convert_constant' name in
  let%bind typer = Typer_common.Constant_typers.constant_typers name in
  let%bind tv = typer lst tv_opt in
  ok(name, tv)

(* Apply type_declaration on every node of the AST_core from the root p *)
let type_program_returns_state ((env, state, p) : environment * O'.typer_state * I.program) : (environment * O'.typer_state * O.program, Typer_common.Errors.typer_error) result =
  let aux ((e : environment), (s : O'.typer_state) , (ds : O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let%bind (e' , s' , d'_opt) = type_declaration e s (Location.unwrap d) in
    let ds' = match d'_opt with
      | None -> ds
      | Some d' -> Location.wrap ~loc:(Location.get_location d) d' :: ds
    in
    ok (e' , s' , ds')
  in
  let%bind (env' , state' , declarations) =
    trace (program_error_tracer p) @@
    bind_fold_list aux (env , state , []) p in
  let declarations = List.rev declarations in (* Common hack to have O(1) append: prepend and then reverse *)
  ok (env', state', declarations)

let print_env_state_node (node_printer : Format.formatter -> 'a -> unit) ((env,state,node) : environment * O'.typer_state * 'a) =
  ignore node; (* TODO *)
  Printf.printf "%s" @@
    Format.asprintf "{ \"ENV\": %a,\n\"STATE\": %a,\n\"NODE\": %a\n},\n"
      Ast_typed.PP_json.environment env
      Typesystem.Solver_types.json_typer_state state
      node_printer node

let type_and_subst_xyz
      (in_printer : Format.formatter -> 'a -> unit)
      (out_printer : Format.formatter -> 'b -> unit)
      (env_state_node : environment * O'.typer_state * 'a)
      (apply_substs : ('b , Typer_common.Errors.typer_error) Typesystem.Misc.Substitution.Pattern.w)
      (type_xyz_returns_state : (environment * O'.typer_state * 'a) -> (environment * O'.typer_state * 'b , typer_error) Trace.result)
    : ('b * O'.typer_state , typer_error) result =
  let () = (if Ast_typed.Debug.json_new_typer then Printf.printf "%!\n###############################START_OF_JSON\n[%!") in
  let () = (if Ast_typed.Debug.debug_new_typer then Printf.printf "\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env_state_node here.\n\n") in
  let () = (if Ast_typed.Debug.debug_new_typer || Ast_typed.Debug.json_new_typer then print_env_state_node in_printer env_state_node) in
  let%bind (env, state, node) = type_xyz_returns_state env_state_node in
  let subst_all =
    let aliases = state.structured_dbs.aliases in
    let assignments = state.structured_dbs.assignments in
    let substs : variable: I.type_variable -> _ = fun ~variable ->
      to_option @@
      let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%s" @@ Format.asprintf "TRY   %a\n" Var.pp variable) in
      let%bind root =
        trace_option (corner_case (Format.asprintf "can't find alias root of variable %a" Var.pp variable)) @@
          (* TODO: after upgrading UnionFind, this will be an option, not an exception. *)
          try Some (Solver.UF.repr variable aliases) with Not_found -> None in
      let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%s" @@ Format.asprintf "TRYR  %a (%a)\n" Var.pp variable Var.pp root) in
      let%bind assignment =
        trace_option (corner_case (Format.asprintf "can't find assignment for root %a" Var.pp root)) @@
          (Map.find_opt root assignments) in
      let O.{ tv ; c_tag ; tv_list } = assignment in
      let () = ignore tv (* I think there is an issue where the tv is stored twice (as a key and in the element itself) *) in
      let%bind (expr : O.type_content) = trace_option (corner_case "wrong constant tag") @@
        Typesystem.Core.type_expression'_of_simple_c_constant (c_tag , (List.map (fun s -> O.t_variable s ()) tv_list)) in
      let () = (if Ast_typed.Debug.debug_new_typer then Printf.fprintf stderr "%s" @@ Format.asprintf "SUBST %a (%a is %a)\n" Var.pp variable Var.pp root Ast_typed.PP_generic.type_content expr) in
      ok @@ expr
    in
    let p = apply_substs ~substs node in
    p in
  let%bind node = subst_all in
  let () = (if Ast_typed.Debug.debug_new_typer then Printf.printf "\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env,state,node here again.\n\n") in
  let () = (if Ast_typed.Debug.debug_new_typer || Ast_typed.Debug.json_new_typer then print_env_state_node out_printer (env, state, node)) in
  let () = (if Ast_typed.Debug.json_new_typer then Printf.printf "%!\"end of JSON\"],\n###############################END_OF_JSON\n%!") in
  let () = ignore env in        (* TODO: shouldn't we use the `env` somewhere? *)
  ok (node, state)

let type_program (p : I.program) : (O.program * O'.typer_state, typer_error) result =
  let empty_env = DEnv.default in
  let empty_state = Solver.initial_state in
  type_and_subst_xyz (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.program\"") Ast_typed.PP_json.program (empty_env , empty_state , p) Typesystem.Misc.Substitution.Pattern.s_program type_program_returns_state

let type_expression_returns_state : (environment * O'.typer_state * I.expression) -> (environment * O'.typer_state * O.expression, typer_error) result =
  fun (env, state, e) ->
  let%bind (e , state) = type_expression env state e in
  ok (env, state, e)

let type_expression_subst (env : environment) (state : O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (O.expression * O'.typer_state , typer_error) result =
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  type_and_subst_xyz (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.expression\"") Ast_typed.PP_json.expression (env , state , e) Typesystem.Misc.Substitution.Pattern.s_expression type_expression_returns_state

let untype_type_expression  = Untyper.untype_type_expression
let untype_expression       = Untyper.untype_expression

(* These aliases are just here for quick navigation during debug, and can safely be removed later *)
let [@warning "-32"] (*rec*) type_declaration _env _state : I.declaration -> (environment * O'.typer_state * O.declaration option, typer_error) result = type_declaration _env _state
and [@warning "-32"] type_match : environment -> O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> (O.matching_expr * O'.typer_state, typer_error) result = type_match
and [@warning "-32"] evaluate_type (e:environment) (t:I.type_expression) : (O.type_expression, typer_error) result = evaluate_type e t
and [@warning "-32"] type_expression : environment -> O'.typer_state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * O'.typer_state, typer_error) result = type_expression
and [@warning "-32"] type_lambda e state lam = type_lambda e state lam
and [@warning "-32"] type_constant (name:I.constant') (lst:O.type_expression list) (tv_opt:O.type_expression option) : (O.constant' * O.type_expression, typer_error) result = type_constant name lst tv_opt
let [@warning "-32"] type_program_returns_state ((env, state, p) : environment * O'.typer_state * I.program) : (environment * O'.typer_state * O.program, typer_error) result = type_program_returns_state (env, state, p)
let [@warning "-32"] type_and_subst_xyz (in_printer : (Format.formatter -> 'a -> unit)) (out_printer : (Format.formatter -> 'b -> unit)) (env_state_node : environment * O'.typer_state * 'a) (apply_substs : ('b,typer_error) Typesystem.Misc.Substitution.Pattern.w) (type_xyz_returns_state : (environment * O'.typer_state * 'a) -> (environment * O'.typer_state * 'b, typer_error) result) : ('b * O'.typer_state, typer_error) result = type_and_subst_xyz in_printer out_printer env_state_node apply_substs type_xyz_returns_state
let [@warning "-32"] type_program (p : I.program) : (O.program * O'.typer_state, typer_error) result = type_program p
let [@warning "-32"] type_expression_returns_state : (environment * O'.typer_state * I.expression) -> (environment * O'.typer_state * O.expression, typer_error) Trace.result = type_expression_returns_state
let [@warning "-32"] type_expression_subst (env : environment) (state : O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (O.expression * O'.typer_state, typer_error) result = type_expression_subst env state ?tv_opt e
