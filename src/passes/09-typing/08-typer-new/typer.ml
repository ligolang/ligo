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

(* TODO : find a better name for fonction that are called "type_something".
They are not typing per say, just add a type variable to all expression and make the appropriate constraints *)

let assert_type_expression_eq ((tv',tv):O.type_expression * O.type_expression) : (unit,typer_error) result = 
  Compare_types.assert_type_expression_eq (tv' , tv)

let cast_var (orig: 'a Var.t Location.wrap) = { orig with wrap_content = Var.todo_cast orig.wrap_content}

(*
  Extract pairs of (name,type) in the declaration and add it to the environment
*)
let rec type_declaration env state : I.declaration -> (environment * _ O'.typer_state * O.declaration, typer_error) result = function
  | Declaration_type {type_binder; type_expr} ->
    let type_binder = Var.todo_cast type_binder in
    let%bind type_expr = evaluate_type env type_expr in
    let env' = Environment.add_type (type_binder) type_expr env in
    ok (env', state , O.Declaration_type {type_binder; type_expr})
  | Declaration_constant {binder; type_opt; attr; expr} -> (
    (*
      Determine the type of the expression and add it to the environment
    *)
    let%bind tv_opt = bind_map_option (evaluate_type env) type_opt in
    let%bind (state', expr) =
      trace (constant_declaration_tracer binder expr tv_opt) @@
      type_expression env state ?tv_opt expr in
    let binder = Location.map Var.todo_cast binder in
    let post_env = Environment.add_ez_declaration binder expr env in
    ok (post_env, state' , O.Declaration_constant { binder ; expr ; inline=attr.inline})
    )

(*
  Recursively search the type_expression and return a result containing the
  type_value at the leaves
*)
and evaluate_type : environment -> I.type_expression -> (O.type_expression, typer_error) result = fun e t ->
  let return tv' = ok (make_t ~loc:t.location tv' (Some t)) in
  match t.content with
  | T_arrow {type1;type2} ->
    let%bind type1 = evaluate_type e type1 in
    let%bind type2 = evaluate_type e type2 in
    return (T_arrow {type1;type2})
  | T_sum m ->
    let aux v =
      let {associated_type ; michelson_annotation ; decl_pos} : I.row_element = v in
      let%bind associated_type = evaluate_type e associated_type in
      ok @@ ({associated_type ; michelson_annotation ; decl_pos}:O.row_element)
    in
    let%bind m = Stage_common.Helpers.bind_map_lmap aux m in
    return (T_sum m)
  | T_record m ->
    let aux v =
      let {associated_type ; michelson_annotation ; decl_pos} : I.row_element = v in
      let%bind associated_type = evaluate_type e associated_type in
      ok @@ ({associated_type ; michelson_annotation ; decl_pos}:O.row_element)
    in
    let%bind m = Stage_common.Helpers.bind_map_lmap aux m in
    return (T_record m)
  | T_variable name ->
    (* Check that the variable is in the environment *)
    let name : O.type_variable = Var.todo_cast name in
    trace_option (unbound_type_variable e name t.location)
    @@ Environment.get_type_opt (name) e
  | T_wildcard ->
    return @@ T_variable (Var.fresh ())
  | T_constant {type_constant; arguments} ->
    let assert_constant lst = match lst with
      [] -> ok () 
    | _ -> fail @@ type_constant_wrong_number_of_arguments type_constant 0 (List.length lst) t.location
    in
    let assert_unary lst = match lst with
      [_] -> ok () 
    | _ -> fail @@ type_constant_wrong_number_of_arguments type_constant 1 (List.length lst) t.location
    in
    let get_unary lst = match lst with
      [x] -> ok x
    | _ -> fail @@ type_constant_wrong_number_of_arguments type_constant 1 (List.length lst) t.location
    in
    let assert_binary lst = match lst with
      [_;_] -> ok ()
    | _ -> fail @@ type_constant_wrong_number_of_arguments type_constant 2 (List.length lst) t.location
    in
    match type_constant with
      | (TC_unit | TC_string | TC_bytes | TC_nat | TC_int | TC_mutez | TC_operation | TC_address | TC_key | TC_key_hash | TC_chain_id | TC_signature | TC_timestamp) as type_constant ->
        let%bind () = assert_constant arguments in
        let%bind arguments = bind_map_list (evaluate_type e) arguments in
        return @@ T_constant {type_constant; arguments}
      | (TC_set | TC_list | TC_option | TC_contract) as type_constant ->
        let%bind () = assert_unary arguments in
        let%bind arguments = bind_map_list (evaluate_type e) arguments in
        return @@ T_constant {type_constant; arguments}
      | (TC_map | TC_big_map ) as type_constant ->
        let%bind () = assert_binary arguments in
        let%bind arguments = bind_map_list (evaluate_type e) arguments in
        return @@ T_constant {type_constant; arguments}
      (* TODO : remove when we have polymorphism *)
      | TC_map_or_big_map ->
        let%bind () = assert_binary arguments in
        let%bind arguments = bind_map_list (evaluate_type e) arguments in
        return @@ T_constant {type_constant=TC_map_or_big_map; arguments}
      | TC_michelson_pair_right_comb ->
          let%bind c = bind (evaluate_type e) @@ get_unary arguments in
          let%bind lmap = match c.type_content with
            | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap)) -> ok lmap
            | _ -> fail (michelson_comb_no_record t.location) in
          let record = Typer_common.Michelson_type_converter.convert_pair_to_right_comb (Ast_typed.LMap.to_kv_list lmap) in
          return @@ record
      | TC_michelson_pair_left_comb ->
          let%bind c = bind (evaluate_type e) @@ get_unary arguments in
          let%bind lmap = match c.type_content with
            | T_record lmap when (not (Ast_typed.Helpers.is_tuple_lmap lmap)) -> ok lmap
            | _ -> fail (michelson_comb_no_record t.location) in
          let record = Typer_common.Michelson_type_converter.convert_pair_to_left_comb (Ast_typed.LMap.to_kv_list lmap) in
          return @@ record
      | TC_michelson_or_right_comb ->
          let%bind c = bind (evaluate_type e) @@ get_unary arguments in
          let%bind cmap = match c.type_content with
            | T_sum cmap -> ok cmap
            | _ -> fail (michelson_comb_no_variant t.location) in
          let pair = Typer_common.Michelson_type_converter.convert_variant_to_right_comb (Ast_typed.LMap.to_kv_list cmap) in
          return @@ pair
      | TC_michelson_or_left_comb ->
          let%bind c = bind (evaluate_type e) @@ get_unary arguments in
          let%bind cmap = match c.type_content with
            | T_sum cmap -> ok cmap
            | _ -> fail (michelson_comb_no_variant t.location) in
          let pair = Typer_common.Michelson_type_converter.convert_variant_to_left_comb (Ast_typed.LMap.to_kv_list cmap) in
          return @@ pair
      | _ -> fail @@ unrecognized_type_constant t

and type_expression : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> (_ O'.typer_state * O.expression, typer_error) result = fun ?tv_opt e state ae ->
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  let open Solver in
  let module L = Logger.Stateful() in
  let return : _ -> _ O'.typer_state -> _ -> _ (* return of type_expression *) = fun expr state constraints type_name ->
    let%bind new_state = aggregate_constraints state constraints in
    let tv = t_variable type_name () in
    let location = ae.location in
    let expr' = make_e ~location expr tv in
    ok @@ (new_state, expr') in
  let return_wrapped expr state (constraints , expr_type) = return expr state constraints expr_type in
  trace (expression_tracer ae) @@
  match ae.content with

  (* TODO: this file should take care only of the order in which program fragments
     are translated by Wrap.xyz

     TODO: produce an ordered list of sub-fragments, and use a common piece of code
     to actually perform the recursive calls *)

  (* Basic *)
  | E_variable name -> (
    (* Check that the variable exist in the environment and add a new constraint *)
    let name: O.expression_variable = Location.map Var.todo_cast name in
    let%bind (tv' : Environment.element) =
      trace_option (unbound_variable e name ae.location)
      @@ Environment.get_opt name e in
    let (constraints , expr_type) = Wrap.variable tv'.type_value in
    let expr' = e_variable name in
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

  | E_constant {cons_name; arguments=lst} ->
    let%bind t = Typer_common.Constant_typers_new.Operators_types.constant_type cons_name in
    let%bind state',lst = bind_fold_map_list (type_expression e) state lst in
    let lst_annot = List.map get_type_expression lst in
    let wrapped = Wrap.constant t lst_annot in
    return_wrapped (E_constant {cons_name;arguments=lst}) state' wrapped

  | E_lambda lambda -> 
    let%bind (lambda,state',wrapped) = type_lambda e state lambda in
    return_wrapped (E_lambda lambda) state' wrapped

  | E_application {lamb;args} ->
    let%bind (state' , lamb)  = type_expression e state lamb in
    let%bind (state'', args) = type_expression e state' args in
    let wrapped = Wrap.application lamb.type_expression args.type_expression in
    return_wrapped (E_application {lamb;args}) state'' wrapped

  (* Sum *)
  | E_constructor {constructor;element} ->
    (* Check that the constructor is from an existing variant *)
    let%bind (c_tv, sum_tv) = trace_option (unbound_constructor e constructor ae.location) @@
      Environment.get_constructor constructor e in
    let%bind (state', element) = type_expression e state element in
    (* Check that the element in the variant as the proper type *)
    (* TODO: infer the variant or the type of the element *)
    let%bind _assert = assert_type_expression_eq (element.type_expression, c_tv) in
    let wrapped = Wrap.constructor element.type_expression c_tv sum_tv in
    return_wrapped (E_constructor {constructor; element}) state' wrapped

  | E_matching {matchee;cases} -> (
      let%bind (state'  , ex') = type_expression e state matchee in
      let%bind (state'' , m' ) = type_match e state' ex'.type_expression cases ae ae.location in
      let tvs =
        let aux (cur : O.matching_expr) =
          match cur with
          | Match_list { match_nil ; match_cons = { hd=_ ; tl=_ ; body ; tv=_} } -> [ match_nil ; body ]
          | Match_option { match_none ; match_some = {opt=_; body; tv=_} } -> [ match_none ; body ]
          | Match_variant { cases ; tv=_ } -> List.map (fun ({constructor=_; pattern=_; body} : O.matching_content_case) -> body) cases in
        List.map get_type_expression @@ aux m' in
      (* constraints:
         all the items of tvs should be equal to the first one
         result = first item of tvs
      *)
      (* ?TODO?: Check that the type of the matching has the variant type corresponding to the case *)
      let wrapped = Wrap.matching tvs in
      return_wrapped (O.E_matching {matchee=ex';cases=m'}) state'' wrapped
    )

  (* Record *)
  | E_record m ->
    let aux state _ expr = type_expression e state expr in
    let%bind (state', m') = Stage_common.Helpers.bind_fold_map_lmap aux state m in
    (* Do we need row_element for AST_typed ? *)
    let wrapped = Wrap.record (O.LMap.map (fun e -> ({associated_type = get_type_expression e ; michelson_annotation = None ; decl_pos = 0}: O.row_element)) m') in
    return_wrapped (E_record m') state' wrapped

  | E_record_accessor {record;path} -> (
      let%bind (state', base') = type_expression e state record in
      let wrapped = Wrap.access_label ~base:base'.type_expression ~label:path in
      return_wrapped (E_record_accessor {record=base';path}) state' wrapped
    )

  | E_record_update {record; path; update} ->
    let%bind (state, record) = type_expression e state record in
    let%bind (state, update) = type_expression e state update in
    let wrapped = get_type_expression record in
    let%bind (wrapped,tv) = 
      match wrapped.type_content with 
      | T_record record -> (
          let%bind {associated_type;_} = trace_option (bad_record_access path ae wrapped update.location) @@
            O.LMap.find_opt path record in
          ok (record, associated_type)
      )
      (* TODO: write a real error *)
      | _ -> failwith "Update an expression which is not a record"
    in
    (* Check that the expression type is compatible with the field type *)
    let%bind () = assert_type_expression_eq (tv, get_type_expression update) in
    (* TODO: wrap.record_update *)
    return_wrapped (E_record_update {record; path; update}) state (Wrap.record wrapped)

  (* Advanced *)
  | E_let_in {let_binder ; rhs ; let_result; inline} ->
    let%bind rhs_tv_opt = bind_map_option (evaluate_type e) (let_binder.ascr) in
    (* TODO in abstractor : the binder annotation should just be an annotation node *)
    let%bind (state', rhs) = type_expression e state rhs in
    let let_binder = cast_var let_binder.binder in 
    let e' = Environment.add_ez_declaration let_binder rhs e in
    let%bind (state'', let_result) = type_expression e' state' let_result in
    let wrapped =
      Wrap.let_in rhs.type_expression rhs_tv_opt let_result.type_expression in
    return_wrapped (E_let_in {let_binder; rhs; let_result; inline}) state'' wrapped

  | E_recursive {fun_name;fun_type;lambda} ->
    (* Add the function name to the environment before evaluating the lambda*)
    let fun_name = cast_var fun_name in
    let%bind fun_type = evaluate_type e fun_type in
    let e = Environment.add_ez_binder fun_name fun_type e in
    let%bind (lambda,state,_) = type_lambda e state lambda in
    let wrapped = Wrap.recursive fun_type in
    return_wrapped (E_recursive {fun_name;fun_type;lambda}) state wrapped

  | E_raw_code {language ; code} ->
    (* The code is a string with an annotation*)
    let%bind state',code = type_expression e state code in
    let wrapped = Wrap.raw_code code.type_expression in
    return_wrapped (E_raw_code {language; code}) state' wrapped
  | E_ascription {anno_expr;type_annotation} ->
    let%bind tv = evaluate_type e type_annotation in
    let%bind (state', expr') = type_expression e state anno_expr in
    let wrapped = Wrap.annotation expr'.type_expression tv
    (* TODO: we're probably discarding too much by using expr'.expression.
       Previously: {expr' with type_annotation = the_explicit_type_annotation}
       but then this case is not like the others and doesn't call return_wrapped,
       which might do some necessary work *)
    in return_wrapped expr'.expression_content state' wrapped

and type_lambda e state {
      binder ;
      input_type ;
      output_type ;
      result ;
    } =
      let binder = cast_var binder in
      let%bind input_type'  = bind_map_option (evaluate_type e) input_type in
      let%bind output_type' = bind_map_option (evaluate_type e) output_type in

      let fresh : O.type_expression = t_variable (Wrap.fresh_binder ()) () in
      let e' = Environment.add_ez_binder (binder) fresh e in

      let%bind (state', result) = type_expression e' state result in
      let wrapped = Wrap.lambda fresh input_type' output_type' result.type_expression in
      ok (({binder;result}:O.lambda),state',wrapped)

(* TODO: Rewrite this entire function *)
and type_match : environment -> _ O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> (_ O'.typer_state * O.matching_expr, typer_error) result =
  fun e state t i _ae loc ->
  let return state m = ok @@ (state, m) in
  match i with
    | Match_option {match_none ; match_some} ->
      let%bind tv =
        trace_option (match_error ~expected:i ~actual:t loc)
        @@ get_t_option t in
      let%bind (state', match_none) = type_expression e state match_none in
      let {opt; body}:I.match_some = match_some in
      let opt = cast_var opt in
      (* Add the binder just for typing the case *)
      let e' = Environment.add_ez_binder opt tv e in
      let%bind (state'', body) = type_expression e' state' body in
      return state'' @@ O.Match_option {match_none ; match_some = { opt; body; tv}}
    | Match_list {match_nil ; match_cons} ->
      let%bind t_elt =
        trace_option (match_error ~expected:i ~actual:t loc)
        @@ get_t_list t in
      let%bind (state', match_nil) = type_expression e state match_nil in
      let {hd; tl; body} : I.match_cons = match_cons in
      let hd = cast_var hd in
      let tl = cast_var tl in
      (* Add the binder just for typing the case *)
      let e' = Environment.add_ez_binder hd t_elt e in
      let e' = Environment.add_ez_binder tl t e' in
      let%bind (state'', body) = type_expression e' state' body in
      return state'' @@ O.Match_list {match_nil ; match_cons = {hd; tl; body;tv=t}}
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
        let variant_cases = List.map fst @@ O.LMap.to_kv_list variant_cases' in
        let match_cases = List.map (fun ({constructor;_} : I.match_variant) -> constructor) lst in
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
      (* Convert constructors *)
      let%bind (state'' , cases) =
        let aux state ({constructor; proj; body}: I.match_variant) =
          let%bind (constructor_type , _) =
            trace_option (unbound_constructor e constructor loc) @@
            Environment.get_constructor constructor e in
          let pattern = cast_var proj in
          let e' = Environment.add_ez_binder pattern constructor_type e in
          let%bind (state', body) = type_expression e' state body in
          let constructor = constructor in
          ok (state' , ({constructor ; pattern ; body} : O.matching_content_case))
        in
        bind_fold_map_list aux state lst in
      return state'' @@ O.Match_variant {cases ; tv=variant }

(* Apply type_declaration on every node of the AST_core from the root p *)
let type_program_returns_env ((env, state, p) : environment * _ O'.typer_state * I.program) : (environment * _ O'.typer_state * O.program, Typer_common.Errors.typer_error) result =
  let aux ((e : environment), (s : _ O'.typer_state) , (ds : O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let%bind (e' , s' , d') = type_declaration e s (Location.unwrap d) in
    (* TODO: Move this filter to the spiller *)
    let ds' = match d' with
      | O.Declaration_type _ -> ds
      | _ -> Location.wrap ~loc:(Location.get_location d) d' :: ds
    in
    ok (e' , s' , ds')
  in
  let%bind (env' , state' , declarations) =
    trace (program_error_tracer p) @@
    bind_fold_list aux (env , state , []) p in
  let declarations = List.rev declarations in (* Common hack to have O(1) append: prepend and then reverse *)
  ok (env', state', declarations)

let print_env_state_node (node_printer : Format.formatter -> 'a -> unit) ((env,state,node) : environment * _ O'.typer_state * 'a) =
  ignore node; (* TODO *)
  Printf.printf "%s" @@
    Format.asprintf "{ \"ENV\": %a,\n\"STATE\": %a,\n\"NODE\": %a\n},\n"
      Ast_typed.PP_json.environment env
      Typesystem.Solver_types.json_typer_state state
      node_printer node

let type_and_subst
      (in_printer : Format.formatter -> 'a -> unit)
      (out_printer : Format.formatter -> 'b -> unit)
      (env_state_node : environment * _ O'.typer_state * 'a)
      (apply_substs : ('b , Typer_common.Errors.typer_error) Typesystem.Misc.Substitution.Pattern.w)
      (types_and_returns_env : (environment * _ O'.typer_state * 'a) -> (environment * _ O'.typer_state * 'b , typer_error) Trace.result)
    : ('b * _ O'.typer_state , typer_error) result =
  let () = (if Ast_typed.Debug.json_new_typer then Printf.printf "%!\n###############################START_OF_JSON\n[%!") in
  let () = (if Ast_typed.Debug.debug_new_typer then Printf.printf "\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env_state_node here.\n\n") in
  let () = (if Ast_typed.Debug.debug_new_typer || Ast_typed.Debug.json_new_typer then print_env_state_node in_printer env_state_node) in
  let%bind (env, state, node) = types_and_returns_env env_state_node in
  let subst_all =
    let aliases = state.structured_dbs.aliases in
    let assignments = state.structured_dbs.assignments in
    let substs : variable: O.type_variable -> _ = fun ~variable ->
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
    apply_substs ~substs node
  in
  let%bind node = subst_all in
  let () = (if Ast_typed.Debug.debug_new_typer then Printf.printf "\nTODO AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA Print env,state,node here again.\n\n") in
  let () = (if Ast_typed.Debug.debug_new_typer || Ast_typed.Debug.json_new_typer then print_env_state_node out_printer (env, state, node)) in
  let () = (if Ast_typed.Debug.json_new_typer then Printf.printf "%!\"end of JSON\"],\n###############################END_OF_JSON\n%!") in
  let () = ignore env in        (* TODO: shouldn't we use the `env` somewhere? *)
  ok (node, state)

let type_program (p : I.program) : (O.program * _ O'.typer_state, typer_error) result =
  let empty_env = DEnv.default in
  let empty_state = Solver.initial_state in
  type_and_subst (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.program\"") Ast_typed.PP_json.program (empty_env , empty_state , p) Typesystem.Misc.Substitution.Pattern.s_program type_program_returns_env

(* Change the signature of type_expression for compatibility with type_and_subst *)
let type_expression_returns_env : (environment * _ O'.typer_state * I.expression) -> (environment * _ O'.typer_state * O.expression, typer_error) result =
  fun (env, state, e) ->
  let%bind (state, e) = type_expression env state e in
  ok (env, state, e)

let type_expression_subst (env : environment) (state : _ O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (O.expression * _ O'.typer_state , typer_error) result =
  let () = ignore tv_opt in     (* For compatibility with the old typer's API, this argument can be removed once the new typer is used. *)
  type_and_subst (fun ppf _v -> Format.fprintf ppf "\"no JSON yet for I.PP.expression\"") Ast_typed.PP_json.expression (env , state , e) Typesystem.Misc.Substitution.Pattern.s_expression type_expression_returns_env

let untype_type_expression  = Untyper.untype_type_expression
let untype_expression       = Untyper.untype_expression

(* These aliases are just here for quick navigation during debug, and can safely be removed later *)
let [@warning "-32"] (*rec*) type_declaration _env _state : I.declaration -> (environment * _ O'.typer_state * O.declaration, typer_error) result = type_declaration _env _state
and [@warning "-32"] type_match : environment -> _ O'.typer_state -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> (_ O'.typer_state * O.matching_expr, typer_error) result = type_match
and [@warning "-32"] evaluate_type (e:environment) (t:I.type_expression) : (O.type_expression, typer_error) result = evaluate_type e t
and [@warning "-32"] type_expression : ?tv_opt:O.type_expression -> environment -> _ O'.typer_state -> I.expression -> (_ O'.typer_state * O.expression, typer_error) result = type_expression
and [@warning "-32"] type_lambda e state lam = type_lambda e state lam
let [@warning "-32"] type_program_returns_env ((env, state, p) : environment * _ O'.typer_state * I.program) : (environment * _ O'.typer_state * O.program, typer_error) result = type_program_returns_env (env, state, p)
let [@warning "-32"] type_and_subst (in_printer : (Format.formatter -> 'a -> unit)) (out_printer : (Format.formatter -> 'b -> unit)) (env_state_node : environment * _ O'.typer_state * 'a) (apply_substs : ('b,typer_error) Typesystem.Misc.Substitution.Pattern.w) (types_and_returns_env : (environment * _ O'.typer_state * 'a) -> (environment * _ O'.typer_state * 'b, typer_error) result) : ('b * _ O'.typer_state, typer_error) result = type_and_subst in_printer out_printer env_state_node apply_substs types_and_returns_env
let [@warning "-32"] type_program (p : I.program) : (O.program * _ O'.typer_state, typer_error) result = type_program p
let [@warning "-32"] type_expression_returns_env : (environment * _ O'.typer_state * I.expression) -> (environment * _ O'.typer_state * O.expression, typer_error) Trace.result = type_expression_returns_env
let [@warning "-32"] type_expression_subst (env : environment) (state : _ O'.typer_state) ?(tv_opt : O.type_expression option) (e : I.expression) : (O.expression * _ O'.typer_state, typer_error) result = type_expression_subst env state ?tv_opt e
