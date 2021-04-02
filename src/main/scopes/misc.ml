open Types
open Trace

type tenv = Ast_typed.environment

let var_equal : Ast_typed.expression_variable -> Ast_typed.expression_variable -> bool = fun v1 v2 ->
  Var.equal v1.wrap_content v2.wrap_content

(* type t_bindings = (Ast_typed.expression_variable * Ast_typed.type_expression) list *)
let extract_variable_types :
  bindings_map -> Ast_typed.module_fully_typed -> bindings_map =
  fun prev prg ->
    let add env b =
      let aux : Ast_typed.expression_variable *  Ast_typed.type_expression -> Ast_typed.expression_variable * Ast_typed.type_expression = fun (v,t) ->
        let t' = match t.orig_var with Some t' -> { t with type_content = T_variable t'} | None -> (* let () = Format.printf "\nYAA\n NONE : %a" Ast_typed.PP.type_expression t in *) t in
        (v,t')
      in
      let b' = List.map aux b in
      ok @@ Bindings_map.add_bindings b' env
    in
    let aux : bindings_map -> Ast_typed.expression -> (bindings_map, _) result = fun env exp ->
      let return = add env in
      match exp.expression_content with
      | E_literal _ | E_application _ | E_raw_code _ | E_constructor _
      | E_type_in _ | E_mod_in _ | E_mod_alias _
      | E_record _ | E_record_accessor _ | E_record_update _ | E_constant _ -> return []
      | E_module_accessor _ -> return []
      | E_variable v -> return [(v,exp.type_expression)]
      | E_lambda { binder ; _ } ->
        let in_t = match exp.type_expression.type_content with
          | T_arrow { type1 ; _ } -> type1
          | _ -> failwith "lambda does not have type arrow"
        in
        return [binder,in_t]
      | E_recursive { fun_name ; fun_type ; lambda = { binder ; _ } } ->
        let in_t = match fun_type.type_content with
          | T_arrow { type1 ; _ } -> type1
          | _ -> failwith "rec fun does not have type arrow"
        in
        return [ (fun_name , fun_type) ; (binder , in_t) ]
      | E_let_in { let_binder ; rhs ; _ } ->
        return @@ [(let_binder,rhs.type_expression)]
      | E_matching {matchee ; cases } -> (
        match cases with
        | Match_list {match_nil = _ ; match_cons = {hd ; tl ; _}} ->
          let proj_t = match Ast_typed.get_t_list matchee.type_expression with
            | Some t -> t
            | None -> failwith "match_list projection does not have type list"
          in
          return [ (tl , matchee.type_expression) ; (hd , proj_t) ]
        | Match_option {match_none = _ ; match_some = {opt; }} ->
          let proj_t = match Ast_typed.get_t_option matchee.type_expression with
            | Some t -> t
            | None -> failwith "match_option projection does not have type option"
          in
          return [ (opt,proj_t) ]
        | Match_variant {cases ; tv=_} ->
          let variant_t = match Ast_typed.get_t_sum matchee.type_expression with
            | Some t -> t
            | None -> failwith "match_variant projection does not have type sum"
          in
          let aux :
            Ast_typed.matching_content_case -> (Ast_typed.expression_variable * Ast_typed.type_expression) =
            fun { constructor ; pattern ; _ } ->
              let proj_t = (Ast_core.LMap.find constructor variant_t.content).associated_type in
              (pattern,proj_t)
          in
          return (List.map aux cases)
        | Match_record { fields ; _ }  ->
          return (Ast_typed.LMap.to_list fields)
      )
    in
    let aux' : bindings_map -> Ast_typed.declaration -> (bindings_map, _) result = fun env decl ->
      match decl with
      | Declaration_constant { binder ; expr ; _ } -> add env [binder,expr.type_expression]
      | Declaration_type _ -> ok env
      | Declaration_module _ -> ok env
      | Module_alias _ -> ok env
    in
    match to_option @@ Self_ast_typed.Helpers.fold_module_decl aux aux' prev prg with
    | Some bindings -> bindings
    | None -> prev

let get_binder_name : 'a Var.t -> string = fun v ->
  if Var.is_generated v
  then "generated"
  else Var.to_name v

let make_def_id name i =
  (name ^ "#" ^ (string_of_int i), i+1)

let add_shadowing_def : (int * _ Var.t) -> def -> def_map -> (int * def_map) =  fun (i,var) def env ->
  if Var.is_generated var then (i,env)
  else
    let name = get_binder_name var in
    let (definition_id,i) = make_def_id name i in
    let shadow = Def_map.filter
      (fun _ s_def -> match def, s_def with
        | Variable _ , Variable _ | Type _ , Type _ ->
          not @@ String.equal (get_def_name s_def) name
        | _ -> true )
      env in
    let env = Def_map.add definition_id def shadow in
    (i,env)

type type_ppx = Ast_typed.type_expression -> Ast_typed.type_expression

let resolve_if :
  with_types:bool -> ?ppx:type_ppx -> bindings_map -> Ast_core.expression_variable -> type_case =
  fun ~with_types ?(ppx = fun i -> i) bindings var ->
    if with_types then (
      let t_opt = Bindings_map.find_opt var bindings in
      match t_opt with
      | Some t -> Resolved (ppx t)
      | None -> Unresolved
    )
    else Unresolved

let make_v_def_from_core :
  with_types:bool -> bindings_map -> Ast_core.expression_variable -> Location.t -> Location.t -> def =
  fun ~with_types bindings var range body_range ->
    let name' = get_binder_name var.wrap_content in
    let type_case = resolve_if ~with_types bindings var in
    make_v_def name' type_case range body_range

let make_v_def_option_type :
  with_types:bool -> bindings_map -> Ast_core.expression_variable -> Ast_core.type_expression option -> Location.t -> Location.t -> def =
  fun ~with_types bindings var core_t_opt range body_range ->
    let name' = get_binder_name var.wrap_content in
    let type_case = match core_t_opt with
      | Some t -> Core t
      | None -> resolve_if ~with_types bindings var
    in
    make_v_def name' type_case range body_range
