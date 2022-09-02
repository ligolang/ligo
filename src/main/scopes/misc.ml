open Ligo_prim
open Types

type tenv = Environment.t

let extract_variable_types :
  bindings_map -> Ast_typed.declaration_content -> bindings_map =
  fun prev decl ->
    let add env b =
      let aux : ValueVar.t *  Ast_typed.type_expression -> ValueVar.t * Ast_typed.type_expression = fun (v,t) ->
        let t' = match t.orig_var with Some t' -> { t with type_content = T_variable t'} | None -> (* let () = Format.printf "\nYAA\n NONE : %a" Ast_typed.PP.type_expression t in *) t in
        (v,t')
      in
      let b' = List.map ~f:aux b in
      Bindings_map.add_bindings b' env
    in
    let aux : bindings_map -> Ast_typed.expression -> bindings_map = fun env exp ->
      let return = add env in
      match exp.expression_content with
      | E_literal _ | E_application _ | E_raw_code _ | E_constructor _ | E_assign _
      | E_type_abstraction _ | E_mod_in _
      | E_record _ | E_accessor _ | E_update _ | E_constant _ -> return []
      | E_module_accessor _ -> return []
      | E_type_inst _ -> return [] (* TODO *)
      | E_variable v -> return [(v,exp.type_expression)]
      | E_lambda { binder ; _ } ->
        let rec in_t t = match t.Ast_typed.type_content with
          | T_arrow { type1 ; _ } -> type1
          | T_for_all { type_ ; _ } -> in_t type_
          | _ -> failwith "lambda does not have type arrow"
        in
        let in_t = in_t exp.type_expression in
        return [binder.var,in_t]
      | E_recursive { fun_name ; fun_type ; lambda = { binder ; _ } } ->
        let in_t = match fun_type.type_content with
          | T_arrow { type1 ; _ } -> type1
          | _ -> failwith "rec fun does not have type arrow"
        in
        return [ (fun_name , fun_type) ; (binder.var , in_t) ]
      | E_let_in { let_binder ; rhs ; _ } ->
        return @@ [(let_binder.var,rhs.type_expression)]
      | E_matching {matchee ; cases } -> (
        match cases with
        | Match_variant {cases ; tv=_} -> (
          match Ast_typed.get_t_sum matchee.type_expression with
            | Some variant_t ->
              let aux : _ Ast_typed.matching_content_case -> (ValueVar.t * Ast_typed.type_expression) =
                fun { constructor ; pattern ; _ } ->
                  let proj_t = (Record.LMap.find constructor variant_t.fields).associated_type in
                  (pattern,proj_t)
              in
              return (List.map ~f:aux cases)
            | None -> (
              match Ast_typed.get_t_list matchee.type_expression with
              | Some list_proj ->
                let x = List.find_exn ~f:(fun ({constructor=Label l;_}: _ Ast_typed.matching_content_case) -> String.equal l "Cons") cases in
                let t = Ast_typed.t_pair list_proj matchee.type_expression in
                return [(x.pattern,t)]
              | None -> failwith "matched value in the Match_variant: wrong type"
            )
        )
        | Match_record { fields ; _ }  ->
          let aux = fun Binder.{var;ascr;attributes=_} -> (var, ascr) in
          return (List.map ~f:aux @@ Record.LMap.to_list fields)
      )
    in
    match decl with
    | D_value { attr = { hidden = true ; _ } ; _ } -> prev
    | D_value { binder ; expr ; _ } ->
      let prev = add prev [binder.var,expr.type_expression] in
      Self_ast_typed.Helpers.fold_expression aux prev expr
    | D_type _ -> prev
    | D_module _ -> prev

let generated_flag = "#?generated"
let get_binder_name : ValueVar.t -> string = fun v ->
  if ValueVar.is_generated v
  then generated_flag
  else ValueVar.to_name_exn v

let get_type_binder_name : TypeVar.t -> string = fun v ->
  if TypeVar.is_generated v
  then generated_flag
  else TypeVar.to_name_exn v
let get_mod_binder_name : ModuleVar.t -> string = fun v ->
  if ModuleVar.is_generated v
  then generated_flag
  else ModuleVar.to_name_exn v

let make_def_id name i =
  (name ^ "#" ^ (string_of_int i), i+1)

let add_shadowing_def : (int * string) -> def -> def_map -> (int * def_map) =  fun (i,name) def env ->
  match get_def_name def with
  | x when String.equal x generated_flag -> (i,env)
  | _ ->
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
  with_types:bool -> ?ppx:type_ppx -> bindings_map -> ValueVar.t -> type_case =
  fun ~with_types ?(ppx = fun i -> i) bindings var ->
    if with_types then (
      let t_opt = Bindings_map.find_opt var bindings in
      match t_opt with
      | Some t -> Resolved (ppx t)
      | None -> Unresolved
    )
    else Unresolved

let make_v_def_from_core :
  with_types:bool -> bindings_map -> ValueVar.t -> Location.t -> Location.t -> def =
  fun ~with_types bindings var range body_range ->
    let type_case = resolve_if ~with_types bindings var in
    make_v_def (get_binder_name var) type_case range body_range

let make_v_def_option_type :
  with_types:bool -> bindings_map -> ValueVar.t -> Ast_core.type_expression option -> Location.t -> Location.t -> def =
  fun ~with_types bindings var core_t_opt range body_range ->
    let type_case = match core_t_opt with
      | Some t -> Core t
      | None -> resolve_if ~with_types bindings var
    in
    make_v_def (get_binder_name var) type_case range body_range
