open Ligo_prim
open Ast_typed.Types
open Simple_utils.Trace
module Ligo_string = Simple_utils.Ligo_string

type contract_pass_data = {
  contract_type : Helpers.contract_type ;
  main_name : Value_var.t ;
}

module VVarSet = Caml.Set.Make(Value_var)
module MVarMap = Simple_utils.Map.Make(Module_var)

type env = {env:env MVarMap.t;used_var:VVarSet.t}
let empty_env = {env=MVarMap.empty;used_var=VVarSet.empty}
let rec pp_env ppf env =
  Format.fprintf ppf "{env: %a;used_var: %a}"
    (Simple_utils.PP_helpers.list_sep_d (fun ppf (k,v) -> Format.fprintf ppf "(%a,%a)" Module_var.pp k pp_env v)) (MVarMap.to_kv_list env.env)
    (Simple_utils.PP_helpers.list_sep_d Value_var.pp) (VVarSet.elements env.used_var)


(* Detect and remove unesed declaration *)
let rec merge_env {env=x1;used_var=y1} {env=x2;used_var=y2} =
  let aux _ a b = Some (merge_env a b) in
  {env = MVarMap.union aux x1 x2; used_var = VVarSet.union y1 y2}
and unions = fun l ->
      List.fold l ~init:{env=MVarMap.empty;used_var=VVarSet.empty} ~f:merge_env
and get_fv expr =
  let self = get_fv in
  let return env expression_content = env, {expr with expression_content} in
  match expr.expression_content with
  | E_variable v ->
     return {env=MVarMap.empty ;used_var=VVarSet.singleton v} @@ E_variable v
  | E_literal _ | E_raw_code _ as ec ->
     return {env=MVarMap.empty ;used_var=VVarSet.empty} @@ ec
  | E_constant {cons_name;arguments} ->
     let env_lst,arguments = List.unzip @@ List.map ~f:self arguments in
     return (unions @@ env_lst) @@ E_constant {cons_name;arguments}
  | E_application {lamb; args} ->
     let env_l,lamb = self lamb in
     let env_a,args = self args in
     return (merge_env env_l env_a) @@ E_application {lamb;args}
  | E_type_inst {forall;type_} ->
     let env,forall = self forall in
     return env @@ E_type_inst {forall;type_}
  | E_lambda {binder ; output_type ; result} ->
     let {env;used_var},result = self result in
     return {env;used_var=VVarSet.remove binder.var @@ used_var} @@ E_lambda {binder;output_type;result}
  | E_type_abstraction {type_binder;result} ->
     let env,result = self result in
     return env @@ E_type_abstraction {type_binder;result}
  | E_recursive {fun_name; lambda = {binder; output_type; result};fun_type} ->
     let {env;used_var},result = self result in
     return {env;used_var=VVarSet.remove fun_name @@ VVarSet.remove binder.var @@ used_var} @@
       E_recursive {fun_name; lambda = {binder; output_type; result};fun_type}
  | E_constructor {constructor;element} ->
     let env,element = self element in
     return env @@ E_constructor {constructor;element}
  | E_matching {matchee; cases} ->
     let env,matchee = self matchee in
     let env_c,cases = get_fv_cases cases in
     return (merge_env env env_c) @@ E_matching{matchee;cases}
  | E_record m ->
     let res = Record.map self m in
     let keys,env_exp = List.unzip @@ Record.LMap.to_kv_list res in
     let env,exp = List.unzip env_exp in
     let m = Record.of_list @@ List.zip_exn keys exp in
     return (unions env) @@ E_record m
  | E_update {record;path;update} ->
     let env_r,record = self record in
     let env_u,update = self update in
     return (merge_env env_r env_u) @@ E_update {record;path;update}
  | E_accessor {record;path} ->
     let env, record = self record in
     return env @@ E_accessor {record;path}
  | E_let_in { let_binder ; rhs ; let_result ; attr} ->
     let env,let_result = (self let_result) in
     let env = {env with used_var=VVarSet.remove let_binder.var env.used_var} in
     let env', rhs = self rhs in
     return (merge_env env env') @@ E_let_in {let_binder; rhs; let_result; attr}
  | E_mod_in { module_binder; rhs ; let_result } ->
     let env,let_result = (self let_result) in
     (match MVarMap.find_opt module_binder env.env with
        Some (env') ->
         let env = {env with env = MVarMap.remove module_binder env.env} in
         let env',rhs = get_fv_module_expr env' rhs in
         return (merge_env env env') @@ E_mod_in {module_binder; rhs; let_result}
      | None ->
         env,let_result
     )
  | E_module_accessor { module_path ; element } ->
    let init = {env=MVarMap.empty ;used_var=VVarSet.singleton element} in
    let env = List.fold_right module_path ~f:(fun module_name env -> {env=MVarMap.singleton module_name env;used_var=VVarSet.empty}) ~init in
    return env @@ E_module_accessor {module_path;element}
  | E_assign { binder; expression } ->
     let env, expression = self expression in
     return env @@ E_assign { binder; expression}
and get_fv_cases : matching_expr -> env * matching_expr = fun m ->
  match m with
  | Match_variant {cases;tv} ->
     let aux {constructor; pattern ; body} =
       let env,body= get_fv body in
       {env with used_var=VVarSet.remove pattern @@ env.used_var},{constructor;pattern;body} in
     let envs,cases = List.unzip @@  List.map ~f:aux cases in
     (unions envs), Match_variant {cases;tv}
  | Match_record {fields; body; tv} ->
     let pattern = Record.LMap.values fields |> List.map ~f:(fun b -> b.Binder.var) in
     let env,body = get_fv body in
     {env with used_var=List.fold_right pattern ~f:VVarSet.remove ~init:env.used_var}, Match_record {fields;body;tv}

and get_fv_module (env:env) acc = function
  | [] -> env, acc
  | ({Location.wrap_content = D_value {binder; expr;attr}; _} as hd) :: tl ->
    let binder' = binder in
    if VVarSet.mem binder'.var env.used_var then
      let env = {env with used_var = VVarSet.remove binder'.var env.used_var} in
      let env',expr = get_fv expr in
      let env = merge_env env @@ env' in
      get_fv_module env ({hd with wrap_content = D_value {binder;expr;attr}} :: acc) tl
    else
      get_fv_module env acc tl
  | ({Location.wrap_content = D_module {module_binder; module_;module_attr}; _} as hd) :: tl -> (
    match MVarMap.find_opt module_binder env.env with
    | Some (env') ->
      let new_env,module_ = get_fv_module_expr env' module_ in
      let env = {env with env = MVarMap.remove module_binder env.env} in
      let env = merge_env env new_env in
      get_fv_module env ({hd with wrap_content=D_module{module_binder;module_;module_attr}} :: acc) tl
    | None ->
      get_fv_module env acc tl
  )
  | hd :: tl ->
    get_fv_module env (hd :: acc) tl

and get_fv_module_expr env x =
  match x.wrap_content with
  | M_struct prg -> (
    let new_env,prg = get_fv_module env [] @@ List.rev prg in
    new_env, { x with wrap_content = M_struct prg }
  )
  | M_module_path path -> (
    let rec push_env (name,name_lst) toto =
      match name_lst with
        [] -> {env=MVarMap.singleton name toto;used_var=VVarSet.empty}
      | hd::tl -> {env=MVarMap.singleton name @@ push_env (hd,tl) toto;used_var=VVarSet.empty}
    in
    let new_env = push_env path env in
    new_env, { x with wrap_content = M_module_path path }
  )
  | M_variable v ->
    let new_env = { env = MVarMap.singleton v env ; used_var = VVarSet.empty } in
    new_env, x


and get_fv_program (env:env) acc : program -> _ * program = function
  | [] -> env, acc
  | ({Location.wrap_content = D_value {binder; expr;attr}; _} as hd) :: tl ->
    let binder' = binder in
    if VVarSet.mem binder'.var env.used_var then
      let env = {env with used_var = VVarSet.remove binder'.var env.used_var} in
      let env',expr = get_fv expr in
      let env = merge_env env @@ env' in
      get_fv_program env ({hd with wrap_content = D_value {binder;expr;attr}} :: acc) tl
    else
      get_fv_program env acc tl
  | ({Location.wrap_content = D_module {module_binder; module_;module_attr}; _} as hd) :: tl -> (
    match MVarMap.find_opt module_binder env.env with
    | Some (env') ->
      let new_env,module_ = get_fv_module_expr env' module_ in
      let env = {env with env = MVarMap.remove module_binder env.env} in
      let env = merge_env env new_env in
      get_fv_program env ({hd with wrap_content=D_module{module_binder;module_;module_attr}} :: acc) tl
    | None ->
      get_fv_program env acc tl
  )
  | hd :: tl ->
    get_fv_program env (hd :: acc) tl

let remove_unused ~raise : contract_pass_data -> program -> program = fun contract_pass_data prg ->
  (* Process declaration in reverse order *)
  let prg_decls = List.rev prg in
  let aux = function
      {Location.wrap_content = D_value {binder;_}; _} -> not (Value_var.equal binder.var contract_pass_data.main_name)
    | _ -> true in
  (* Remove the definition after the main entry_point (can't be relevant), mostly remove the test *)
  let _, prg_decls = List.split_while prg_decls ~f:aux in
  let main_decl, prg_decls = trace_option ~raise (Errors.corner_case "Entrypoint not found") @@ Simple_utils.List.uncons prg_decls in
  let main_dc = trace_option ~raise (Errors.corner_case "Entrypoint not found") @@ match main_decl with
      {Location.wrap_content = D_value dc; _} -> Some dc
    | _ -> None in
  let env,main_expr = get_fv main_dc.expr in
  let main_dc = {main_dc with expr = main_expr} in
  let main_decl = {main_decl with wrap_content = D_value main_dc} in
  let _,module_ = get_fv_program env [main_decl] prg_decls in
  module_

let remove_unused_for_views ~raise ~(view_names:Value_var.t list ) : program -> program = fun prg ->
  let view_names = List.rev view_names in
  let is_view_name var = List.mem view_names var ~equal:Value_var.equal in
  (* Process declaration in reverse order *)
  let prg_decls = List.rev prg in
  let pred = fun _ -> function
      {Location.wrap_content = D_value {binder;_}; _} -> (is_view_name binder.var)
    | _ -> false in
  let idx,_ = trace_option ~raise (Errors.corner_case "View not found") @@ List.findi prg_decls ~f:pred in
  (* Remove the definition after the last view (can't be relevant), mostly remove the test *)
  let _,prg_decls = List.split_n prg_decls idx in
  let view_decls = List.rev @@ List.filter_map prg_decls
    ~f:(fun decl ->
      match decl with
        {Location.wrap_content = D_value dc; _} when is_view_name dc.binder.var -> Some dc
      | _ -> None)
  in
  let env = List.fold view_decls ~init:empty_env ~f:(fun env view_decl ->
    let env',_ = get_fv view_decl.expr in
    merge_env env env'
  ) in
  let view_decls = List.map view_decls ~f:(fun decl -> Location.wrap (D_value decl)) in
  let _, prg_decls = trace_option ~raise (Errors.corner_case "View not found") @@ Simple_utils.List.uncons prg_decls in
  let _,module_ = get_fv_program env view_decls prg_decls in
  module_

let remove_unused_expression : expression -> program -> expression * program = fun expr prg ->
  (* Process declaration in reverse order *)
  let prg_decls = List.rev prg in
  let env,main_expr = get_fv expr in
  let _,module_ = get_fv_program env [] prg_decls in
  main_expr, module_
