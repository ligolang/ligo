open Ligo_prim
open Ast_aggregated

let get_fv_e = Helpers.Free_variables.expression_only_var

module VVarSet = Set

type env = (Value_var.t, Value_var.comparator_witness) VVarSet.t

let empty_env = VVarSet.empty (module Value_var)

let is_pattern_unit (p : type_expression Pattern.t) =
  match Location.unwrap p with
  | P_unit -> true
  | _ -> false


let rec merge_env x1 x2 = VVarSet.union x1 x2
and unions l = List.fold l ~init:empty_env ~f:merge_env

and remove_unused_program (env : env) acc : context -> _ * context = function
  | [] -> env, acc
  | ({ Location.wrap_content = D_irrefutable_match { pattern; expr; attr }; _ } as hd)
    :: tl ->
    let binders =
      List.filter (Pattern.binders pattern) ~f:(fun binder' ->
          VVarSet.mem env (Binder.get_var binder'))
    in
    if List.is_empty binders && not (is_pattern_unit pattern)
    then remove_unused_program env acc tl
    else (
      let env =
        List.fold binders ~init:env ~f:(fun env binder' ->
            VVarSet.remove env (Binder.get_var binder'))
      in
      let env' = VVarSet.of_list (module Value_var) (get_fv_e expr) in
      let env = merge_env env env' in
      remove_unused_program
        env
        ({ hd with wrap_content = D_irrefutable_match { pattern; expr; attr } } :: acc)
        tl)
  | ({ Location.wrap_content = D_value { binder; expr; attr }; _ } as hd) :: tl ->
    let binder' = binder in
    if VVarSet.mem env (Binder.get_var binder')
    then (
      let env = VVarSet.remove env (Binder.get_var binder') in
      let env' = VVarSet.of_list (module Value_var) (get_fv_e expr) in
      let env = merge_env env env' in
      remove_unused_program
        env
        ({ hd with wrap_content = D_value { binder; expr; attr } } :: acc)
        tl)
    else remove_unused_program env acc tl


let remove_unused : program -> program =
 fun (ctxt, exp) ->
  let env = VVarSet.of_list (module Value_var) (get_fv_e exp) in
  let _env, ctxt = remove_unused_program env [] (List.rev ctxt) in
  ctxt, exp
