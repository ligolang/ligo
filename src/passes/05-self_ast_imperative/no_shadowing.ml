open Ligo_prim
open Ast_imperative
open Errors
open Simple_utils.Trace

(* Prevents shadowing in the same scope. Needed for JsLIGO. *)

let rec check_block_scope ~raise vars types mods e =
  match e.expression_content with
  | E_let_in { let_binder; rhs; let_result; _ } ->
    let binders = List.map ~f:Binder.get_var (Pattern.binders let_binder) in
    let () =
      List.iter binders ~f:(fun var ->
          if List.mem ~equal:Value_var.equal vars var
          then raise.error @@ no_shadowing e.location)
    in
    check_block_scope ~raise [] [] [] rhs;
    check_block_scope ~raise (binders @ vars) types mods let_result
  | E_type_in { type_binder; let_result; _ } ->
    if List.mem ~equal:Type_var.equal types type_binder
    then raise.error @@ no_shadowing e.location
    else check_block_scope ~raise vars (type_binder :: types) mods let_result
  | E_mod_in { module_binder; let_result; _ } ->
    let mod_ = module_binder in
    if List.mem ~equal:Module_var.equal mods mod_
    then raise.error @@ no_shadowing e.location
    else check_block_scope ~raise vars types (mod_ :: mods) let_result
  | _ -> ()


let peephole_expression ~raise : expression -> expression =
 fun e ->
  check_block_scope ~raise [] [] [] e;
  e


let peephole_program ~raise : program -> program =
 fun m ->
  let rec aux vars types mods = function
    | Location.{ wrap_content = D_value t; location } :: remaining ->
      let var = Binder.get_var t.binder in
      let () =
        if List.mem ~equal:Value_var.equal vars var
        then raise.error (no_shadowing location)
      in
      aux (var :: vars) types mods remaining
    | Location.{ wrap_content = D_irrefutable_match t; location } :: remaining ->
      let pattern_vars = List.map ~f:Binder.get_var (Pattern.binders t.pattern) in
      let () =
        List.iter pattern_vars ~f:(fun var ->
            if List.mem ~equal:Value_var.equal vars var
            then raise.error (no_shadowing location))
      in
      aux (pattern_vars @ vars) types mods remaining
    | { wrap_content = D_type t; location } :: remaining ->
      if List.mem ~equal:Type_var.equal types t.type_binder
      then raise.error @@ no_shadowing location
      else aux vars (t.type_binder :: types) mods remaining
    | { wrap_content = D_module t; location } :: remaining ->
      let mod_ = t.module_binder in
      if List.mem ~equal:Module_var.equal mods mod_
      then raise.error @@ no_shadowing location
      else aux vars types (mod_ :: mods) remaining
    | [] -> ()
  in
  aux [] [] [] m;
  m
