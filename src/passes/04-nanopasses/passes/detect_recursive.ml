open Core
open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Ne_list = Simple_utils.Ne_list
module Location = Simple_utils.Location
include Flag.No_arg ()

(* automatically detect recursive declaration and expression *)

let name = __MODULE__

let rec dig ~f e =
  match get_e_type_abstraction e with
  | Some { type_binder; result } ->
    e_type_abstraction ~loc:(get_e_loc e) { type_binder; result = dig ~f result }
  | None ->
    (match get_e_annot e with
    | Some (expr, ty) -> e_annot ~loc:(get_e_loc e) (dig ~f expr, ty)
    | None -> f e)


let recursify ~raise fun_name let_rhs =
  dig
    ~f:(fun let_rhs ->
      match get_e_poly_fun let_rhs with
      | Some { type_params; parameters; ret_type; body } ->
        let is_rec = Set.mem (Free_vars.fv_expr let_rhs) fun_name in
        if is_rec
        then (
          let fun_type : ty_expr Nonempty_list.t =
            let param_type_opt =
              List.map ~f:(fun x -> Option.map ~f:fst @@ get_p_typed x.pattern) parameters
            in
            let aux = Nonempty_list.(ret_type :: List.rev param_type_opt) in
            let fun_type_opt = Ne_list.collect (Nonempty_list.reverse aux) in
            Trace.trace_option ~raise (recursive_no_annot let_rhs) fun_type_opt
          in
          e_poly_recursive
            ~loc:(get_e_loc let_rhs)
            { fun_name
            ; fun_type = Extracted fun_type
            ; lambda = { type_params; parameters; ret_type; body }
            })
        else let_rhs
      | None -> let_rhs)
    let_rhs


let mono_binder_opt pattern =
  match get_pattern_binders pattern with
  | [ fun_name ] -> Some fun_name
  | _ -> None


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_simple_let_in ({ binder; rhs; _ } as letin) ->
      let f_binder_opt = mono_binder_opt binder in
      let rhs =
        Option.value_map f_binder_opt ~default:rhs ~f:(fun fun_name ->
            recursify ~raise fun_name rhs)
      in
      e_simple_let_in ~loc { letin with rhs }
    | E_let_in
        ({ is_rec = false; type_params = None; lhs = [ pattern ]; rhs; _ } as letin) ->
      let f_binder_opt = mono_binder_opt pattern in
      let rhs =
        Option.value_map f_binder_opt ~default:rhs ~f:(fun fun_name ->
            recursify ~raise fun_name rhs)
      in
      e_let_in ~loc { letin with rhs }
    | E_let_mut_in
        ({ is_rec = false; type_params = None; lhs = [ pattern ]; rhs; _ } as letin) ->
      let f_binder_opt = mono_binder_opt pattern in
      let rhs =
        Option.value_map f_binder_opt ~default:rhs ~f:(fun fun_name ->
            recursify ~raise fun_name rhs)
      in
      e_let_mut_in ~loc { letin with rhs }
    | _ -> make_e ~loc e.wrap_content
  in
  let declaration : _ declaration_ -> declaration =
   fun d ->
    let loc = Location.get_location d in
    let ignore = make_d ~loc d.wrap_content in
    match Location.unwrap d with
    | D_const { type_params = None; pattern; rhs_type; let_rhs } ->
      let f_binder_opt = mono_binder_opt pattern in
      let let_rhs =
        Option.value_map f_binder_opt ~default:let_rhs ~f:(fun fun_name ->
            recursify ~raise fun_name let_rhs)
      in
      d_const ~loc { type_params = None; pattern; rhs_type; let_rhs }
    | D_var { type_params = None; pattern; rhs_type; let_rhs } ->
      let f_binder_opt = mono_binder_opt pattern in
      let let_rhs =
        Option.value_map f_binder_opt ~default:let_rhs ~f:(fun fun_name ->
            recursify ~raise fun_name let_rhs)
      in
      d_var ~loc { type_params = None; pattern; rhs_type; let_rhs }
    | _ -> ignore
  in
  Fold { idle_fold with expr; declaration }


let reduction ~raise:_ = Iter.defaults
let decompile ~raise:_ = Nothing (* for now .. *)
