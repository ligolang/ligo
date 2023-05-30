open Types
open Recursion_schemes.Catamorphism
module VarSet = Caml.Set.Make (Variable)

let empty = VarSet.empty
let singleton = VarSet.singleton
let union = VarSet.union
let diff = VarSet.diff
let add = VarSet.add
let remove = VarSet.remove
let of_list = VarSet.of_list
let unions : VarSet.t list -> VarSet.t = fun l -> List.fold l ~init:empty ~f:union

type fv = VarSet.t
type bound = VarSet.t
type sequence = fv * bound

let fv_of_sequence (lst : sequence list) : fv =
  snd
  @@ List.fold
       ~f:(fun (bound', fv') (bound, fv) ->
         union bound' bound, union fv' (diff fv bound'))
       ~init:(empty, empty)
       lst


let fv_folder =
  let ignore_bound : fv -> sequence -> fv =
   fun acc_fv (_bound, fv) -> unions [ acc_fv; fv ]
  in
  let propagate_bound (acc_bound, acc_fv) (bound, fv) =
    union acc_bound bound, union acc_fv fv
  in
  let merge_bound (acc_bound, acc_fv) instr_fv = acc_bound, union acc_fv instr_fv in
  let fv_ty x () = x in
  let pattern : _ pattern_ -> fv =
   fun p ->
    match Location.unwrap p with
    | P_var x -> singleton x
    | _ -> fold_pattern_ union fv_ty empty p
  in
  let expr : _ expression_ -> fv =
   fun expr ->
    match Location.unwrap expr with
    | E_variable x -> singleton x
    | E_simple_let_in { binder; rhs; let_result } ->
      let fv_body = diff let_result binder in
      union rhs fv_body
    | E_let_in { is_rec; lhs = fun_name, params; rhs; body; _ }
    | E_let_mut_in { is_rec; lhs = fun_name, params; rhs; body; _ } ->
      let bound =
        let b = if is_rec then fun_name :: params else params in
        List.fold ~f:union ~init:empty b
      in
      let fv_body = diff body bound in
      union rhs fv_body
    | E_for { index; init; bound; step; block } ->
      let used = unions [ init; bound; Option.value ~default:empty step; block ] in
      remove index used
    | E_for_in (ForMap { binding = v1, v2; collection; block }) ->
      let used = unions [ collection; block ] in
      diff used (of_list [ v1; v2 ])
    | E_for_in (ForSetOrList { var; for_kind = _; collection; block }) ->
      let used = unions [ collection; block ] in
      remove var used
    | E_for_in (ForAny { pattern; collection; block }) ->
      let used = unions [ collection; block ] in
      diff used pattern
    | E_block_poly_fun { parameters; body; _ } ->
      let bound = unions (List.map ~f:(fun x -> x.pattern) parameters) in
      diff body bound
    | E_poly_fun { parameters; body; _ } ->
      let bound = unions (List.map ~f:(fun x -> x.pattern) parameters) in
      diff body bound
    | E_poly_recursive { fun_name; lambda = { parameters; body; _ }; _ } ->
      let fv_lamb =
        let bound = unions (List.map ~f:(fun x -> x.pattern) parameters) in
        diff body bound
      in
      remove fun_name fv_lamb
    | E_lambda { binder = { binder; _ }; result; _ } ->
      let bound = Ligo_prim.Binder.get_var binder in
      remove bound result
    | E_recursive { fun_name; lambda = { binder = { binder; _ }; result; _ }; _ } ->
      let fv_lamb =
        let bound = Ligo_prim.Binder.get_var binder in
        remove bound result
      in
      remove fun_name fv_lamb
    | E_match { expr; cases } ->
      let f Case.{ pattern; rhs } = diff rhs pattern in
      union expr (unions @@ List.map (List.Ne.to_list cases) ~f)
    | _ -> fold_expr_ union fv_ty union union union empty expr
  in
  let declaration : _ declaration_ -> sequence =
   fun d ->
    match Location.unwrap d with
    | D_let { is_rec; pattern = fun_name, params; let_rhs; _ } ->
      let bound = if is_rec then unions (fun_name :: params) else unions params in
      bound, diff let_rhs bound
    | D_var { pattern; let_rhs; _ } | D_const { pattern; let_rhs; _ } ->
      let bound = pattern in
      bound, diff let_rhs bound
    | D_multi_var lst | D_multi_const lst ->
      let lst = List.Ne.to_list lst in
      let b_fv_lst =
        List.map lst ~f:(fun { pattern; let_rhs; _ } -> pattern, diff let_rhs pattern)
      in
      let b, fv = List.unzip b_fv_lst in
      unions b, unions fv
    | D_fun { is_rec; fun_name; parameters; return; _ } ->
      let bound =
        let params = unions @@ List.map parameters ~f:(fun x -> x.pattern) in
        if is_rec then add fun_name params else params
      in
      bound, diff return bound
    | D_irrefutable_match { pattern; expr } -> pattern, diff expr pattern
    | _ -> empty, fold_declaration_ ignore_bound union fv_ty union union union empty d
  in
  let block : _ block_ -> fv =
   fun lst -> fv_of_sequence (List.Ne.to_list (Location.unwrap lst))
  in
  let mod_expr = fold_mod_expr_ union union empty in
  let program : _ program_ -> fv = fv_of_sequence in
  let sig_expr : _ sig_expr_ -> fv = fun _ -> empty in
  let sig_entry : _ sig_entry_ -> fv = fun _ -> empty in
  let instruction = fold_instruction_ union union union ignore_bound union empty in
  let program_entry : _ program_entry_ -> sequence =
    fold_program_entry_ propagate_bound propagate_bound merge_bound (empty, empty)
  in
  let statement : _ statement_ -> sequence =
    fold_statement_ propagate_bound merge_bound propagate_bound (empty, empty)
  in
  { expr
  ; ty_expr = Fun.const ()
  ; pattern
  ; statement
  ; block
  ; mod_expr
  ; instruction
  ; declaration
  ; program_entry
  ; program
  ; sig_expr
  ; sig_entry
  }


let fv_expr = cata_expr ~f:fv_folder
