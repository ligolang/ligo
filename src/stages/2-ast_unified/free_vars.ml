open Core
open Types
open Recursion_schemes.Catamorphism
module VarSet = Set.Make (Variable)

let unions : VarSet.t list -> VarSet.t =
 fun l -> List.fold l ~init:VarSet.empty ~f:Set.union


type fv = VarSet.t
type bound = VarSet.t
type sequence = fv * bound

let fv_of_sequence (lst : sequence list) : fv =
  snd
  @@ List.fold
       ~f:(fun (bound', fv') (bound, fv) ->
         Set.union bound' bound, Set.union fv' (Set.diff fv bound'))
       ~init:VarSet.(empty, empty)
       lst


let fv_folder =
  let ignore_bound : fv -> sequence -> fv =
   fun acc_fv (_bound, fv) -> unions [ acc_fv; fv ]
  in
  let propagate_bound (acc_bound, acc_fv) (bound, fv) =
    Set.union acc_bound bound, Set.union acc_fv fv
  in
  let merge_bound (acc_bound, acc_fv) instr_fv = acc_bound, Set.union acc_fv instr_fv in
  let fv_ty x () = x in
  let pattern : _ pattern_ -> fv =
   fun p ->
    match Location.unwrap p with
    | P_var x -> VarSet.singleton x
    | _ -> fold_pattern_ Set.union fv_ty VarSet.empty p
  in
  let expr : _ expression_ -> fv =
   fun expr ->
    match Location.unwrap expr with
    | E_variable x -> VarSet.singleton x
    | E_simple_let_in { binder; rhs; let_result } ->
      let fv_body = Set.diff let_result binder in
      Set.union rhs fv_body
    | E_let_in { is_rec; lhs = fun_name :: params; rhs; body; _ }
    | E_let_mut_in { is_rec; lhs = fun_name :: params; rhs; body; _ } ->
      let bound =
        let b = if is_rec then fun_name :: params else params in
        List.fold ~f:Set.union ~init:VarSet.empty b
      in
      let fv_body = Set.diff body bound in
      Set.union rhs fv_body
    | E_for { index; init; bound; step; block } ->
      let used = unions [ init; bound; Option.value ~default:VarSet.empty step; block ] in
      Set.remove used index
    | E_for_in (ForMap { binding = v1, v2; collection; block }) ->
      let used = unions [ collection; block ] in
      Set.diff used (VarSet.of_list [ v1; v2 ])
    | E_for_in (ForSetOrList { var; for_kind = _; collection; block }) ->
      let used = unions [ collection; block ] in
      Set.remove used var
    | E_for_in (ForAny { pattern; collection; block }) ->
      let used = unions [ collection; block ] in
      Set.diff used pattern
    | E_block_poly_fun { parameters; body; _ } ->
      let bound = unions (List.map ~f:(fun x -> x.pattern) parameters) in
      Set.diff body bound
    | E_poly_fun { parameters; body; _ } ->
      let bound = unions (List.map ~f:(fun x -> x.pattern) parameters) in
      Set.diff body bound
    | E_poly_recursive { fun_name; lambda = { parameters; body; _ }; _ } ->
      let fv_lamb =
        let bound = unions (List.map ~f:(fun x -> x.pattern) parameters) in
        Set.diff body bound
      in
      Set.remove fv_lamb fun_name
    | E_lambda { binder = { binder; _ }; result; _ } ->
      let bound = Ligo_prim.Binder.get_var binder in
      Set.remove result bound
    | E_recursive { fun_name; lambda = { binder = { binder; _ }; result; _ }; _ } ->
      let fv_lamb =
        let bound = Ligo_prim.Binder.get_var binder in
        Set.remove result bound
      in
      Set.remove fv_lamb fun_name
    | E_match { expr; cases } ->
      let f Case.{ pattern; rhs } =
        Set.diff rhs (Option.value ~default:VarSet.empty pattern)
      in
      Set.union expr (unions @@ List.map (Nonempty_list.to_list cases) ~f)
    | _ -> fold_expr_ Set.union fv_ty Set.union Set.union Set.union VarSet.empty expr
  in
  let declaration : _ declaration_ -> sequence =
   fun d ->
    match Location.unwrap d with
    | D_let { is_rec; pattern = fun_name :: params; let_rhs; _ } ->
      let bound = if is_rec then unions (fun_name :: params) else unions params in
      bound, Set.diff let_rhs bound
    | D_var { pattern; let_rhs; _ } | D_const { pattern; let_rhs; _ } ->
      let bound = pattern in
      bound, Set.diff let_rhs bound
    | D_multi_var lst | D_multi_const lst ->
      let lst = Nonempty_list.to_list lst in
      let b_fv_lst =
        List.map lst ~f:(fun { pattern; let_rhs; _ } -> pattern, Set.diff let_rhs pattern)
      in
      let b, fv = List.unzip b_fv_lst in
      unions b, unions fv
    | D_fun { is_rec; fun_name; parameters; return; _ } ->
      let bound =
        let params = unions @@ List.map parameters ~f:(fun x -> x.pattern) in
        if is_rec then Set.add params fun_name else params
      in
      bound, Set.diff return bound
    | D_irrefutable_match { pattern; expr } -> pattern, Set.diff expr pattern
    | _ ->
      ( VarSet.empty
      , fold_declaration_
          ignore_bound
          Set.union
          fv_ty
          Set.union
          Set.union
          Set.union
          VarSet.empty
          d )
  in
  let block : _ block_ -> fv =
   fun lst -> fv_of_sequence (Nonempty_list.to_list (Location.unwrap lst))
  in
  let mod_expr = fold_mod_expr_ Set.union Set.union VarSet.empty in
  let program : _ program_ -> fv = fv_of_sequence in
  let sig_expr : _ sig_expr_ -> fv = fun _ -> VarSet.empty in
  let sig_entry : _ sig_entry_ -> fv = fun _ -> VarSet.empty in
  let instruction =
    fold_instruction_ Set.union Set.union Set.union ignore_bound Set.union VarSet.empty
  in
  let program_entry : _ program_entry_ -> sequence =
    fold_program_entry_ propagate_bound propagate_bound merge_bound VarSet.(empty, empty)
  in
  let statement : _ statement_ -> sequence =
    fold_statement_ propagate_bound merge_bound propagate_bound VarSet.(empty, empty)
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
