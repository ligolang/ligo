open Types
open Recursion_schemes.Catamorphism

(* accounts for branches, e.g. 
   `let x = 1 in let y = 2 in x + y` |-> [[ x ; y ]]
   and
   `match x with One x -> let x = x+1 in x | Two z -> z` |-> [[x ; x] ; [ z ]]
   *)
type bound = Variable.t list list

let empty = [ [] ]
let singleton x = [ x ]

let union a b =
  match List.map2 a b ~f:List.append with
  | Types.List.Or_unequal_lengths.Ok x -> x
  | Types.List.Or_unequal_lengths.Unequal_lengths -> assert false


let add (el : Variable.t) (x : bound) : bound = List.map x ~f:(fun lst -> el :: lst)

let adds (els : Variable.t list) (x : bound) =
  List.fold els ~init:x ~f:(fun acc b -> add b acc)


let fv_folder =
  let expr : (bound, ty_expr, pattern, bound, bound) expression_ -> bound =
   fun expr ->
    match Location.unwrap expr with
    | E_simple_let_in { binder; rhs = _; let_result = body }
    | E_let_in { lhs = binder, _; rhs = _; body; _ }
    | E_let_mut_in { lhs = binder, _; rhs = _; body; _ } ->
      adds (Combinators.get_pattern_binders binder) body
    | E_for { index; init = _; bound = _; step = _; block } -> add index block
    | E_for_in (ForMap { binding = v1, v2; collection = _; block }) ->
      let b = [ v1; v2 ] in
      adds b block
    | E_for_in (ForSetOrList { var; for_kind = _; collection = _; block }) ->
      add var block
    | E_for_in (ForAny { pattern; collection = _; block }) ->
      adds (Combinators.get_pattern_binders pattern) block
    | E_block_poly_fun { parameters; body; _ } | E_poly_fun { parameters; body; _ } ->
      let bound =
        List.concat
        @@ List.map ~f:(fun x -> Combinators.get_pattern_binders x.pattern) parameters
      in
      adds bound body
    | E_poly_recursive { fun_name; lambda = { parameters; body; _ }; _ } ->
      let bound =
        List.concat
        @@ List.map ~f:(fun x -> Combinators.get_pattern_binders x.pattern) parameters
      in
      adds (fun_name :: bound) body
    | E_lambda { binder = { binder; _ }; result; _ } ->
      let bound = Ligo_prim.Binder.get_var binder in
      add bound result
    | E_recursive { fun_name; lambda = { binder = { binder; _ }; result; _ }; _ } ->
      let b_lamb =
        let bound = Ligo_prim.Binder.get_var binder in
        add bound result
      in
      add fun_name b_lamb
    | E_match { expr = _; cases } ->
      let f Case.{ pattern; rhs } =
        match pattern with
        | None -> []
        | Some pattern -> adds (Combinators.get_pattern_binders pattern) rhs
      in
      let bounds = List.map (List.Ne.to_list cases) ~f in
      List.concat bounds
    | _ -> empty
  in
  let declaration : (bound, bound, ty_expr, pattern, bound, bound) declaration_ -> bound =
   fun d ->
    match Location.unwrap d with
    | D_attr (_, d) -> d
    | D_let { is_rec; pattern = fun_name, params; let_rhs = _; _ } ->
      let bound =
        let binders = if is_rec then fun_name :: params else params in
        List.concat @@ List.map ~f:(fun x -> Combinators.get_pattern_binders x) binders
      in
      singleton bound
    | D_var { pattern; let_rhs = _; _ } | D_const { pattern; let_rhs = _; _ } ->
      let bound = Combinators.get_pattern_binders pattern in
      singleton bound
    | D_multi_var lst | D_multi_const lst ->
      let lst = List.Ne.to_list lst in
      let bounds =
        List.map lst ~f:(fun { pattern; _ } -> Combinators.get_pattern_binders pattern)
      in
      singleton (List.concat bounds)
    | D_fun { is_rec; fun_name; parameters; return = _; _ } ->
      let bound =
        let params =
          List.concat
          @@ List.map parameters ~f:(fun x -> Combinators.get_pattern_binders x.pattern)
        in
        if is_rec then fun_name :: params else params
      in
      singleton bound
    | D_irrefutable_match { pattern; expr = _ } ->
      let binders = Combinators.get_pattern_binders pattern in
      singleton binders
    | D_signature { name = _; sig_expr = _; extends = _ } -> empty
    | _ -> empty
  in
  let block : _ block_ -> bound = fold_block_ union union empty in
  let mod_expr = fold_mod_expr_ union union empty in
  let program : _ program_ -> bound = fold_program_ union union empty in
  let sig_expr : _ sig_expr_ -> bound = fold_sig_expr_ union union union empty in
  let sig_entry : _ sig_entry_ -> bound =
   fun si ->
    match Location.unwrap si with
    | S_value (v, _, _) -> singleton [ v ]
    | S_attr (_, si) -> si
    | _ -> empty
  in
  let instruction : _ instruction_ -> bound = fun _ -> empty in
  let program_entry : _ program_entry_ -> bound =
    fold_program_entry_ union union union empty
  in
  let statement : _ statement_ -> bound = fold_statement_ union union union empty in
  { expr
  ; ty_expr =
      (fun x -> Combinators.make_t ~loc:(Location.get_location x) (Location.unwrap x))
  ; pattern =
      (fun p -> Combinators.make_p ~loc:(Location.get_location p) (Location.unwrap p))
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


let bound_expr = cata_expr ~f:fv_folder
let bound_program = cata_program ~f:fv_folder
let bound_block = cata_block ~f:fv_folder
let bound_sig_expr = cata_sig_expr ~f:fv_folder
