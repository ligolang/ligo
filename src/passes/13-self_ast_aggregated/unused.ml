open Ligo_prim
open Ast_aggregated

(* We go through the Typed AST and maintain a map
   from variables to a boolean indicating if the variable
   was used.
   To deal with name capture, a list of known unused variables is
   also maintained.
*)

module V = Value_var
module M = Simple_utils.Map.Make (V)

(* A map recording if a variable is being used * a list of unused variables. *)
type defuse = bool M.t * V.t list

let defuse_union (x, a) (y, b) = M.union (fun _ x y -> Some (x || y)) x y, a @ b
let defuse_neutral = M.empty, []
let defuse_unions defuse = List.fold_left ~f:defuse_union ~init:(defuse, [])

let replace_opt k x m =
  Option.value_map ~default:(M.remove k m) ~f:(fun x -> M.add k x m) x


let add_if_not_generated ?forbidden x xs b =
  let sv = Format.asprintf "%a" V.pp x in
  if (not b)
     && (not (V.is_generated x))
     && Char.( <> ) (String.get sv 0) '_'
     && Option.value_map ~default:true ~f:(String.( <> ) sv) forbidden
  then x :: xs
  else xs


let remove_defined_var_after defuse binder f expr =
  let old_binder = M.find_opt binder defuse in
  let defuse, unused = f (M.add binder false defuse) expr in
  let unused = add_if_not_generated binder unused (M.find binder defuse) in
  replace_opt binder old_binder defuse, unused


(* Return a def-use graph + a list of unused variables *)
let rec defuse_of_expr defuse expr : defuse =
  match expr.expression_content with
  | E_literal _ -> defuse, []
  | E_constructor { element; _ } -> defuse_of_expr defuse element
  | E_constant { arguments; _ } ->
    defuse_unions defuse (List.map ~f:(defuse_of_expr defuse) arguments)
  | E_variable v -> M.add v true defuse, []
  | E_application { lamb; args } ->
    defuse_union (defuse_of_expr defuse lamb) (defuse_of_expr defuse args)
  | E_lambda l -> defuse_of_lambda defuse l
  | E_recursive { lambda; _ } -> defuse_of_lambda defuse lambda
  | E_type_abstraction { result; _ } -> defuse_of_expr defuse result
  | E_raw_code { code; _ } -> defuse_of_expr defuse code
  | E_let_mut_in { let_binder; rhs; let_result; _ }
  | E_let_in { let_binder; rhs; let_result; _ } ->
    let cases = [ let_binder, let_result ] in
    defuse_union (defuse_of_expr defuse rhs) (defuse_of_cases defuse cases)
  | E_matching { matchee; cases } ->
    let cases = List.map cases ~f:(fun { pattern; body } -> pattern, body) in
    defuse_union (defuse_of_expr defuse matchee) (defuse_of_cases defuse cases)
  | E_record re ->
    Record.fold
      ~f:(fun acc x -> defuse_union (defuse_of_expr defuse x) acc)
      ~init:defuse_neutral
      re
  | E_accessor { struct_; _ } -> defuse_of_expr defuse struct_
  | E_update { struct_; update; _ } ->
    defuse_union (defuse_of_expr defuse struct_) (defuse_of_expr defuse update)
  | E_type_inst { forall; _ } -> defuse_of_expr defuse forall
  | E_assign { binder; expression } ->
    defuse_union
      (M.add (Binder.get_var binder) true M.empty, [])
      (defuse_of_expr defuse expression)
  | E_deref var -> M.add var true defuse, []
  | E_for { binder; start; final; incr; f_body } ->
    defuse_unions
      defuse
      [ defuse_of_expr defuse start
      ; defuse_of_expr defuse final
      ; defuse_of_expr defuse incr
      ; defuse_of_binder defuse binder (fun defuse -> defuse_of_expr defuse f_body)
      ]
  | E_for_each { fe_binder = binder1, binder2; collection; fe_body; _ } ->
    (* Recover type of binders *)
    let binders = binder1 :: Option.to_list binder2 in
    defuse_unions
      defuse
      [ defuse_of_expr defuse collection
      ; defuse_of_binders defuse binders (fun defuse -> defuse_of_expr defuse fe_body)
      ]
  | E_while { cond; body } ->
    defuse_unions defuse [ defuse_of_expr defuse cond; defuse_of_expr defuse body ]


and defuse_of_lambda defuse { binder; output_type = _; result } =
  remove_defined_var_after defuse (Param.get_var binder) defuse_of_expr result


and defuse_of_binder defuse binder in_ =
  let old_binder = M.find_opt binder defuse in
  let defuse, unused = in_ (M.add binder false defuse) in
  let unused = add_if_not_generated binder unused (M.find binder defuse) in
  replace_opt binder old_binder defuse, unused


and defuse_of_binders defuse binders in_ =
  let map = List.fold_left ~f:(fun m v -> M.add v false m) ~init:defuse binders in
  let binders' = List.map ~f:(fun v -> v, M.find_opt v defuse) binders in
  let defuse, unused = in_ map in
  let unused =
    List.fold_left
      ~f:(fun m v -> add_if_not_generated v m (M.find v defuse))
      ~init:unused
      binders
  in
  let defuse =
    List.fold_left ~f:(fun m (v, v') -> replace_opt v v' m) ~init:defuse binders'
  in
  defuse, unused


and defuse_of_cases defuse cases =
  List.fold_left cases ~init:(defuse, []) ~f:(fun (defuse, unused_) (pattern, body) ->
      let vars = Pattern.binders pattern |> List.rev_map ~f:Binder.get_var in
      let map = List.fold_left ~f:(fun m v -> M.add v false m) ~init:defuse vars in
      let vars' = List.map ~f:(fun v -> v, M.find_opt v defuse) vars in
      let defuse, unused = defuse_of_expr map body in
      let unused =
        List.fold_left
          ~f:(fun m v -> add_if_not_generated v m (M.find v defuse))
          ~init:unused
          vars
      in
      let defuse =
        List.fold_left ~f:(fun m (v, v') -> replace_opt v v' m) ~init:defuse vars'
      in
      defuse, unused_ @ unused)


and defuse_of_declaration defuse (decl : declaration) : defuse =
  match Location.unwrap decl with
  | D_irrefutable_match { expr; _ } | D_value { expr; _ } -> defuse_of_expr defuse expr


let defuse_of_declaration defuse decl =
  let _, unused = defuse_of_declaration defuse decl in
  List.rev unused


let unused_declaration ~raise (decl : declaration) =
  let update_annotations annots = List.iter ~f:raise.Simple_utils.Trace.warning annots in
  let defuse, _ = defuse_neutral in
  let unused = defuse_of_declaration defuse decl in
  let warn_var v =
    `Self_ast_aggregated_warning_unused (V.get_location v, Format.asprintf "%a" V.pp v)
  in
  update_annotations @@ List.map ~f:warn_var unused


let unused_map_program ~raise : program -> program = function
  | p, e ->
    let () = List.iter ~f:(unused_declaration ~raise) p in
    p, e
