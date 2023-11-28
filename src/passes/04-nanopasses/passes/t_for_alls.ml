open Ast_unified
open Ast_unified.Catamorphism
open Pass_type
open Simple_utils.Trace
open Errors

(* In this pass we turn T_ForAlls into many T_ForAll,
   and also error when generics are used in signatures *)
let name = __MODULE__

include Flag.With_arg (struct
  type flag = bool
end)

let compile ~raise =
  let ty_expr : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_for_alls { ty_binders; kind; type_ } } ->
      List.fold_right
        ty_binders
        ~f:(fun ty_binder type_ -> t_for_all ~loc { ty_binder; kind; type_ })
        ~init:type_
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  let sig_entry : (sig_expr, sig_entry, ty_expr) Types.sig_entry_ -> sig_entry =
   fun se ->
    let loc = Location.get_location se in
    match Location.unwrap se with
    | S_type (_, _ :: _, _) -> raise.error (unsupported_parametric_type_in_signature loc)
    | _ -> make_sig_entry se
  in
  Fold { idle_fold with ty_expr; sig_entry }


let reduction ~raise:_ = Iter.defaults

let decompile ~raise:_ =
  let tick =
    let ty_expr ~var ~to_ : ty_expr Types.type_expression_ -> ty_expr =
     fun t ->
      let loc = Location.get_location t in
      match Location.unwrap t with
      | T_var v when Ty_variable.equal var v -> make_t ~loc @@ T_var to_
      | _ -> make_t ~loc @@ Location.unwrap t
    in
    let ty_expr : ty_expr Types.type_expression_ -> ty_expr =
     fun t ->
      let loc = Location.get_location t in
      match Location.unwrap t with
      | T_for_all { ty_binder; kind; type_ }
        when (not (Ty_variable.is_generated ty_binder))
             && not (String.is_prefix ~prefix:"'" (Ty_variable.to_name_exn ty_binder)) ->
        let to_ = Ty_variable.add_prefix "'" ty_binder in
        let fold = { Catamorphism.idle with ty_expr = ty_expr ~var:ty_binder ~to_ } in
        let type_ = cata_ty_expr ~f:fold type_ in
        t_for_all ~loc { ty_binder = to_; kind; type_ }
      | t -> make_t ~loc t
    in
    Fold { idle_fold with ty_expr }
  in
  let transform =
    let ty_expr : ty_expr Types.type_expression_ -> ty_expr =
     fun t ->
      let loc = Location.get_location t in
      match Location.unwrap t with
      | T_for_all { ty_binder; kind; type_ } ->
        t_for_alls ~loc { ty_binders = [ ty_binder ]; kind; type_ }
      | t -> make_t ~loc t
    in
    Fold { idle_fold with ty_expr }
  in
  let merge =
    let rec get_for_alls ~loc ~expectedKind t binders =
      match t with
      | T_for_alls { ty_binders; kind = expectedKind; type_ } ->
        let t = get_t type_ in
        get_for_alls ~loc ~expectedKind t (ty_binders :: binders)
      | _ -> List.concat (List.rev binders), make_t ~loc t
    in
    let ty_expr : ty_expr Types.type_expression_ -> ty_expr =
     fun t ->
      let loc = Location.get_location t in
      match Location.unwrap t with
      | T_for_alls { ty_binders = _; kind; type_ = _ } ->
        let ty_binders, type_ =
          get_for_alls ~loc ~expectedKind:kind (Location.unwrap t) []
        in
        t_for_alls ~loc { ty_binders; kind; type_ }
      | t -> make_t ~loc t
    in
    Fold { idle_fold with ty_expr }
  in
  let always = Seq (transform, merge) in
  if get_flag () then Seq (tick, always) else always
