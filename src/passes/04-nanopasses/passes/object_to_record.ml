open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let label_of_var x =
  let loc = Variable.get_location x in
  Location.wrap ~loc @@ Label.of_string (Variable.to_name_exn x)


let field_of_property ~raise : expr Object_.property -> (Label.t, expr) Field.t =
 fun { field_id; field_rhs } ->
  match field_id, field_rhs with
  | F_Name l, Some expr -> Complete (l, expr)
  | F_Name l, None -> Punned (Location.wrap ~loc:Location.generated l)
  | _, Some expr -> raise.error @@ unsupported_object_field expr
  | _, None ->
    raise.error @@ unsupported_object_field (e_string ~loc:Location.generated "TODO")


let field_update_of_property ~raise : expr Object_.property -> expr Update.field =
 fun { field_id; field_rhs } ->
  match field_id, field_rhs with
  | F_Name l, Some expr ->
    Full_field { field_lhs = [ FieldName l ]; field_lens = Lens_Id; field_rhs = expr }
  | F_Name l, None -> Pun (Location.wrap ~loc:Location.generated l)
  | _, Some expr -> raise.error @@ unsupported_object_field expr
  | _, None ->
    raise.error @@ unsupported_object_field (e_string ~loc:Location.generated "TODO")


let compile ~raise =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_object_update { object_; updates } ->
      let update = List.map ~f:(field_update_of_property ~raise) updates in
      e_update ~loc { structure = object_; update }
    | E_object fields ->
      let fields = List.map ~f:(field_of_property ~raise) fields in
      e_record_pun ~loc fields
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_object _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* TODO*)
