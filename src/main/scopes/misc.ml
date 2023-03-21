open Ligo_prim
open Types

let make_v_def
    :  ?core_type:Ast_core.type_expression -> def_type -> Value_var.t -> Location.t
    -> Location.t -> def option
  =
 fun ?core_type def_type var range body_range ->
  if Value_var.is_generated var
  then None
  else (
    let type_case =
      match core_type with
      | Some t -> Core t
      | None -> Unresolved
    in
    Some (make_v_def (get_binder_name var) type_case def_type range body_range))


let get_location_of_module_path : Module_var.t list -> Location.t =
 fun mvs ->
  List.fold mvs ~init:Location.dummy ~f:(fun loc m ->
      Location.cover loc (Module_var.get_location m))
