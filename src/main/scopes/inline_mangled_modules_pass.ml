open Types
module SMap = Caml.Map.Make (String)

type t = string Uid_map.t

(** Mapping from file name to mangled module's definitions *)
type mangled_mdefs = mdef SMap.t

(** Mapping from mangled UIDs to resolved UIDs. *)
type mangled_to_resolved = Uid.t Uid_map.t

let rec extracted_mangled_uids : def list -> mangled_to_resolved =
  List.fold ~init:Uid_map.empty ~f:(fun acc -> function
    | Variable _ | Type _ | Label _ -> acc
    | Module mdef ->
      (match mdef.mod_case with
      | Def defs ->
        Uid_map.union (fun _uid _fst snd -> Some snd) acc (extracted_mangled_uids defs)
      | Alias { resolve_mod_name = Unresolved_path _ } -> acc
      | Alias { resolve_mod_name = Resolved_path resolved } ->
        if Location.equal (Uid.to_location resolved.resolved_module) Location.env
           && String.is_prefix
                (Uid.to_name resolved.resolved_module)
                ~prefix:"Mangled_module_"
        then
          (* n.b.: If the same module is imported twice (or some constructor is exported
             twice from different imports), then by my observations, we'll pick the one
             that was imported first. *)
          Uid_map.add resolved.resolved_module mdef.uid acc
        else acc))


let unmangle_module_names_in_typed_type
    :  (Ast_typed.module_variable, Ast_typed.module_variable) Hashtbl.t
    -> mangled_to_resolved -> Ast_typed.type_expression -> Ast_typed.type_expression
  =
 fun mvar_cache mangled_to_resolved ->
  let find_if_not_generated mvar =
    if Ligo_prim.Module_var.is_generated mvar
    then mvar
    else
      Hashtbl.find_or_add mvar_cache mvar ~default:(fun () ->
          Option.value_map ~default:mvar ~f:id_to_mvar
          @@ Uid_map.find_opt (mvar_to_id mvar) mangled_to_resolved)
  in
  let unmangle_module_list = List.map ~f:find_if_not_generated in
  Misc.map_typed_type_expression_module_path unmangle_module_list


let extract_mangled_mdefs : t -> def list -> mangled_mdefs =
 fun mangled_uids defs ->
  let rec collect : def -> (string * mdef) list = function
    | Variable _ | Type _ | Label _ -> []
    | Module mdef ->
      let first_mapping =
        match Uid_map.find_opt mdef.uid mangled_uids with
        | None -> []
        | Some file_name -> [ file_name, mdef ]
      in
      (match mdef.mod_case with
      | Alias _ -> first_mapping
      | Def defs -> first_mapping @ List.concat_map ~f:collect defs)
  in
  defs
  |> List.concat_map ~f:collect
  |> List.fold_left ~init:SMap.empty ~f:(fun acc (key, data) -> SMap.add key data acc)


let strip_mangled_defs : mangled_mdefs -> t -> def list -> def list =
 fun mangled_file_name_to_mdef mangled_uids ->
  let rec mangled_alias_filter : def -> def option = function
    | (Variable _ | Type _ | Label _) as def -> Some def
    | Module mdef as def ->
      if Uid_map.mem mdef.uid mangled_uids
      then None
      else (
        match mdef.mod_case with
        | Alias _ -> Some def
        | Def defs ->
          Some
            (Module
               { mdef with mod_case = Def (List.filter_map ~f:mangled_alias_filter defs) }))
  in
  List.filter_map ~f:mangled_alias_filter


let inline_mangled
    :  (Ast_typed.module_variable, Ast_typed.module_variable) Hashtbl.t -> mangled_mdefs
    -> t -> mangled_to_resolved -> def list -> def list
  =
 fun mvar_cache mangled_file_name_to_mdef mangled_uids mangled_to_resolved ->
  let rec inline : def -> def = function
    | Variable vdef ->
      Variable
        { vdef with
          t =
            (match vdef.t with
            | Core core -> Core core
            | Resolved typed ->
              Resolved
                (unmangle_module_names_in_typed_type mvar_cache mangled_to_resolved typed)
            | Unresolved -> Unresolved)
        }
    | Type tdef -> Type tdef
    | Label ldef -> Label ldef
    | Module mdef ->
      let inlined_mdef =
        match mdef.mod_case with
        | Alias { resolve_mod_name = Resolved_path resolved } ->
          Option.value
            ~default:mdef
            (let open Option.Let_syntax in
            let mangled_uid = resolved.resolved_module in
            let%bind file_name = Uid_map.find_opt mangled_uid mangled_uids in
            let%bind mdef_orig = SMap.find_opt file_name mangled_file_name_to_mdef in
            return
            @@ { mdef with mod_case = mdef_orig.mod_case; inlined_name = Some file_name })
        | Alias { resolve_mod_name = Unresolved_path _ } | Def _ -> mdef
      in
      (match inlined_mdef.mod_case with
      | Alias _ -> Module inlined_mdef
      | Def defs -> Module { inlined_mdef with mod_case = Def (List.map ~f:inline defs) })
  in
  List.map ~f:inline


let patch : t -> def list -> def list =
 fun mangled_uids defs ->
  let mangled_file_name_to_mdef = extract_mangled_mdefs mangled_uids defs in
  let mangled_to_resolved = extracted_mangled_uids defs in
  let mvar_cache = Hashtbl.create (module Ligo_prim.Module_var) in
  defs
  |> strip_mangled_defs mangled_file_name_to_mdef mangled_uids
  |> inline_mangled mvar_cache mangled_file_name_to_mdef mangled_uids mangled_to_resolved
