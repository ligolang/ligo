open Types
module SMap = Caml.Map.Make (String)
module UidMap = Caml.Map.Make (Uid)

(** Mapping from mangled uid to its resolved file name. *)
type t = string UidMap.t

let empty : t = UidMap.empty

(** Mapping from file name to mangled module's definitions *)
type mangled_mdefs = mdef SMap.t

(** Mapping from mangled UIDs to resolved UIDs. *)
type mangled_to_resolved = Uid.t UidMap.t

let rec extracted_mangled_uids : def list -> mangled_to_resolved =
  List.fold ~init:UidMap.empty ~f:(fun acc -> function
    | Variable _ | Type _ | Label _ -> acc
    | Module mdef ->
      (match mdef.mod_case with
      | Def defs ->
        UidMap.union (fun _uid _fst snd -> Some snd) acc (extracted_mangled_uids defs)
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
          UidMap.add resolved.resolved_module mdef.uid acc
        else acc))


let (unmangle_module_names_in_core_type, unmangle_module_names_in_typed_type) :
      (mangled_to_resolved -> Ast_core.type_expression -> Ast_core.type_expression)
      * (mangled_to_resolved -> Ast_typed.type_expression -> Ast_typed.type_expression)
  =
  let find_if_not_generated mangled_to_resolved mvar =
    if Ligo_prim.Module_var.is_generated mvar
    then mvar
    else
      Option.value_map ~default:mvar ~f:id_to_mvar
      @@ UidMap.find_opt (mvar_to_id mvar) mangled_to_resolved
  in
  let unmangle_module_list mangled_to_resolved =
    List.map ~f:(find_if_not_generated mangled_to_resolved)
  in
  ( (fun mangled_to_resolved ->
      Misc.map_core_type_expression_module_path
        (unmangle_module_list mangled_to_resolved)
        (List.Ne.map (find_if_not_generated mangled_to_resolved)))
  , fun mangled_to_resolved ->
      Misc.map_typed_type_expression_module_path
        (unmangle_module_list mangled_to_resolved) )


let extract_mangled_mdefs : t -> def list -> mangled_mdefs =
 fun mangled_uids defs ->
  let rec collect : def -> (string * mdef) list = function
    | Variable _ | Type _ | Label _ -> []
    | Module mdef ->
      let first_mapping =
        match UidMap.find_opt mdef.uid mangled_uids with
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
      if UidMap.mem mdef.uid mangled_uids
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


let inline_mangled : mangled_mdefs -> t -> mangled_to_resolved -> def list -> def list =
 fun mangled_file_name_to_mdef mangled_uids mangled_to_resolved ->
  let rec inline : def -> def = function
    | Variable vdef ->
      Variable
        { vdef with
          t =
            (match vdef.t with
            | Core core ->
              Core (unmangle_module_names_in_core_type mangled_to_resolved core)
            | Resolved typed ->
              Resolved (unmangle_module_names_in_typed_type mangled_to_resolved typed)
            | Unresolved -> Unresolved)
        }
    | Type tdef ->
      Type
        { tdef with
          content =
            Option.map
              ~f:(unmangle_module_names_in_core_type mangled_to_resolved)
              tdef.content
        }
    | Label ldef ->
      Label
        { ldef with
          content = unmangle_module_names_in_core_type mangled_to_resolved ldef.content
        }
    | Module mdef ->
      let inlined_mdef =
        match mdef.mod_case with
        | Alias { resolve_mod_name = Resolved_path resolved } ->
          Option.value
            ~default:mdef
            (let open Option.Let_syntax in
            let mangled_uid = resolved.resolved_module in
            let%bind file_name = UidMap.find_opt mangled_uid mangled_uids in
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


(** Preprocessing produces mangled modules.
    The next thing
    {[
     #import "file.mligo" "A"
    ]}
    translates into
    {[
     module A = Mangled_bla_bla
    ]}
    Thus we get a [Mangled_bla_bla] definition and [A] module alias.

    The idea here is to strip mangled definitions and inline them into each mangled alias.
    Motivation is simple: we don't care about mangled modules. We should think about
    them each time we're working with definitions (e.g. they may appear in completions).
    It would be better to inline and forget about them. *)
let patch : t -> def list -> def list =
 fun mangled_uids defs ->
  let mangled_file_name_to_mdef = extract_mangled_mdefs mangled_uids defs in
  let mangled_to_resolved = extracted_mangled_uids defs in
  defs
  |> strip_mangled_defs mangled_file_name_to_mdef mangled_uids
  |> inline_mangled mangled_file_name_to_mdef mangled_uids mangled_to_resolved
