open Handler
open Lsp_helpers

let get_definition : Position.t -> Path.t -> Scopes.def list -> Scopes.def option =
 fun pos uri -> List.find ~f:(Def.is_reference pos uri)


type decl_def_or_impl =
  | Decl
  | Def
  | Impl

let find_module_with_uid_conversion
    :  (Scopes.Uid.t -> string) -> string list -> Scopes.Types.mdef list
    -> Scopes.Types.mdef option
  =
 fun to_string mod_path ->
  List.find_map ~f:(fun (mdef : Scopes.Types.mdef) ->
      Option.some_if
        (List.equal String.equal mod_path
        @@ List.map ~f:to_string (mdef.mod_path @ [ mdef.uid ]))
        mdef)


let find_module_by_uid_path
    : Scopes.Uid.t list -> Scopes.Types.mdef list -> Scopes.Types.mdef option
  =
 fun mod_path ->
  let to_string = Scopes.Uid.to_string in
  find_module_with_uid_conversion to_string (List.map ~f:to_string mod_path)


let find_module_by_string_path
    : string list -> Scopes.Types.mdef list -> Scopes.Types.mdef option
  =
  find_module_with_uid_conversion Scopes.Uid.to_name


let find_defs_by_name_and_level (def : Scopes.def) : Scopes.def list -> Scopes.def list =
  List.filter
    ~f:
      Scopes.Types.(
        fun def' ->
          String.equal (get_def_name def) (get_def_name def')
          &&
          match def, def' with
          | Variable _, Variable _ | Type _, Type _ | Module _, Module _ -> true
          | (Variable _ | Type _ | Module _), (Variable _ | Type _ | Module _) -> false)


module Mod_identifier = struct
  type t =
    | Standalone_signature_or_module of Scopes.Uid.t
    | Ad_hoc_signature of Scopes.Uid.t

  let compare (id1 : t) (id2 : t) : int =
    match id1, id2 with
    | Standalone_signature_or_module u1, Standalone_signature_or_module u2 ->
      Scopes.Uid.compare u1 u2
    | Standalone_signature_or_module _, Ad_hoc_signature _ -> -1
    | Ad_hoc_signature _, Standalone_signature_or_module _ -> 1
    | Ad_hoc_signature u1, Ad_hoc_signature u2 -> Scopes.Uid.compare u1 u2


  let pp : t Fmt.t =
   fun ppf -> function
    | Ad_hoc_signature uid -> Format.fprintf ppf "Ad_hoc_signature (%a)" Scopes.Uid.pp uid
    | Standalone_signature_or_module uid ->
      Format.fprintf ppf "Standalone_signature_or_module (%a)" Scopes.Uid.pp uid
end

module Mod_graph = Graph.Make (Mod_identifier)
module Mod_map = Caml.Map.Make (Mod_identifier)

let try_to_resolve_standalone
    :  Scopes.Types.mdef list -> module_path:(string list, Scopes.Uid.t list) Either.t
    -> resolved_module:Scopes.Uid.t option -> Scopes.Uid.t option
  =
 fun mdefs ~module_path ~resolved_module ->
  match resolved_module with
  | None ->
    (match module_path with
    | First module_path ->
      let%map.Option mdef = find_module_by_string_path module_path mdefs in
      mdef.uid
    | Second module_path -> List.last module_path)
  | Some resolved_module -> Some resolved_module


let implementation_to_identifier
    : Scopes.Types.mdef list -> Scopes.Types.implementation -> Mod_graph.vertex option
  =
 fun mdefs -> function
  | Ad_hoc_signature [] -> None
  | Ad_hoc_signature (dec :: _) -> Some (Ad_hoc_signature (Scopes.Types.get_def_uid dec))
  | Standalone_signature_or_module { module_path; resolved_module } ->
    let uid =
      match try_to_resolve_standalone mdefs ~module_path ~resolved_module with
      | None ->
        let name =
          String.concat ~sep:"."
          @@ Either.value_map
               ~first:Fn.id
               ~second:(List.map ~f:Scopes.Uid.to_name)
               module_path
        in
        Scopes.Uid.make name Loc.generated
      | Some uid -> uid
    in
    Some (Standalone_signature_or_module uid)


let mod_case_to_implementation : Scopes.Types.mod_case -> Scopes.Types.implementation
  = function
  | Def defs -> Ad_hoc_signature defs
  | Alias { module_path; resolved_module; file_name = _ } ->
    Standalone_signature_or_module { module_path = Second module_path; resolved_module }


let mdef_to_implementation : Scopes.Types.mdef -> Scopes.Types.implementation =
 fun mdef ->
  Standalone_signature_or_module
    { module_path = Second mdef.mod_path; resolved_module = Some mdef.uid }


let mdefs_to_identifiers : Scopes.Types.mdef list -> Scopes.Types.implementation Mod_map.t
  =
 fun mdefs ->
  Mod_map.of_seq
  @@ Caml.List.to_seq
  @@ List.concat_map mdefs ~f:(fun (mdef : Scopes.Types.mdef) ->
         ( Mod_identifier.Standalone_signature_or_module mdef.uid
         , mdef_to_implementation mdef )
         :: List.filter_map mdef.implements ~f:(fun impl ->
                Option.map (implementation_to_identifier mdefs impl) ~f:(fun id ->
                    id, impl)))


let build_mod_graph : Scopes.Types.mdef list -> Mod_graph.t =
 fun mdefs ->
  Mod_graph.(Fn.flip from_assoc empty)
  @@ List.concat_map mdefs ~f:(fun (child : Scopes.Types.mdef) ->
         let child_id = Mod_identifier.Standalone_signature_or_module child.uid in
         List.filter_map
           child.implements
           ~f:
             (Option.map ~f:(fun parent -> child_id, parent)
             <@ implementation_to_identifier mdefs))


let rec try_to_resolve_mod_case
    : Scopes.Types.mdef list -> Scopes.Types.mod_case -> Scopes.def list option
  =
 fun mdefs -> function
  | Def defs -> Some defs
  | Alias { module_path = _; resolved_module; file_name = _ } ->
    let%bind.Option resolved_module = resolved_module in
    let%bind.Option mdef =
      List.find mdefs ~f:(fun mdef -> Scopes.Uid.equal mdef.uid resolved_module)
    in
    try_to_resolve_mod_case mdefs mdef.mod_case


(** Given some definition, look for the module/namespace and signature/interface that
    declares it. *)
let get_declaration : Scopes.def -> Scopes.Types.mdef list -> Scopes.def list =
 fun def _mdefs -> [ def ]


(** Given some definition, look for all modules/namespaces and signatures/interfaces that
    implement it. *)
let get_implementations : Scopes.def -> Scopes.Types.mdef list -> Scopes.def list =
 fun def mdefs ->
  let mod_ids = mdefs_to_identifiers mdefs in
  match
    (* Is [def] defined inside a module? We need to find the module in order to look into
       every signature/interface. *)
    let%bind.Option def_mod_id =
      match find_module_by_uid_path (Scopes.Types.get_mod_path def) mdefs with
      | None ->
        Seq.find_map (function
            | id, Scopes.Types.Ad_hoc_signature defs ->
              if List.is_empty @@ find_defs_by_name_and_level def defs
              then None
              else Some id
            | _, Standalone_signature_or_module _ -> None)
        @@ Mod_map.to_seq mod_ids
      | Some def_mod -> Some (Mod_identifier.Standalone_signature_or_module def_mod.uid)
    in
    (* Find everything that implements [def_mod] by looking at its children. *)
    let%map.Option implementers =
      Mod_graph.reachable def_mod_id (Mod_graph.transpose @@ build_mod_graph mdefs)
    in
    let find_defs_in_implementation : Scopes.Types.implementation -> Def.t list option
      = function
      | Ad_hoc_signature _defs -> None (* We don't care about signatures *)
      | Standalone_signature_or_module { module_path; resolved_module } ->
        let%bind.Option uid =
          try_to_resolve_standalone mdefs ~module_path ~resolved_module
        in
        let%bind.Option mdef =
          List.find mdefs ~f:(fun mdef ->
              Scopes.Types.(
                match mdef.mdef_type with
                | Module -> String.equal (Uid.to_name uid) (get_mod_name_name mdef.name)
                | Signature -> false))
        in
        let%map.Option defs = try_to_resolve_mod_case mdefs mdef.mod_case in
        find_defs_by_name_and_level def defs
    in
    (* Search for [def] within the modules. *)
    List.concat
    @@ List.filter_map (Mod_graph.to_vertices implementers) ~f:(fun id ->
           Option.bind ~f:find_defs_in_implementation @@ Mod_map.find_opt id mod_ids)
  with
  | None | Some [] -> [ def ]
  | Some (_ :: _ as implementers) -> implementers


(** Given some definition, look for all modules/namespaces and signatures/interfaces that
    define it. This function goes in the opposite way compared to [get_implementers]. *)
let get_definitions : Scopes.def -> Scopes.Types.mdef list -> Scopes.def list =
 fun def mdefs ->
  (* Is [def] defined inside a module? We need to find the module in order to look into
     every signature/interface. *)
  let def_mod_opt = find_module_by_uid_path (Scopes.Types.get_mod_path def) mdefs in
  let rec find_defs_in_implementation
      : Scopes.Types.implementation -> Scopes.Types.def list
    = function
    | Ad_hoc_signature defs ->
      (* Got all definitions of this signature/interface, but is [def] inside? We do not
         compare by [Scopes.Uid.t] since we consider each declaration and implementation
         to be distinct. *)
      find_defs_by_name_and_level def defs
    | Standalone_signature_or_module { module_path; resolved_module = _ } ->
      (* This is an alias to an existing signature/interface, so we need to resolve it
         until it's [Def] to get its definitions and look if [def] is inside. We need to
         also look into every parent signature/interface.
           Note: We convert the UID path to a name path to find the module since we
         collect [Scopes.def]s from modules and signatures separately. So we need some way
         to test for equality since these two definitions have distinct UIDs. *)
      let%bind.List mdef =
        Option.to_list
        @@
        match module_path with
        | First module_path -> find_module_by_string_path module_path mdefs
        | Second module_path -> find_module_by_uid_path module_path mdefs
      in
      List.concat_map
        (* XXX: [mdef_to_implementation mdef] doesn't work. Why? *)
        (mod_case_to_implementation mdef.mod_case :: mdef.implements)
        ~f:find_defs_in_implementation
  in
  (* Are there signatures implementing [def_mod_opt] that actually define [def]? *)
  match
    List.bind (Option.to_list def_mod_opt) ~f:(fun def_mod ->
        List.bind def_mod.implements ~f:find_defs_in_implementation)
  with
  | [] -> [ def ]
  | _ :: _ as defs -> defs


let on_req_impl : decl_def_or_impl -> Position.t -> Path.t -> Locations.t option Handler.t
  =
 fun decl_def_or_impl pos file ->
  with_cached_doc_pure file ~default:None
  @@ fun { definitions; _ } ->
  let%bind.Option definition = get_definition pos file definitions in
  let definitions =
    (match decl_def_or_impl with
    | Decl -> get_declaration
    | Def -> get_definitions
    | Impl -> get_implementations)
      definition
      (List.filter_map definitions ~f:(function
          | Variable _ | Type _ -> None
          | Module mdef -> Some mdef))
  in
  match
    List.filter_map definitions ~f:(fun definition ->
        match Def.get_location definition with
        | File { range; path } ->
          Some (Location.create ~range ~uri:(DocumentUri.of_path path))
        | StdLib _ | Virtual _ -> None)
  with
  | [] -> None
  | _ :: _ as definitions -> Some (`Location definitions)


let on_req_declaration : Position.t -> Path.t -> Locations.t option Handler.t =
  on_req_impl Decl


let on_req_definition : Position.t -> Path.t -> Locations.t option Handler.t =
  on_req_impl Def


let on_req_implementation : Position.t -> Path.t -> Locations.t option Handler.t =
  on_req_impl Impl
