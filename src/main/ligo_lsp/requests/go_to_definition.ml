open Handler
open Lsp_helpers

(** The type of the request we are getting. The Language Server Protocol doesn't explain
    the differences between each request, so we've implemented them in the following ways:

    * The [Decl]aration points to the exact symbol that gave a concrete body
      implementation to that symbol. It's a strict match on the UID of that symbol from
      where it was defined.
    * The [Def]inition tries to find the given symbol in all the signatures and interfaces
      that match its name and level (term, type, or module), provided that the module that
      implements it are implementers of such signatures. In other words, it looks for the
      abstract/virtual definitions of a symbol.
    * The [Impl]ementation looks into all modules and namespaces that give a concrete body
      to the target. It's the opposite of the [Def]inition in the sense that we are
      concerned in finding the symbol in modules and namespaces rather than signatures and
      interfaces.

    Assume that the top-level [t] in [M1.t] is our target, then the following CameLIGO
    example should summarize it:
    {[
      module type I1 = sig
        (* Definition of t *)
        type t
      end

      module type I2 = sig
        (* Definition of t *)
        type t
      end

      module type I = sig
        include I1
        include I2
      end

      module M1 : I = struct
        (* Implementation of t *)
        (* Declaration of t *)
        type t = unit
      end

      module M2 : I = struct
        (* Implementation of t *)
        type t = int
      end

      type t = M1.t
    ]} *)
type decl_def_or_impl =
  | Decl
  | Def
  | Impl

(** Seaches for a module definition whose module path name matches the provided path. The
    conversion function is used to convert an [Uid.t] into a module name. See
    [find_module_by_uid_path] and [find_module_by_string_path]. *)
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


(** Searches for a module definition that matches the provided [Uid.t] module path. *)
let find_module_by_uid_path
    : Scopes.Uid.t list -> Scopes.Types.mdef list -> Scopes.Types.mdef option
  =
  let to_string = Scopes.Uid.to_string in
  find_module_with_uid_conversion to_string <@ List.map ~f:to_string


(** Searches for a module definition that matches the provided [string] module path. *)
let find_module_by_string_path
    : string list -> Scopes.Types.mdef list -> Scopes.Types.mdef option
  =
  find_module_with_uid_conversion Scopes.Uid.to_name


(** Searches for a definition that has the same name and level (term level, type level,
    module level, or label level) as the provided definition, that was defined as late as
    possible, i.e., whose start position is the greatest. *)
let find_last_def_by_name_and_level (def : Scopes.def)
    : Scopes.def list -> Scopes.def option
  =
  let open Scopes.Types in
  List.max_elt ~compare:(fun def1 def2 ->
      Simple_utils.Location.compare (get_range def1) (get_range def2))
  <@ List.filter ~f:(fun def' ->
         match def, def' with
         | Variable _, Variable _ | Type _, Type _ | Module _, Module _ | Label _, Label _
           -> String.equal (get_def_name def) (get_def_name def')
         | ( (Variable _ | Type _ | Module _ | Label _)
           , (Variable _ | Type _ | Module _ | Label _) ) -> false)


module Mod_identifier = struct
  (** A module identifier may represent an ad-hoc signature, or a standalone signature or
      module.

      We say that an ad-hoc signature is one that is not bound to an interface or module
      type. For example, in [module M : sig type t end = struct type t = unit end], the
      signature annotating module [M] is ad-hoc. The UID of an ad-hoc signature is that of
      the first signature item defined in it.

      A standalone signature or module is the opposite, so in the same example, [M] is a
      standalone module. The UID of a standalone signature or module is that of itself. *)
  type t =
    | Ad_hoc_signature of Scopes.Uid.t
    | Standalone_signature_or_module of Scopes.Uid.t

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

(** A directed graph whose vertices represent modules, namespaces, interfaces, and
    signatures and whose edges represent that a module or namespace implements, extends,
    annotates, or includes another. *)
module Mod_graph = Graph.Make (Mod_identifier)

(** A map that allows looking up modules, namespaces, interfaces, and signatures given a
    [Mod_identifier.t]. *)
module Mod_map = Caml.Map.Make (Mod_identifier)

(** If the provided [resolve_mod_name] is resolved, then returns the resolved module.
    Otherwise, attempts to resolve it by approximating it with a module that has the same
    name. *)
let rec try_to_resolve_path
    : Scopes.Types.mdef list -> Scopes.Types.resolve_mod_name -> Scopes.Uid.t option
  =
 fun mdefs -> function
  | Unresolved_path { module_path } ->
    let%bind.Option mdef =
      find_module_by_string_path (List.map ~f:Scopes.Uid.to_name module_path) mdefs
    in
    (match mdef.mod_case with
    | Def _ -> Some mdef.uid
    | Alias { resolve_mod_name } -> try_to_resolve_path mdefs resolve_mod_name)
  | Resolved_path { module_path = _; resolved_module_path = _; resolved_module } ->
    Some resolved_module


(** Attempts to resolve the provided [resolve_mod_name] with [try_to_resolve_path] and
    creates a standalone signature or module identifier out of it. *)
let path_to_identifier
    : Scopes.Types.mdef list -> Scopes.Types.resolve_mod_name -> Mod_identifier.t option
  =
 fun mdefs ->
  Option.map ~f:(fun path -> Mod_identifier.Standalone_signature_or_module path)
  <@ try_to_resolve_path mdefs


(** "Strips" an implementation in order to get its module identifier. Returns [None] if an
    empty ad-hoc signature was provided, or if it wasn't possible to resolve the path of a
    standalone signature or module. *)
let implementation_to_identifier
    : Scopes.Types.mdef list -> Scopes.Types.implementation -> Mod_identifier.t option
  =
 fun mdefs -> function
  | Ad_hoc_signature [] -> None
  | Ad_hoc_signature (dec :: _) -> Some (Ad_hoc_signature (Scopes.Types.get_def_uid dec))
  | Standalone_signature_or_module path -> path_to_identifier mdefs path


(** Turns a namespace extension into an implementation. This is the same as just wrapping
    it into a [Standalone_signature_or_module]. *)
let extension_to_implementation : Scopes.Types.extension -> Scopes.Types.implementation =
 fun path -> Standalone_signature_or_module path


(** Gets a namespace extension's module identifier. This is the same as just calling
    [path_to_identifier]. *)
let extension_to_identifier
    : Scopes.Types.mdef list -> Scopes.Types.extension -> Mod_identifier.t option
  =
  path_to_identifier


(** Turns a [mod_case] into an [implementation]. A definition will become an ad-hoc
    signature, and an alias into a standalone signature or module. *)
let mod_case_to_implementation : Scopes.Types.mod_case -> Scopes.Types.implementation
  = function
  | Def defs -> Ad_hoc_signature defs
  | Alias { resolve_mod_name } -> Standalone_signature_or_module resolve_mod_name


(** Turns a module definition into an [implementation]. This is done by looking at its
    [mod_case] and calling [mod_case_to_implementation]. However, unlike that function
    where we don't have data about its [mod_path] and [uid], we can instead return a
    [Standalone_signature_or_module] for both cases. *)
let mdef_to_implementation : Scopes.Types.mdef -> Scopes.Types.implementation =
 fun mdef ->
  match mod_case_to_implementation mdef.mod_case with
  | Ad_hoc_signature _ ->
    Standalone_signature_or_module
      (Resolved_path
         { module_path = mdef.mod_path @ [ mdef.uid ]
         ; resolved_module_path = mdef.mod_path
         ; resolved_module = mdef.uid
         })
  | Standalone_signature_or_module _ as impl -> impl


(** Creates a map from the provided list of [Module]s that allows lookup by a
    [Mod_identifier.t]. *)
let mdefs_to_identifiers : Scopes.Types.mdef list -> Scopes.Types.implementation Mod_map.t
  =
 fun mdefs ->
  Mod_map.of_seq
  @@ Caml.List.to_seq
  @@ List.concat_map mdefs ~f:(fun mdef ->
         (( Mod_identifier.Standalone_signature_or_module mdef.uid
          , mdef_to_implementation mdef )
         :: List.filter_map mdef.implements ~f:(fun impl ->
                Option.map (implementation_to_identifier mdefs impl) ~f:(fun id ->
                    id, impl)))
         @ List.filter_map mdef.extends ~f:(fun ext ->
               Option.map (extension_to_identifier mdefs ext) ~f:(fun id ->
                   id, Scopes.Types.Standalone_signature_or_module ext)))


(** Creates a graph whose vertices are built from the provided list of [Module]s and whose
    edges represent that a vertex [m1] includes, annotates, extends, or implements another
    vertex [m2]. *)
let build_mod_graph : Scopes.Types.mdef list -> Mod_graph.t =
 fun mdefs ->
  Mod_graph.(Fn.flip from_assocs empty)
  @@ List.map mdefs ~f:(fun child ->
         ( Mod_identifier.Standalone_signature_or_module child.uid
         , List.filter_map
             (child.implements @ List.map child.extends ~f:extension_to_implementation)
             ~f:(implementation_to_identifier mdefs) ))


(** Given a [mod_case], tries to resolve by following aliases until it finds a definition.
    This is normally not needed since scopes should ensure that a [resolve_mod_name] has
    a fully resolved path, but it's included here to ensure a total function. *)
let rec try_to_resolve_mod_case
    : Scopes.Types.mdef list -> Scopes.Types.mod_case -> Scopes.def list option
  =
 fun mdefs -> function
  | Def defs -> Some defs
  | Alias { resolve_mod_name } ->
    let%bind.Option resolved_module =
      match resolve_mod_name with
      | Unresolved_path { module_path = _ } -> None
      | Resolved_path { module_path = _; resolved_module_path = _; resolved_module } ->
        Some resolved_module
    in
    let%bind.Option mdef =
      List.find mdefs ~f:(fun mdef -> Scopes.Uid.equal mdef.uid resolved_module)
    in
    try_to_resolve_mod_case mdefs mdef.mod_case


(** Given some definition, look for the module/namespace and signature/interface that
    declares it. *)
let get_declaration : Scopes.def -> Scopes.Types.mdef list -> Scopes.def list =
 fun def _mdefs -> [ def ]


(** Tries to find the module identifier that contains the given definition, or an
    approximation using [find_last_def_by_name_and_level]. *)
let try_to_get_mdef_uid
    :  Scopes.def -> Scopes.Types.mdef list -> Scopes.Types.implementation Mod_map.t
    -> Mod_identifier.t option
  =
 fun def mdefs mod_ids ->
  match find_module_by_uid_path (Scopes.Types.get_mod_path def) mdefs with
  | None ->
    Seq.find_map (function
        | id, Scopes.Types.Ad_hoc_signature defs ->
          Option.map (find_last_def_by_name_and_level def defs) ~f:(const id)
        | _, Standalone_signature_or_module _ -> None)
    @@ Mod_map.to_seq mod_ids
  | Some def_mod -> Some (Mod_identifier.Standalone_signature_or_module def_mod.uid)


(** Given some definition, look for all modules/namespaces and signatures/interfaces that
    implement it. *)
let get_implementations : Scopes.def -> Scopes.Types.mdef list -> Scopes.def list =
 fun def mdefs ->
  let mod_ids = mdefs_to_identifiers mdefs in
  (* Is [def] defined inside a module? We need to find the module in order to look into
     every signature/interface. It's possible that [def] is a signature, in which case we
     want to just look for its implementing modules. *)
  let sig_uid_opt =
    match def with
    | Variable _ | Type _ | Label _ -> None
    | Module mdef ->
      (match mdef.mdef_type with
      | Module -> None
      | Signature ->
        (match mdef.mod_case with
        | Def _ -> Some mdef.uid
        | Alias { resolve_mod_name } ->
          (match resolve_mod_name with
          | Unresolved_path _ -> None
          | Resolved_path { module_path = _; resolved_module_path = _; resolved_module }
            -> Some resolved_module)))
  in
  match
    let%bind.Option def_mod_id =
      (* If we are looking at a signature, and not a signature item, then we need to
         search for the signature itself. *)
      Option.first_some
        (try_to_get_mdef_uid def mdefs mod_ids)
        (Option.map sig_uid_opt ~f:(fun sig_uid ->
             Mod_identifier.Standalone_signature_or_module sig_uid))
    in
    let%map.Option defining_mods =
      List.find (Mod_graph.wcc @@ build_mod_graph mdefs) ~f:(Mod_graph.mem def_mod_id)
    in
    let find_defs_in_implementation : Scopes.Types.implementation -> Def.t option
      = function
      (* We don't care about signatures, as they don't implement anything. *)
      | Ad_hoc_signature _defs -> None
      | Standalone_signature_or_module path ->
        let%bind.Option uid = try_to_resolve_path mdefs path in
        let%bind.Option mdef =
          List.find mdefs ~f:(fun mdef ->
              Scopes.Types.(
                match mdef.mdef_type with
                | Module -> String.equal (Uid.to_name uid) mdef.name
                | Signature -> false))
        in
        (* It might be that we are not looking at a signature item, but rather at a
           signature name. In this case, we don't need to search inside the module
           definitions, but rather just return the module itself. *)
        if Option.is_some sig_uid_opt
        then Some (Module mdef : Scopes.def)
        else (
          let%bind.Option defs = try_to_resolve_mod_case mdefs mdef.mod_case in
          find_last_def_by_name_and_level def defs)
    in
    (* Search for [def] within the modules. *)
    List.filter_map (Mod_graph.to_vertices defining_mods) ~f:(fun id ->
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
      Option.to_list @@ find_last_def_by_name_and_level def defs
    | Standalone_signature_or_module path ->
      (* This is an alias to an existing signature/interface, so we need to resolve it
         until it's [Def] to get its definitions and look if [def] is inside. We need to
         also look into every parent signature/interface.
           Note: We convert the UID path to a name path to find the module since we
         collect [Scopes.def]s from modules and signatures separately. So we need some way
         to test for equality since these two definitions have distinct UIDs. *)
      let%bind.List mdef =
        Option.to_list
        @@
        match path with
        | Unresolved_path { module_path } ->
          find_module_by_string_path (List.map ~f:Scopes.Uid.to_name module_path) mdefs
        | Resolved_path { module_path = _; resolved_module_path = _; resolved_module } ->
          List.find mdefs ~f:(fun mdef -> Scopes.Uid.equal mdef.uid resolved_module)
      in
      List.concat_map
        (* Note: [mdef_to_implementation mdef] doesn't work because it resolves to
           [Standalone_signature_or_module] and creates a loop within
           [find_defs_in_implementation]. We want [mod_case_to_implementation
           mdef.mod_case] because if we get [Def], we'll resolve to [Ad_hoc_signature]. We
           could match directly on [mdef.mod_case], but we take this shortcut instead. *)
        (mod_case_to_implementation mdef.mod_case
        :: (mdef.implements @ List.map ~f:extension_to_implementation mdef.extends))
        ~f:find_defs_in_implementation
  in
  (* Are there signatures implementing [def_mod_opt] that actually define [def]? *)
  match
    List.bind (Option.to_list def_mod_opt) ~f:(fun def_mod ->
        List.bind
          (def_mod.implements @ List.map ~f:extension_to_implementation def_mod.extends)
          ~f:find_defs_in_implementation)
  with
  | [] -> [ def ]
  | _ :: _ as defs -> defs


(** Extracts every [Scopes.Types.Module] from the definitions. *)
let filter_mdefs : Scopes.definitions -> Scopes.Types.mdef list =
  Def.filter_map ~f:(function
      | Scopes.Types.Variable _ | Type _ | Label _ -> None
      | Module mdef -> Some mdef)


(** Generic handler for a declaration, definition, or implementation. Some of the logic is
    shared between the three, hence we have this extra handler. *)
let on_req_impl : decl_def_or_impl -> Position.t -> Path.t -> Locations.t option Handler.t
  =
 fun decl_def_or_impl pos file ->
  let@ normalize = ask_normalize in
  with_cached_doc_pure file ~default:None
  @@ fun { definitions; _ } ->
  let%bind.Option definition = Def.get_definition ~normalize pos file definitions in
  let definitions =
    (match decl_def_or_impl with
    | Decl -> get_declaration
    | Def -> get_definitions
    | Impl -> get_implementations)
      definition
      (filter_mdefs definitions)
  in
  match
    List.filter_map definitions ~f:(fun definition ->
        match Def.get_location ~normalize definition with
        | File { range; path } ->
          Some (Location.create ~range ~uri:(DocumentUri.of_path path))
        | StdLib _ | Virtual _ -> None)
  with
  | [] -> None
  | _ :: _ as definitions -> Some (`Location definitions)


(** Runs the handler for the declaration. This is normally when the user clicks "Go to
    Declaration". *)
let on_req_declaration : Position.t -> Path.t -> Locations.t option Handler.t =
  on_req_impl Decl


(** Runs the handler for the definition. This is normally when the user clicks "Go to
    Definition". *)
let on_req_definition : Position.t -> Path.t -> Locations.t option Handler.t =
  on_req_impl Def


(** Runs the handler for the implementation. This is normally when the user clicks "Go to
    Implementation" or "Find All Implementations". *)
let on_req_implementation : Position.t -> Path.t -> Locations.t option Handler.t =
  on_req_impl Impl
