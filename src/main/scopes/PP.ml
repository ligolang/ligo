open Simple_utils.Function
module PP_helpers = Simple_utils.PP_helpers
open Types

(** Helper for printing a [type_case] or [signature_case]. *)
let resolve_case
    :  'core_expr Fmt.t -> 'typed_expr Fmt.t
    -> ('core_expr, 'typed_expr) resolve_case Fmt.t
  =
 fun pp_core pp_typed ppf -> function
  | Core core -> Format.fprintf ppf "core: %a" pp_core core
  | Resolved typed -> Format.fprintf ppf "resolved: %a" pp_typed typed
  | Unresolved -> Format.fprintf ppf "unresolved"


(** Prints the [label_case] field from a [ldef]. *)
let label_case : label_case Fmt.t =
 fun ppf ->
  Format.fprintf ppf
  <@ function
  | Ctor -> "Constructor"
  | Field -> "Field"


(** Prints the [type_case] field from a [vdef]. *)
let type_case : type_case Fmt.t =
  resolve_case Ast_core.PP.type_expression Ast_typed.PP.type_expression_orig


(** Prints the [signature_case] field from an [mdef]. *)
let signature_case : signature_case Fmt.t =
  resolve_case Ast_core.PP.signature Ast_typed.PP.signature


(** Prints a list of scopes. The format for each scope will be [[ bindings ] loc] where
    [bindings] is a collection of UIDs in-scope in [loc]. *)
let scopes : Format.formatter -> inlined_scopes -> unit =
 fun f s ->
  let pp_scope f (scope : inlined_scope) =
    let loc, defs = scope in
    let defs =
      List.sort defs ~compare:(fun d1 d2 ->
          Simple_utils.Location.compare (get_range d1) (get_range d2))
    in
    let pp_bindings f = List.iter ~f:(Format.fprintf f "%a " Uid.pp <@ get_def_uid) in
    Format.fprintf f "[ %a ] %a" pp_bindings defs Location.pp loc
  in
  let pp_scopes f = List.iter ~f:(Format.fprintf f "@[<v>%a@ @]" pp_scope) in
  let s =
    List.sort s ~compare:(fun (l1, _) (l2, _) -> Simple_utils.Location.compare l1 l2)
  in
  Format.fprintf f "@[<v>Scopes:@ %a@]" pp_scopes s


(** Prints the [references] field from a definition. *)
let refs : Format.formatter -> LSet.t -> unit =
 fun ppf locs ->
  match Core.Set.to_list locs with
  | [] -> Format.fprintf ppf "references: []"
  | locs ->
    let locs = List.sort locs ~compare:Location.compare in
    Format.fprintf
      ppf
      "@[<hv 2>references:@ %a@]"
      PP_helpers.(list_sep Location.pp (tag " ,@ "))
      locs


(** Prints a [vdef] in the following format:

    |[type_case]|
    [references]
    Mod Path = [mod_path]
    Def Type = [def_type] *)
let vdef : Format.formatter -> vdef -> unit =
 fun ppf { t; references; mod_path; def_type; _ } ->
  let pp_def_type ppf =
    Format.fprintf ppf
    <@ function
    | Local -> "Local"
    | Global -> "Global"
    | Parameter -> "Parameter"
    | Module_field -> "Module_field"
  in
  Format.fprintf
    ppf
    "|%a|@ %a @ Mod Path = %a @ Def Type = %a"
    type_case
    t
    refs
    references
    (PP_helpers.list (fun ppf -> String.pp ppf <@ Uid.to_name))
    mod_path
    pp_def_type
    def_type


(** Prints a [tdef] in the following format:

    |[content]|
    [references] *)
let tdef : Format.formatter -> tdef -> unit =
 fun ppf { content; references; _ } ->
  let pp_content ppf content =
    match content with
    | None -> ()
    | Some content -> Ast_core.PP.type_expression ppf content
  in
  Format.fprintf ppf "|%a|@ %a" pp_content content refs references


(** Prints a dot-separated module path using [Uid.pp] for the module names. *)
let rec path : Uid.t list Fmt.t =
 fun ppf -> function
  | [] -> ()
  | [ m ] -> Uid.pp ppf m
  | m :: ms -> Format.fprintf ppf "%a.%a" Uid.pp m path ms


(** Prints a [resolve_mod_name]. This function might have three different notations:
    * "M (unresolved)": the module path M points to an unresolved module.
    * "M (-> N"): the module path M points to a module path N where each module name has
      been resolved.
    * "M (N -> O)": the module path M points to a module path N where each module name has
      been resolved, which in turn resolves to a module name O such that the last module
      name of N is not the same as O.
    Most modules will fall in the second case. The third case is interesting because most
    modules paths won't have so many layers of indirection, in which the alias has parts
    pointing to other aliases, and so the last module name might already be resolved to a
    definition. See [module2.mligo] and [module3.mligo] (in
    [test/contracts/get_scope_tests]) and [src/bin/expect_tests/get_scope.ml] for examples
    of tests that fall into the third case. *)
let resolve_mod_name : resolve_mod_name Fmt.t =
 fun ppf -> function
  | Unresolved_path { module_path } ->
    Format.fprintf ppf "%a (unresolved)" path module_path
  | Resolved_path { module_path; resolved_module_path; resolved_module } ->
    if Option.equal Uid.equal (List.last resolved_module_path) (Some resolved_module)
    then Format.fprintf ppf "%a (-> %a)" path module_path path resolved_module_path
    else
      Format.fprintf
        ppf
        "%a (%a -> %a)"
        path
        module_path
        path
        resolved_module_path
        Uid.pp
        resolved_module


(** Prints a [tdef] in the following format:

    [content]
    [label_case] *)
let ldef : Format.formatter -> ldef -> unit =
 fun ppf { content; label_case = case; _ } ->
  Format.fprintf ppf "%a @ %a @ " Ast_core.PP.type_expression content label_case case


(** Prints a [mdef] in the following format:

    [mod_case]
    [references]
    Implements: [implements]
    Extends: [extends] *)
let rec mdef : Format.formatter -> mdef -> unit =
 fun ppf { mod_case = mod_case'; references; implements; extends; _ } ->
  let pp_implements ppf =
    List.iter ~f:(Format.fprintf ppf "Implements: %a\n" implementation)
  in
  let pp_extends ppf =
    List.iter ~f:(Format.fprintf ppf "Extends: %a\n" resolve_mod_name)
  in
  Format.fprintf
    ppf
    "%a @ %a @ %a @ %a @ "
    mod_case
    mod_case'
    refs
    references
    pp_implements
    implements
    pp_extends
    extends


(** Prints an [implementation]. *)
and implementation : Format.formatter -> implementation -> unit =
 fun ppf -> function
  | Ad_hoc_signature d -> Format.fprintf ppf "Ad hoc: %a" definitions d
  | Standalone_signature_or_module path ->
    Format.fprintf ppf "Standalone: %a" resolve_mod_name path


(** Prints a [mod_case]. *)
and mod_case : Format.formatter -> mod_case -> unit =
 fun ppf -> function
  | Alias { resolve_mod_name = path } -> resolve_mod_name ppf path
  | Def d -> Format.fprintf ppf "Members: %a" definitions d


(** Prints a definition in the following format:
    ([uid] -> [name])
    Range: [range]
    Decl Range: [decl_range]
    Content: [content] *)
and definition : Format.formatter -> def -> unit =
 fun ppf def ->
  let pp_content ppf = function
    | Variable v -> vdef ppf v
    | Type t -> tdef ppf t
    | Module m -> mdef ppf m
    | Label l -> ldef ppf l
  in
  let pp_def ppf def =
    Format.fprintf
      ppf
      "(%s -> %s) @ Range: %a @ Decl Range: %a @ Content: %a@ "
      (Uid.to_string @@ get_def_uid def)
      (get_def_name def)
      Location.pp
      (get_range def)
      Location.pp
      (get_decl_range def)
      pp_content
      def
  in
  Format.fprintf ppf "%a" pp_def def


(** Prints a list of definitions. The definitions will be sorted by their [range]s, and
    partitioned into variables, types, labels, and modules. *)
and definitions : Format.formatter -> def list -> unit =
 fun ppf defs ->
  let defs =
    List.sort defs ~compare:(fun d1 d2 ->
        Simple_utils.Location.compare (get_range d1) (get_range d2))
  in
  let variables, labels_types_modules =
    List.partition_tf
      ~f:(function
        | Type _ | Module _ | Label _ -> false
        | Variable _ -> true)
      defs
  in
  let labels, types_modules =
    List.partition_tf
      ~f:(function
        | Variable _ | Type _ | Module _ -> false
        | Label _ -> true)
      labels_types_modules
  in
  let types, modules =
    List.partition_tf
      ~f:(function
        | Type _ -> true
        | Variable _ | Module _ | Label _ -> false)
      types_modules
  in
  let pp_defs ppf = List.iter ~f:(definition ppf) in
  Format.fprintf
    ppf
    "@[<v>Variable definitions:@ %aType definitions:@ %aConstructors and fields:@ \
     %aModule definitions:@ %a@]"
    pp_defs
    variables
    pp_defs
    types
    pp_defs
    labels
    pp_defs
    modules


(** Convert a definition to JSON. The first element of the tuple is that definition's UID
    converted to string. *)
let rec def_to_yojson : def -> string * Yojson.Safe.t =
 fun def ->
  let type_case_to_yojson = function
    | Core t -> `Assoc [ "core", Ast_core.type_expression_to_yojson t ]
    | Resolved t -> `Assoc [ "resolved", Ast_typed.type_expression_to_yojson t ]
    | Unresolved -> `Assoc [ "unresolved", `Null ]
  in
  let option_to_yojson to_yojson = function
    | None -> `Null
    | Some x -> to_yojson x
  in
  let content_to_yojson = option_to_yojson Ast_core.type_expression_to_yojson in
  let definition ~name ~range ~t ~references =
    let references = Core.Set.to_list references in
    `Assoc
      [ "name", `String name
      ; "range", Location.to_yojson range
      ; "t", type_case_to_yojson t
      ; "references", `List (List.map ~f:Location.to_yojson references)
      ]
  in
  let type_definition ~name ~range ~content ~references =
    let references = Core.Set.to_list references in
    `Assoc
      [ "name", `String name
      ; "range", Location.to_yojson range
      ; "content", content_to_yojson content
      ; "references", `List (List.map ~f:Location.to_yojson references)
      ]
  in
  let aux = function
    | Variable
        { name
        ; range
        ; decl_range = _
        ; t
        ; references
        ; uid
        ; def_type = _
        ; mod_path = _
        ; attributes = _
        } -> uid, definition ~name ~range ~t ~references
    | Type
        { name
        ; range
        ; decl_range = _
        ; content
        ; uid
        ; def_type = _
        ; references
        ; mod_path = _
        ; attributes = _
        } -> uid, type_definition ~name ~range ~content ~references
    | Label
        { name
        ; range
        ; uid
        ; references
        ; content
        ; decl_range = _
        ; def_type = _
        ; mod_path = _
        ; orig_type_loc = _
        ; label_case = _
        } -> uid, type_definition ~name ~range ~content:(Some content) ~references
    | Module
        { name
        ; range
        ; decl_range = _
        ; mod_case
        ; references
        ; uid
        ; def_type = _
        ; mod_path = _
        ; signature = _
        ; attributes = _
        ; implements = _
        ; extends = _
        ; mdef_type = _
        ; inlined_name = _
        } ->
      let def = "definition", definition ~name ~range ~references ~t:Unresolved in
      let decl =
        match mod_case with
        | Def d -> "members", defs_json d
        | Alias { resolve_mod_name } ->
          ( "alias"
          , `List
              (List.map (get_module_path resolve_mod_name) ~f:(fun s ->
                   `String (Uid.to_string s))) )
      in
      uid, `Assoc [ def; decl ]
  in
  Tuple2.map_fst ~f:Uid.to_string @@ aux def


(** Convert a list of definitions to JSON. *)
and defs_json (defs : def list) : Yojson.Safe.t =
  let get_defs defs =
    let variables, labels_types_modules =
      List.partition_tf defs ~f:(function
          | Variable _ -> true
          | Type _ | Module _ | Label _ -> false)
    in
    let labels, types_modules =
      List.partition_tf labels_types_modules ~f:(function
          | Label _ -> true
          | Variable _ | Type _ | Module _ -> false)
    in
    let types, modules =
      List.partition_tf types_modules ~f:(function
          | Type _ -> true
          | Variable _ | Module _ | Label _ -> false)
    in
    let modules, module_aliases =
      List.partition_tf modules ~f:(function
          | Module { mod_case = Def _; _ } -> true
          | Variable _ | Type _ | Module { mod_case = Alias _; _ } | Label _ -> false)
    in
    [ "variables", `Assoc (List.map ~f:(fun def -> def_to_yojson def) variables)
    ; "types", `Assoc (List.map ~f:(fun def -> def_to_yojson def) types)
    ; "labels", `Assoc (List.map ~f:(fun def -> def_to_yojson def) labels)
    ; "modules", `Assoc (List.map ~f:(fun def -> def_to_yojson def) modules)
    ; "module_aliases", `Assoc (List.map ~f:(fun def -> def_to_yojson def) module_aliases)
    ]
  in
  `Assoc (get_defs defs)


(** Convert scopes to JSON. *)
let scopes_json (scopes : inlined_scopes) : Yojson.Safe.t =
  `List
    (List.map
       ~f:(fun scope ->
         let loc, defs = scope in
         let variables, labels_types_modules =
           List.partition_tf defs ~f:(function
               | Type _ | Module _ | Label _ -> false
               | Variable _ -> true)
         in
         let labels, types_modules =
           List.partition_tf labels_types_modules ~f:(function
               | Variable _ | Type _ | Module _ -> false
               | Label _ -> true)
         in
         let types, modules =
           List.partition_tf types_modules ~f:(function
               | Type _ -> true
               | Module _ | Variable _ | Label _ -> false)
         in
         let vs =
           List.map ~f:(fun def -> `String (Uid.to_string @@ get_def_uid def)) variables
         in
         let ts =
           List.map ~f:(fun def -> `String (Uid.to_string @@ get_def_uid def)) types
         in
         let ls =
           List.map ~f:(fun def -> `String (Uid.to_string @@ get_def_uid def)) labels
         in
         let ms =
           List.map ~f:(fun def -> `String (Uid.to_string @@ get_def_uid def)) modules
         in
         `Assoc
           [ "range", Location.to_yojson loc
           ; "expression_environment", `List vs
           ; "type_environment", `List ts
           ; "label_environment", `List ls
           ; "module_environment", `List ms
           ])
       scopes)
