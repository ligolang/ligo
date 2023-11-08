module PP_helpers = Simple_utils.PP_helpers
open Types

let scopes : Format.formatter -> scopes -> unit =
 fun f s ->
  let pp_scope f (scope : scope) =
    let loc, defs = scope in
    let defs =
      List.sort defs ~compare:(fun d1 d2 ->
          Simple_utils.Location_ordered.compare (get_range d1) (get_range d2))
    in
    let pp_bindings f defs =
      List.iter defs ~f:(fun def ->
          Format.fprintf f "%s " (Uid.to_string @@ get_def_uid def))
    in
    Format.fprintf f "[ %a ] %a" pp_bindings defs Location.pp loc
  in
  let pp_scopes f = List.iter ~f:(Format.fprintf f "@[<v>%a@ @]" pp_scope) in
  let s =
    List.sort s ~compare:(fun (l1, _) (l2, _) ->
        Simple_utils.Location_ordered.compare l1 l2)
  in
  Format.fprintf f "@[<v>Scopes:@ %a@]" pp_scopes s


let rec definitions : Format.formatter -> def list -> unit =
 fun f defs ->
  let defs =
    List.sort defs ~compare:(fun d1 d2 ->
        Simple_utils.Location_ordered.compare (get_range d1) (get_range d2))
  in
  let refs ppf locs =
    let locs = LSet.elements locs in
    match locs with
    | [] -> Format.fprintf ppf "references: []"
    | locs ->
      let locs = List.sort locs ~compare:Location.compare in
      Format.fprintf
        ppf
        "@[<hv 2>references:@ %a@]"
        PP_helpers.(list_sep Location.pp (tag " ,@ "))
        locs
  in
  let pp_def_type ppf = function
    | Local -> Format.fprintf ppf "Local"
    | Global -> Format.fprintf ppf "Global"
    | Parameter -> Format.fprintf ppf "Parameter"
    | Module_field -> Format.fprintf ppf "Module_field"
  in
  let pp_content ppf = function
    | Variable v ->
      let typ ppf t =
        match t with
        | Core t -> Format.fprintf ppf "core: %a" Ast_core.PP.type_expression t
        | Resolved t -> Format.fprintf ppf "resolved: %a" Ast_typed.PP.type_expression t
        | Unresolved -> Format.fprintf ppf "unresolved"
      in
      Format.fprintf
        ppf
        "|%a|@ %a @ Mod Path = %a @ Def Type = %a"
        typ
        v.t
        refs
        v.references
        (PP_helpers.list String.pp)
        v.mod_path
        pp_def_type
        v.def_type
    | Type t ->
      let content ppf content =
        match content with
        | None -> Format.fprintf ppf ""
        | Some content -> Format.fprintf ppf "%a" Ast_core.PP.type_expression content
      in
      Format.fprintf ppf ": |%a|@ %a" content t.content refs t.references
    | Module
        { mod_case =
            Alias { module_path = a; resolved_module = _resolved; file_name = _file_name }
        ; references
        ; _
        } ->
      Format.fprintf
        ppf
        "Alias: %s @ %a @ "
        (String.concat ~sep:"." @@ List.map ~f:Uid.to_string a)
        refs
        references
    | Module { mod_case = Def d; references; _ } ->
      Format.fprintf ppf "Members: %a @ %a @ " definitions d refs references
  in
  let variables, types_modules =
    List.partition_tf
      ~f:(function
        | Type _ | Module _ -> false
        | Variable _ -> true)
      defs
  in
  let types, modules =
    List.partition_tf
      ~f:(function
        | Type _ -> true
        | _ -> false)
      types_modules
  in
  let pp_body_range f def =
    match get_body_range def with
    | None -> Format.fprintf f ""
    | Some range -> Format.fprintf f "%a" Location.pp range
  in
  let pp_def f =
    List.iter ~f:(fun def ->
        Format.fprintf
          f
          "(%s -> %s) @ Range: %a @ Body Range: %a @ Content: %a@ "
          (Uid.to_string @@ get_def_uid def)
          (get_def_name def)
          Location.pp
          (get_range def)
          pp_body_range
          def
          pp_content
          def)
  in
  Format.fprintf
    f
    "@[<v>Variable definitions:@ %aType definitions:@ %aModule definitions:@ %a@]"
    pp_def
    variables
    pp_def
    types
    pp_def
    modules


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
  let body_range_to_yojson = option_to_yojson Location.to_yojson in
  let content_to_yojson = option_to_yojson Ast_core.type_expression_to_yojson in
  let definition ~name ~range ~body_range ~t ~references =
    let references = LSet.elements references in
    `Assoc
      [ "name", `String name
      ; "range", Location.to_yojson range
      ; "body_range", body_range_to_yojson body_range
      ; "t", type_case_to_yojson t
      ; "references", `List (List.map ~f:Location.to_yojson references)
      ]
  in
  let type_definition ~name ~range ~body_range ~content ~references =
    let references = LSet.elements references in
    `Assoc
      [ "name", `String name
      ; "range", Location.to_yojson range
      ; "body_range", body_range_to_yojson body_range
      ; "content", content_to_yojson content
      ; "references", `List (List.map ~f:Location.to_yojson references)
      ]
  in
  let aux = function
    | Variable { name; range; body_range; t; references; uid; def_type = _; mod_path = _ }
      -> uid, definition ~name ~range ~body_range ~t ~references
    | Type
        { name; range; body_range; content; uid; def_type = _; references; mod_path = _ }
      -> uid, type_definition ~name ~range ~body_range ~content ~references
    | Module
        { name
        ; range
        ; body_range
        ; mod_case = Def d
        ; references
        ; uid
        ; def_type = _
        ; mod_path = _
        ; signature = _
        } ->
      ( uid
      , `Assoc
          [ ( "definition"
            , definition
                ~name:(get_mod_name_name name)
                ~range
                ~body_range
                ~references
                ~t:Unresolved )
          ; "members", defs_json d
          ] )
    | Module
        { name
        ; range
        ; body_range
        ; mod_case =
            Alias { module_path = a; resolved_module = _resolved; file_name = _file_name }
        ; references
        ; uid
        ; def_type = _
        ; mod_path = _
        ; signature = _
        } ->
      let alias = `List (List.map a ~f:(fun s -> `String (Uid.to_string s))) in
      ( uid
      , `Assoc
          [ ( "definition"
            , definition
                ~name:(get_mod_name_name name)
                ~range
                ~body_range
                ~references
                ~t:Unresolved )
          ; "alias", alias
          ] )
  in
  Tuple2.map_fst ~f:Uid.to_string @@ aux def


and defs_json (defs : def list) : Yojson.Safe.t =
  let get_defs defs =
    let variables, types_modules =
      List.partition_tf defs ~f:(function
          | Variable _ -> true
          | Type _ | Module _ -> false)
    in
    let types, modules =
      List.partition_tf types_modules ~f:(function
          | Type _ -> true
          | Variable _ | Module _ -> false)
    in
    let modules, module_aliases =
      List.partition_tf modules ~f:(function
          | Module { mod_case = Def _; _ } -> true
          | Variable _ | Type _ | Module { mod_case = Alias _; _ } -> false)
    in
    [ "variables", `Assoc (List.map ~f:(fun def -> def_to_yojson def) variables)
    ; "types", `Assoc (List.map ~f:(fun def -> def_to_yojson def) types)
    ; "modules", `Assoc (List.map ~f:(fun def -> def_to_yojson def) modules)
    ; "module_aliases", `Assoc (List.map ~f:(fun def -> def_to_yojson def) module_aliases)
    ]
  in
  `Assoc (get_defs defs)


let scopes_json (scopes : scopes) : Yojson.Safe.t =
  `List
    (List.map
       ~f:(fun scope ->
         let loc, defs = scope in
         let variables, types_modules =
           List.partition_tf defs ~f:(function
               | Type _ | Module _ -> false
               | Variable _ -> true)
         in
         let types, modules =
           List.partition_tf types_modules ~f:(function
               | Type _ -> true
               | Module _ | Variable _ -> false)
         in
         let vs =
           List.map ~f:(fun def -> `String (Uid.to_string @@ get_def_uid def)) variables
         in
         let ts =
           List.map ~f:(fun def -> `String (Uid.to_string @@ get_def_uid def)) types
         in
         let ms =
           List.map ~f:(fun def -> `String (Uid.to_string @@ get_def_uid def)) modules
         in
         `Assoc
           [ "range", Location.to_yojson loc
           ; "expression_environment", `List vs
           ; "type_environment", `List ts
           ; "module_environment", `List ms
           ])
       scopes)


let mod_name (base_path : string option) : Format.formatter -> mod_name -> unit =
 fun f -> function
  | Original n -> Format.fprintf f "%s" n
  | Filename n ->
    Format.fprintf
      f
      {|"%s"|}
      (Option.value_map base_path ~default:n ~f:(fun base_path ->
           FilePath.make_relative base_path n))
