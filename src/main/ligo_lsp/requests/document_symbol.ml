open Handler
open Lsp_helpers

let create_hierarchy : Def.t list -> Def.t Rose.forest =
  Rose.map_forest ~f:snd
  <@ Rose.forest_of_list
       ~compare:(fun ((range1 : Range.t), _def1) (range2, _def2) ->
         let ord = Position.compare range1.start range2.start in
         if ord = 0 then Position.compare range2.end_ range1.end_ else ord)
       ~intersects:(fun (range1, _def1) (range2, _def2) -> Range.intersects range1 range2)
  <@ List.filter_map ~f:(fun def ->
         let loc =
           match Scopes.Types.get_body_range def with
           | None -> Scopes.Types.get_range def
           | Some range -> range
         in
         let%map.Option range = Range.of_loc loc in
         range, def)


let rec get_all_symbols_hierarchy
    : Syntax_types.t -> Def.t Rose.tree -> DocumentSymbol.t option
  =
 fun syntax (Tree (def, defs)) ->
  let detail, kind, name, body_range, range =
    match def with
    | Variable ({ name; range; body_range; def_type; _ } as vdef) ->
      let type_info = Def.get_type vdef in
      let detail =
        Option.map type_info ~f:(Pretty.show_type ~syntax <@ Def.use_var_name_if_available)
      in
      let is_field =
        match def_type with
        | Module_field -> true
        | Local | Parameter | Global -> false
      in
      let kind =
        match type_info with
        | None -> if is_field then SymbolKind.Field else Variable
        | Some { var_name = _; contents = { type_content; location = _ } } ->
          (match type_content with
          | T_sum _ -> Constructor (* or maybe [EnumMember]? *)
          | T_record _ -> Field
          | T_arrow _ -> if is_field then Method else Function
          | _ -> Variable)
      in
      detail, kind, name, body_range, range
    | Type { name; range; body_range; content; _ } ->
      let detail = Option.map content ~f:(Pretty.show_type ~syntax) in
      let kind =
        match content with
        | None -> SymbolKind.TypeParameter
        | Some { type_content; location = _ } ->
          (match type_content with
          | T_sum _ -> Enum
          | _ -> Struct)
      in
      detail, kind, name, body_range, range
    | Module { name; range; body_range; mdef_type; signature; _ } ->
      let detail =
        match%bind.Option
          match signature with
          | Unresolved -> None
          | Core signature -> Some (Pretty.pretty_print_signature ~syntax signature)
          | Resolved signature ->
            Some
              (Pretty.pretty_print_signature ~syntax
              @@ Checking.untype_signature signature)
        with
        | `Ok pretty -> Some pretty
        | `Nonpretty _ -> None
      in
      let kind =
        match syntax, mdef_type with
        | CameLIGO, Module -> SymbolKind.Module
        | CameLIGO, Signature -> Interface
        | JsLIGO, Module -> Namespace
        | JsLIGO, Signature -> Interface
      in
      detail, kind, name, body_range, range
  in
  let%map.Option selectionRange = Range.of_loc range in
  (* If the definition is a signature item, then it won't have a body range. *)
  let range =
    Option.value ~default:selectionRange @@ Option.bind ~f:Range.of_loc body_range
  in
  (* The selection range must be contained in the range, so let's extend the range since
     it doesn't in scopes. *)
  let range = Range.cover selectionRange range in
  let children = get_all_symbols_hierarchies syntax defs in
  DocumentSymbol.create ?children ?detail ~kind ~name ~range ~selectionRange ()


and get_all_symbols_hierarchies
    : Syntax_types.t -> Def.t Rose.forest -> DocumentSymbol.t list option
  =
 fun syntax defs ->
  match List.filter_map defs ~f:(get_all_symbols_hierarchy syntax) with
  | [] -> None
  | _ :: _ as defs -> Some defs


let on_req_document_symbol (path : Path.t)
    : [> `DocumentSymbol of DocumentSymbol.t list ] option Handler.t
  =
  with_cached_doc_pure path ~default:None
  @@ fun { syntax; code = _; definitions } ->
  let definitions =
    List.filter definitions ~f:(fun def ->
        match Scopes.Types.get_range def with
        | File reg -> Path.equal path (Path.from_absolute reg#file)
        | Virtual _ -> false)
  in
  let hierarchy = create_hierarchy definitions in
  Format.eprintf
    "hierarchy: %a\n%!"
    (Rose.pp_forest (fun ppf def ->
         let open Scopes.Types in
         Format.fprintf
           ppf
           "%s#%a"
           (get_def_name def)
           Loc.pp
           (Option.value ~default:(get_range def) (get_body_range def))))
    hierarchy;
  Option.map (get_all_symbols_hierarchies syntax hierarchy) ~f:(fun symbols ->
      `DocumentSymbol symbols)
