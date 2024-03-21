open Handler
open Lsp_helpers

let guard_ghost (input : string) : string option =
  Option.some_if (not @@ Parsing.Errors.ErrorWrapper.is_wrapped input) input


let make_def_info (syntax : Syntax_types.t) (def : Def.t)
    : (string option * SymbolKind.t * string * Range.t * Range.t) option
  =
  let detail, kind, name, decl_range, range =
    match def with
    | Variable ({ name; range; decl_range; def_type; _ } as vdef) ->
      let type_info = Def.get_type ~use_module_accessor:true vdef in
      let detail =
        Option.map
          type_info
          ~f:
            (Pretty.show_type ~syntax ~doc_to_string:Helpers_pretty.doc_to_string
            <@ Def.use_var_name_if_available)
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
          | T_sum _ -> EnumMember
          | T_record _ -> Field
          | T_arrow _ -> if is_field then Method else Function
          | _ -> Variable)
      in
      detail, kind, name, decl_range, range
    | Type { name; range; decl_range; content; _ } ->
      let detail =
        Option.map
          content
          ~f:(Pretty.show_type ~doc_to_string:Helpers_pretty.doc_to_string ~syntax)
      in
      let kind =
        match content with
        | None -> SymbolKind.TypeParameter
        | Some { type_content; location = _ } ->
          (match type_content with
          | T_sum _ -> Enum
          | _ -> Struct)
      in
      detail, kind, name, decl_range, range
    | Module { name; range; decl_range; mdef_type; signature; _ } ->
      let detail =
        match%bind.Option
          match signature with
          | Unresolved -> None
          | Core signature -> Some (Pretty.pretty_print_signature ~syntax signature)
          | Resolved signature ->
            let%map.Option sig' =
              try
                Simple_utils.Trace.to_option ~fast_fail:false
                @@ Checking.untype_signature signature
              with
              | _exn -> None
            in
            Pretty.pretty_print_signature ~syntax sig'
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
      detail, kind, name, decl_range, range
    | Label { name; decl_range; range; content; label_case; _ } ->
      let detail =
        (* [Option.is_some] isn't used here because it forces PP calculation. *)
        if not (Loc.is_dummy_or_generated content.location)
        then Option.some @@ Pretty.show_type ~syntax content
        else None
      in
      let kind =
        match label_case with
        | Ctor -> SymbolKind.EnumMember
        | Field -> Field
      in
      detail, kind, name, decl_range, range
  in
  (* Let's show a ? in case this was generated by error recovery so we can still have a
     node to show children. *)
  let name = Option.value ~default:"?" @@ guard_ghost name in
  let%bind.Option selectionRange = Range.of_loc range in
  let%bind.Option range = Range.of_loc decl_range in
  let%map.Option () = Option.some_if (Range.inside ~small:selectionRange ~big:range) () in
  let detail = Option.bind ~f:guard_ghost detail in
  detail, kind, name, selectionRange, range


let rec get_all_symbols_hierarchy
    : Syntax_types.t -> Def.t Rose.tree -> DocumentSymbol.t option
  =
 fun syntax (Tree ((hd, tl), defs)) ->
  let children = get_all_symbols_hierarchies syntax defs in
  match
    if List.is_empty tl
    then (
      let%map.Option detail, kind, name, selectionRange, range =
        make_def_info syntax hd
      in
      children, detail, kind, name, selectionRange, range)
    else (
      (* TODO #2129: we should use constructor info for this, but we don't have them in scopes
         yet, so we use a placeholder for now. Note that this also assumes a single
         constructor being destructured, but there may be many of them. *)
      let%bind.Option selectionRange =
        Range.of_loc
        @@ List.fold tl ~init:(Scopes.Types.get_range hd) ~f:(fun acc def ->
               Loc.cover acc @@ Scopes.Types.get_range def)
      in
      let%bind.Option range = Range.of_loc @@ Scopes.Types.get_decl_range hd in
      let%map.Option () =
        Option.some_if (Range.inside ~small:selectionRange ~big:range) ()
      in
      let ctor_children = hd :: tl in
      let detail = None in
      let kind = SymbolKind.Constructor in
      let name =
        String.concat ~sep:", " @@ List.map ctor_children ~f:Scopes.Types.get_def_name
      in
      let ctor_children =
        match
          List.filter_map ctor_children ~f:(fun def ->
              let%map.Option detail, kind, name, selectionRange, range =
                make_def_info syntax def
              in
              DocumentSymbol.create
                ?children
                ?detail
                ~kind
                ~name
                ~range
                ~selectionRange
                ())
        with
        | [] -> None
        | _ :: _ as children -> Some children
      in
      let children =
        match ctor_children, children with
        | None, None -> None
        | None, nested_children_opt -> nested_children_opt
        | ctor_children_opt, (* already nested in ctor_children *) _ -> ctor_children_opt
      in
      children, detail, kind, name, selectionRange, range)
  with
  | None ->
    (* We couldn't create this node for some reason, but we can still show its children by
       creating a dummy node. *)
    (match children with
    | None | Some [] -> None
    | Some (child :: children) ->
      let kind = SymbolKind.Null in
      let name = "?" in
      let range =
        Option.value_map (List.last children) ~default:child.range ~f:(fun symbol ->
            Range.cover child.range symbol.range)
      in
      let selectionRange = range in
      Some (DocumentSymbol.create ~children ~kind ~name ~range ~selectionRange ()))
  | Some (children, detail, kind, name, selectionRange, range) ->
    Some (DocumentSymbol.create ?children ?detail ~kind ~name ~range ~selectionRange ())


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
  with_cached_doc path ~default:None
  @@ fun { syntax
         ; code = _
         ; definitions = _
         ; document_version = _
         ; hierarchy
         ; parse_error_ranges = _
         ; lambda_types = _
         ; potential_tzip16_storages = _
         } ->
  let@ () =
    send_debug_msg @@ Format.asprintf "On document symbol request on %a" Path.pp path
  in
  let hierarchy =
    Rose.filter_top_down (force hierarchy) ~f:(fun def ->
        match Scopes.Types.get_decl_range def with
        | File region -> Path.(equal (from_absolute region#file) path)
        | Virtual _ -> false)
  in
  return
  @@ Option.map (get_all_symbols_hierarchies syntax hierarchy) ~f:(fun symbols ->
         `DocumentSymbol symbols)
