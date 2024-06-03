open Handler
open Lsp_helpers

(** A default [pp_mode] that looks good with hovers. *)
let hovers_pp_mode : Pretty.pp_mode =
  { width = Helpers_pretty.default_line_width_for_hovers; indent = 2 }


(** Replaces [T_variable]s in the provided core type for [T_module_accessor]s using the
    module path at the given position. The module path is resolved if it's an alias. *)
let insert_module_path
    ~(normalize : Path.normalization)
    (input_d : Completion_lib.Common.input_d)
    (type' : Ast_core.type_expression)
    : Ast_core.type_expression
  =
  (* Find the the module access of this hovered identifier, if any. *)
  let module_path_opt =
    match%bind.Option
      let open Completion_lib.Fields in
      match input_d.cst with
      | CameLIGO cst -> get_linearized_path (module C_CameLIGO) { input_d with cst }
      | JsLIGO cst -> get_linearized_path (module C_JsLIGO) { input_d with cst }
    with
    | Projection (_struct_pos, _proj_fields_before_cursor) -> None
    | Module (_def_scope, module_names_before_cursor) ->
      Option.some_if
        (not (List.is_empty module_names_before_cursor))
        module_names_before_cursor
  in
  Option.value_map module_path_opt ~default:type' ~f:(fun module_path ->
      let open Ligo_prim in
      let mdefs =
        Def.filter_map input_d.definitions ~f:(function
            | Variable _ | Type _ | Label _ -> None
            | Module mdef -> Some mdef)
      in
      let rec resolve_alias (mdef : Scopes.Types.mdef) : Scopes.Uid.t option =
        match mdef.mod_case with
        | Def _ -> Some mdef.uid
        | Alias { resolve_mod_name = Unresolved_path _ } -> None
        | Alias
            { resolve_mod_name =
                Resolved_path
                  { module_path = _; resolved_module_path = _; resolved_module }
            } ->
          let%bind.Option mdef =
            List.find_map mdefs ~f:(fun mdef ->
                Option.some_if (Scopes.Uid.equal mdef.uid resolved_module) mdef)
          in
          resolve_alias mdef
      in
      let get_definition_of_uid (var : Scopes.Uid.t) : Def.t option =
        let%bind.Option region =
          match Scopes.Uid.to_location var with
          | Virtual _ -> None
          | File reg -> Some reg
        in
        let path = normalize region#file in
        let pos = Position.of_pos region#start in
        Def.get_definition ~normalize pos path input_d.definitions
      in
      let get_module_uid (module_path : Scopes.Uid.t list) : Scopes.Uid.t option =
        let%bind.Option module_name = List.last module_path in
        match%bind.Option get_definition_of_uid module_name with
        | Variable _ | Type _ | Label _ -> None
        | Module mdef -> resolve_alias mdef
      in
      let module_path =
        List.map module_path ~f:(fun m ->
            Module_var.of_input_var m#payload ~loc:(File m#region))
      in
      (* [module_path] might be an alias, so we need to resolve it. *)
      let module_path_uid = List.map module_path ~f:Scopes.Types.mvar_to_id in
      match get_module_uid module_path_uid with
      | None -> type'
      | Some module_uid ->
        Scopes.Misc.map_core_type_content_in_type_expression
          (function
            | T_variable tvar as t
              when not
                     (Type_var.is_generated tvar
                     || Loc.is_virtual (Type_var.get_location tvar)) ->
              let tvar_uid =
                Scopes.Uid.make (Type_var.to_name_exn tvar) (Type_var.get_location tvar)
              in
              (match
                 Option.bind
                   (get_definition_of_uid tvar_uid)
                   ~f:(get_module_uid <@ Scopes.Types.get_mod_path)
               with
              | None -> t
              | Some type_module_uid ->
                if Scopes.Uid.equal type_module_uid module_uid
                then T_module_accessor { module_path; element = tvar }
                else t)
            | t -> t)
          type')


(** Retuns the hovers for some definition. That is, its prettified type/signature and all
    documentation comments attached to it. It's possible that it won't be possible to
    pretty-print it, in which case a non-pretty version will be shown instead. *)
let hover_string
    :  Completion_lib.Common.input_d -> Scopes.def
    -> [> `List of MarkedString.t list ] Handler.t
  =
 fun input_d def ->
  let open Handler.Let_syntax in
  let syntax = Dialect_cst.to_syntax_type input_d.cst in
  let handle_pretty_print_result (name : string) = function
    | `Ok str -> return str
    | `Nonpretty (err, nonpretty_result) ->
      let%bind () =
        send_log_msg ~type_:Error
        @@
        match err with
        | `Exn exn -> Format.asprintf "%s: exception: %a" name Exn.pp exn
        | `PassesError e ->
          Format.asprintf
            "%s: passes error: %a"
            name
            (Nanopasses.Errors.error_ppformat
               ~display_format:Human_readable
               ~no_colour:false)
            e
      in
      return nonpretty_result
  in
  let print_type_with_prefix ?prefix t =
    handle_pretty_print_result
      "pretty_print_type_expression"
      (Pretty.pretty_print_type_expression hovers_pp_mode ~syntax ?prefix t)
  in
  let print_variant label te =
    handle_pretty_print_result
      "print_variant"
      (Pretty.pretty_print_variant hovers_pp_mode ~syntax (label, te))
  in
  let language = Some (Syntax.to_string syntax) in
  let%bind normalize = ask_normalize in
  let doc_comment_hovers =
    let comments = Def.get_comments def in
    let doc_comments =
      List.filter
        ~f:(Ligo_docs.Comments.is_doc_comment ~output_syntax:syntax)
        (* Comments like [(** *)] in CameLIGO are considered as documentation comments *)
        comments
    in
    let source_syntax =
      Option.value ~default:syntax
      @@
      match Def.get_location ~normalize def with
      | File { path; _ } -> Path.get_syntax path
      | StdLib _ -> Some CameLIGO (* Since Stdlib is written in CameLIGO *)
      | Virtual _ -> None
    in
    List.map
      ~f:(fun str ->
        MarkedString.
          { language = None
          ; value = Ligo_docs.Comments.format_doc_comment ~source_syntax str
          })
      doc_comments
  in
  match def with
  | Variable vdef ->
    let prefix = PPrint.(string vdef.name ^//^ colon) in
    let type_info = Def.get_type ~use_module_accessor:true vdef in
    let%bind value =
      Option.value_map
        ~default:(return @@ Helpers_pretty.unresolved_type_as_comment syntax)
        ~f:
          (print_type_with_prefix ~prefix
          <@ insert_module_path ~normalize input_d
          <@ Def.use_var_name_if_available)
        type_info
    in
    return @@ `List (MarkedString.{ language; value } :: doc_comment_hovers)
  | Type tdef ->
    let rec get_params : Ast_core.type_content -> Ligo_prim.Type_var.t list = function
      | T_abstraction Ligo_prim.Abstraction.{ ty_binder; kind = _; type_ } ->
        ty_binder :: get_params type_.type_content
      | _ -> []
    in
    (* Like ['a x] or [x<a>] if there are params, or just [x] otherwise *)
    let%bind name_with_params =
      match
        Option.value_map tdef.content ~default:[] ~f:(fun content ->
            get_params content.type_content)
      with
      | [] -> return tdef.name
      | params ->
        print_type_with_prefix
        @@ Ast_core.Combinators.t_app
             ~loc:Loc.dummy
             { type_operator =
                 Ligo_prim.Module_access.make_el
                 @@ Ligo_prim.Type_var.of_input_var ~loc:Loc.dummy tdef.name
             ; arguments =
                 List.map
                   ~f:(fun var -> Ast_core.Combinators.t_variable var ~loc:Loc.dummy ())
                   params
             }
             ()
    in
    (match tdef.content with
    | None ->
      let value =
        Helpers_pretty.doc_to_string
          ~width:10000
          PPrint.(string "type" ^//^ string name_with_params)
      in
      return @@ `List (MarkedString.{ language; value } :: doc_comment_hovers)
    | Some content ->
      let prefix = PPrint.(string "type" ^//^ string name_with_params ^//^ equals) in
      let%bind value =
        print_type_with_prefix ~prefix @@ insert_module_path ~normalize input_d content
      in
      return @@ `List (MarkedString.{ language; value } :: doc_comment_hovers))
  | Label ldef ->
    (match ldef.label_case with
    | Ctor ->
      let open Ligo_prim in
      let label : Label.t =
        let open Scopes.Types in
        Label (Uid.to_name ldef.uid, Uid.to_location ldef.uid)
      in
      let%bind value = print_variant label ldef.content in
      return @@ `List (MarkedString.{ language; value } :: doc_comment_hovers)
    | Field ->
      let prefix = PPrint.(string ldef.name ^//^ colon) in
      let%bind value = print_type_with_prefix ~prefix ldef.content in
      return @@ `List (MarkedString.{ language; value } :: doc_comment_hovers))
  | Module mdef ->
    let rec strip_generated : Ast_core.signature -> Ast_core.signature =
      let open Ligo_prim in
      fun { items } ->
        let strip_item : Ast_core.sig_item -> Ast_core.sig_item option =
         fun sig_item ->
          let loc = Loc.get_location sig_item in
          match Loc.unwrap sig_item with
          | S_value (v, _, _) when Value_var.is_generated v -> None
          | (S_type (v, _, _) | S_type_var (v, _)) when Type_var.is_generated v -> None
          | S_value _ | S_type _ | S_type_var _ -> Some sig_item
          | (S_module (v, _) | S_module_type (v, _)) when Module_var.is_generated v ->
            None
          | S_module (v, signature) ->
            Some (Loc.wrap ~loc @@ Ast_core.S_module (v, strip_generated signature))
          | S_module_type (v, signature) ->
            Some (Loc.wrap ~loc @@ Ast_core.S_module_type (v, strip_generated signature))
          | S_include ({ wrap_content = S_sig signature; location = _ } as sig_expr) ->
            Some
              (Loc.wrap ~loc
              @@ Ast_core.S_include
                   { sig_expr with wrap_content = S_sig (strip_generated signature) })
          | S_include { wrap_content = S_path _; location = _ } -> Some sig_item
        in
        { items = List.filter_map ~f:strip_item items }
    in
    let drop_common_module_path =
      let prefix = List.map (mdef.mod_path @ [ mdef.uid ]) ~f:Scopes.Types.id_to_mvar in
      Scopes.Misc.map_core_signature_module_path
        (Simple_utils.List.drop_common_prefix ~equal:Ligo_prim.Module_var.equal ~prefix)
        Fn.id
    in
    let core_sig =
      match mdef.signature with
      | Core core_sig -> Some (drop_common_module_path @@ strip_generated core_sig)
      | Resolved signature ->
        let%map.Option sig' =
          try
            Simple_utils.Trace.to_option ~fast_fail:false
            @@ Checking.untype_signature ~use_orig_var:true signature
          with
          | _exn -> None
        in
        drop_common_module_path @@ strip_generated sig'
      | Unresolved -> None
    in
    let print_signature sig_ =
      match Pretty.pretty_print_signature ~syntax sig_ with
      | `Ok str -> return str
      | `Nonpretty (err, nonpretty_sig) ->
        let%bind () =
          send_log_msg ~type_:Error
          @@
          match err with
          | `Exn exn -> "pretty_print_signature: exception: " ^ Exn.to_string exn
          | `PassesError e ->
            "pretty_print_signature: passes error: "
            ^ Helpers_pretty.passes_error_to_string e
        in
        return nonpretty_sig
    in
    let%bind sig_str =
      Option.value_map
        ~default:(return @@ Helpers_pretty.unresolved_type_as_comment syntax)
        ~f:print_signature
        core_sig
    in
    let%bind project_root = ask_last_project_dir >>| fun dir -> !dir in
    let printed_module = Helpers_pretty.print_module ~project_root syntax sig_str mdef in
    return @@ `List (printed_module :: doc_comment_hovers)


(** Runs the handler for the hover request. This is normally called when the user's mouse
    hovers over a symbol. *)
let on_req_hover : Position.t -> Path.t -> Hover.t option Handler.t =
 fun pos file ->
  let open Handler.Let_syntax in
  with_cached_doc file ~default:None
  @@ fun { definitions; syntax; _ } ->
  let%bind normalize = ask_normalize in
  when_some' (Def.get_definition ~normalize pos file definitions)
  @@ fun definition ->
  with_cst file ~default:None
  @@ fun cst ->
  let input =
    Completion_lib.Common.mk_input_d ~cst ~syntax ~path:file ~definitions ~pos
  in
  let%bind (`List strings) = hover_string input definition in
  let replace =
    Parsing_shared.Errors.ErrorWrapper.replace_with
      (Helpers_pretty.unresolved_type_as_comment syntax)
  in
  let contents =
    `List (List.map ~f:(fun str -> { str with value = replace str.value }) strings)
  in
  let hover = Hover.create ~contents () in
  return (Some hover)
