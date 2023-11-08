open Handler
open Lsp_helpers

let hovers_pp_mode : Pretty.pp_mode =
  { width = Helpers_pretty.default_line_width_for_hovers; indent = 2 }


let hover_string
    :  Syntax_types.t -> Scopes.def
    -> [> `MarkupContent of MarkupContent.t | `List of MarkedString.t list ] Handler.t
  =
 fun syntax ->
  let print_type_with_prefix ?prefix t =
    match Pretty.pretty_print_type_expression hovers_pp_mode ~syntax ?prefix t with
    | `Ok str -> return str
    | `Nonpretty (err, nonpretty_type) ->
      let@ () =
        send_log_msg ~type_:Error
        @@
        match err with
        | `Exn exn -> "pretty_print_type_expression: exception: " ^ Exn.to_string exn
        | `PassesError e ->
          "pretty_print_type_expression: passes error: "
          ^ Helpers_pretty.passes_error_to_string e
      in
      return nonpretty_type
  in
  let language = Some (Syntax.to_string syntax) in
  function
  | Variable vdef ->
    let prefix = PPrint.(string vdef.name ^//^ colon) in
    let type_info = Type_definition.get_type vdef in
    let@ value =
      Option.value_map
        ~default:(return @@ Helpers_pretty.unresolved_type_as_comment syntax)
        ~f:(print_type_with_prefix ~prefix <@ Type_definition.use_var_name_if_availiable)
        type_info
    in
    return @@ `List [ MarkedString.{ language; value } ]
  | Type tdef ->
    let rec get_params (t : Ast_core.type_content) =
      match t with
      | T_abstraction Ligo_prim.Abstraction.{ ty_binder; kind = _; type_ } ->
        ty_binder :: get_params type_.type_content
      | _ -> []
    in
    (* Like ['a x] or [x<a>] if there are params, or just [x] otherwise *)
    let@ name_with_params =
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
      return @@ `List [ MarkedString.{ language; value } ]
    | Some content ->
      let prefix = PPrint.(string "type" ^//^ string name_with_params ^//^ equals) in
      let@ value = print_type_with_prefix ~prefix content in
      return @@ `List [ MarkedString.{ language; value } ])
  | Module mdef ->
    let rec strip_generated : Ast_core.signature -> Ast_core.signature =
      let open Ligo_prim in
      fun { items } ->
        let strip_item : Ast_core.sig_item -> Ast_core.sig_item option = function
          | S_value (v, _, _) when Value_var.is_generated v -> None
          | (S_type (v, _) | S_type_var v) when Type_var.is_generated v -> None
          | (S_value _ | S_type _ | S_type_var _) as sig_item -> Some sig_item
          | (S_module (v, _) | S_module_type (v, _)) when Module_var.is_generated v ->
            None
          | S_module (v, signature) -> Some (S_module (v, strip_generated signature))
          | S_module_type (v, signature) ->
            Some (S_module_type (v, strip_generated signature))
          | S_include ({ wrap_content = S_sig signature; location = _ } as sig_expr) ->
            Some
              (S_include
                 { sig_expr with wrap_content = S_sig (strip_generated signature) })
          | S_include { wrap_content = S_path _; location = _ } as s_include ->
            Some s_include
        in
        { items = List.filter_map ~f:strip_item items }
    in
    let core_sig =
      match mdef.signature with
      | Core core_sig -> Some (strip_generated core_sig)
      | Resolved signature ->
        Some (strip_generated @@ Checking.untype_signature ~use_orig_var:true signature)
      | Unresolved -> None
    in
    let print_signature sig_ =
      match Pretty.pretty_print_signature ~syntax sig_ with
      | `Ok str -> return str
      | `Nonpretty (err, nonpretty_sig) ->
        let@ () =
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
    let@ sig_str =
      Option.value_map
        ~default:(return @@ Helpers_pretty.unresolved_type_as_comment syntax)
        ~f:print_signature
        core_sig
    in
    let@ project_root = ask_last_project_file in
    return @@ Helpers_pretty.print_module ~project_root:!project_root syntax sig_str mdef


let on_req_hover : Position.t -> Path.t -> Hover.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { definitions; syntax; _ } ->
  when_some' (Go_to_definition.get_definition pos file definitions)
  @@ fun definition ->
  let@ contents = hover_string syntax definition in
  let hover = Hover.create ~contents () in
  return (Some hover)
