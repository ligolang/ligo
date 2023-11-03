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
    (* Like ['a x] or [x<a>] if there are params, or just [x] othervise *)
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
    let@ project_root = ask_last_project_file in
    return @@ Helpers_pretty.print_module ~project_root:!project_root syntax mdef


let on_req_hover : Position.t -> Path.t -> Hover.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { definitions; syntax; _ } ->
  when_some' (Go_to_definition.get_definition pos file definitions)
  @@ fun definition ->
  let@ contents = hover_string syntax definition in
  let hover = Hover.create ~contents () in
  return (Some hover)
