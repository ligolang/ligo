open Ast_aggregated.Types
open Simple_utils.Trace
module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim

type contract_type =
  { parameter : Ast_aggregated.type_expression
  ; storage : Ast_aggregated.type_expression
  }

let annotation_or_label layout label =
  Option.value ~default:(String.uncapitalize @@ Label.to_string label)
  @@ Ast_typed.Helpers.remove_empty_annotation (Layout.annot layout label)


let check_entrypoint_annotation_format ~raise ep (exp : expression) =
  let allowed_annot_char c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9' -> true
    | _ -> false
  in
  match String.split ~on:'%' ep with
  | [ ""; ep' ] when String.for_all ~f:allowed_annot_char ep' -> ep'
  | _ -> raise.error @@ Errors.bad_format_entrypoint_ann ep exp.location


let find_annot_type layout content entrypoint =
  let content =
    List.map
      ~f:(fun (entrypoint, associated_type) ->
        annotation_or_label layout entrypoint, associated_type)
      content
  in
  List.Assoc.find content ~equal:String.equal entrypoint


let self_typing ~raise : contract_type -> expression -> bool * contract_type * expression =
 fun dat e ->
  let bad_self_err t =
    Main_warnings.warn_bad_self_type
      t
      { e.type_expression with
        type_content =
          T_constant
            { language = Backend.Michelson.name
            ; injection = Ligo_prim.Literal_types.Contract
            ; parameters = [ dat.parameter ]
            }
      }
      e.location
  in
  let loc = e.location in
  match e.expression_content, Ast_aggregated.get_t_option e.type_expression with
  | E_constant { cons_name = C_CHECK_SELF; arguments = [ entrypoint_exp ] }, Some t ->
    let entrypoint =
      match entrypoint_exp.expression_content with
      | E_literal (Literal_string ep) ->
        check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
      | _ -> raise.error @@ Errors.entrypoint_ann_not_literal e.location
    in
    let entrypoint_t =
      match dat.parameter.type_content with
      | T_sum (cmap, _) as t when String.equal "default" (String.uncapitalize entrypoint)
        ->
        let t =
          match Record.to_list cmap.fields with
          | [ (_single_entry, t) ] -> t.type_content
          | _ -> t
        in
        { dat.parameter with type_content = t }
      | T_sum (cmap, _) ->
        let content, layout =
          trace_option ~raise (Errors.unmatched_entrypoint entrypoint_exp.location)
          @@
          match Record.to_list cmap.fields with
          | [ (_single_entry, t) ] ->
            (* to handle single entrypoints:
              parameter type for single entry-point contracts such as
              `[@entry] let main (p:p) (s:s) = ...`
              are now compiled to `| Main of p`
              This representation do not yet persist up until the michelson representation
              due to "optimisations" :  `| Main of p` compiles to `p` *)
            let open Simple_utils.Option in
            let* row, _ = Ast_aggregated.get_t_sum t in
            Some (Record.to_list row.fields, row.layout)
          | x -> Some (x, cmap.layout)
        in
        let associated_type =
          trace_option ~raise (Errors.unmatched_entrypoint entrypoint_exp.location)
          @@ find_annot_type layout content entrypoint
        in
        associated_type
      | t -> { dat.parameter with type_content = t }
    in
    let () =
      if not @@ Ast_aggregated.equal_type_expression entrypoint_t t
      then
        raise.Simple_utils.Trace.warning
        @@ bad_self_err Ast_aggregated.(t_contract ~loc t)
    in
    let e = Ast_aggregated.e_a_none ~loc:e.location e.type_expression in
    true, dat, e
  | _ -> true, dat, e


let self_literal_typing ~raise : expression -> expression =
 fun e ->
  match e.expression_content with
  | E_constant { cons_name = C_CHECK_SELF; arguments = [ entrypoint_exp ] } ->
    let _ =
      match entrypoint_exp.expression_content with
      | E_literal (Literal_string ep) ->
        check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
      | _ -> raise.error @@ Errors.entrypoint_ann_not_literal entrypoint_exp.location
    in
    e
  | _ -> e


let entrypoint_typing ~raise : expression -> expression =
 fun e ->
  match e.expression_content with
  | E_constant { cons_name = C_CHECK_ENTRYPOINT; arguments = [ entrypoint_exp ] } ->
    let _ =
      match entrypoint_exp.expression_content with
      | E_literal (Literal_string ep) ->
        check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
      | _ -> raise.error @@ Errors.entrypoint_ann_not_literal entrypoint_exp.location
    in
    Ast_aggregated.e_a_unit ~loc:e.location ()
  | _ -> e


let emit_event_typing ~raise : expression -> expression =
 fun e ->
  match e.expression_content with
  | E_constant { cons_name = C_CHECK_EMIT_EVENT; arguments = tag :: _ } ->
    let (_ : string) =
      match tag.expression_content with
      | E_literal (Literal_string ep) ->
        check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) tag
      | _ -> raise.error @@ Errors.emit_tag_not_literal tag.location
    in
    Ast_aggregated.e_a_unit ~loc:e.location ()
  | _ -> e


let litstr_check ~raise : expression -> expression =
 fun e ->
  match e.expression_content with
  | E_constant { cons_name = C_CHECK_CALL_VIEW_LITSTR; arguments = [ litstr ] } ->
    let (_ : string) =
      match litstr.expression_content with
      | E_literal (Literal_string ep) -> Ligo_string.extract ep
      | _ -> raise.error @@ Errors.call_view_not_litstr litstr.location
    in
    Ast_aggregated.e_a_unit ~loc:e.location ()
  | _ -> e
