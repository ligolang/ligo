open Ast_aggregated.Types
open Simple_utils.Trace
module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim

type contract_type =
  { parameter : Ast_aggregated.type_expression
  ; storage : Ast_aggregated.type_expression
  }

let annotation_or_label annot label =
  Option.value ~default:(String.uncapitalize @@ Label.to_string label)
  @@ Ast_typed.Helpers.remove_empty_annotation annot


let check_entrypoint_annotation_format ~raise ep (exp : expression) =
  let allowed_annot_char c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9' -> true
    | _ -> false
  in
  match String.split ~on:'%' ep with
  | [ ""; ep' ] when String.for_all ~f:allowed_annot_char ep' -> ep'
  | _ -> raise.error @@ Errors.bad_format_entrypoint_ann ep exp.location


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
      | T_sum _ as t when String.equal "default" (String.uncapitalize entrypoint) ->
        { dat.parameter with type_content = t }
      | T_sum cmap ->
        let content = Record.LMap.to_kv_list cmap.fields in
        let content =
          List.map
            ~f:(fun (entrypoint, { michelson_annotation; associated_type; _ }) ->
              annotation_or_label michelson_annotation entrypoint, associated_type)
            content
        in
        let associated_type =
          trace_option ~raise (Errors.unmatched_entrypoint entrypoint_exp.location)
          @@ List.Assoc.find content ~equal:String.equal entrypoint
        in
        associated_type
      | t -> { dat.parameter with type_content = t }
    in
    let () =
      if not @@ Ast_aggregated.equal_type_expression entrypoint_t t
      then raise.Simple_utils.Trace.warning @@ bad_self_err Ast_aggregated.(t_contract t)
    in
    let e = Ast_aggregated.e_a_none ~location:e.location e.type_expression in
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
    Ast_aggregated.e_a_unit ()
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
    Ast_aggregated.e_a_unit ()
  | _ -> e
