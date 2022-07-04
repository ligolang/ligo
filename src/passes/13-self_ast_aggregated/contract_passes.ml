open Ast_aggregated.Types
open Simple_utils.Trace
module Ligo_string = Simple_utils.Ligo_string

type contract_type = {
  parameter : Ast_aggregated.type_expression ;
  storage : Ast_aggregated.type_expression ;
}

let annotation_or_label annot label = String.capitalize (Option.value ~default:label (Ast_typed.Helpers.remove_empty_annotation annot))

let check_entrypoint_annotation_format ~raise ep (exp: expression) =
  match String.split ~on:'%' ep with
    | [ "" ; ep'] ->
      let cap = String.capitalize ep' in
      if String.equal cap ep' then raise.raise @@ Errors.bad_format_entrypoint_ann ep exp.location
      else cap
    | _ -> raise.raise @@ Errors.bad_format_entrypoint_ann ep exp.location


let self_typing ~raise : contract_type -> expression -> bool * contract_type * expression = fun dat e ->
  let bad_self_err () = Errors.bad_self_type
    e.type_expression
    {e.type_expression with
      type_content =
        T_constant {
          language=Stage_common.Backends.michelson;
          injection=Stage_common.Constant.Contract;
          parameters=[dat.parameter]
        }
    }
    e.location
  in
  match e.expression_content , e.type_expression with
  | (E_constant {cons_name=C_SELF ; arguments=[entrypoint_exp]} , {type_content = T_constant {language=_;injection=Stage_common.Constant.Contract;parameters=[t]} ; _}) ->
    let entrypoint =
      match entrypoint_exp.expression_content with
      | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
      | _ -> raise.raise @@ Errors.entrypoint_ann_not_literal entrypoint_exp.location
    in
    let entrypoint_t =
      match dat.parameter.type_content with
      | (T_sum _ as t) when String.equal "Default" entrypoint -> {dat.parameter with type_content = t}
      | T_sum cmap ->
        let content = LMap.to_kv_list cmap.content in
        let content = List.map ~f:(fun (Label entrypoint, {michelson_annotation;associated_type;_}) ->
                          (annotation_or_label michelson_annotation entrypoint, associated_type)) content in
        let associated_type = trace_option ~raise (Errors.unmatched_entrypoint entrypoint_exp.location) @@
          List.Assoc.find content ~equal:String.equal entrypoint
        in
        associated_type
      | t -> {dat.parameter with type_content = t}
    in
    let () =
      trace_option ~raise (bad_self_err ()) @@
      Ast_aggregated.Misc.assert_type_expression_eq (entrypoint_t , t) in
    (true, dat, e)
  | _ -> (true,dat,e)

let entrypoint_typing ~raise : contract_type -> expression -> bool * contract_type * expression = fun dat e ->
  match e.expression_content with
  | E_constant {cons_name=C_CONTRACT_ENTRYPOINT_OPT|C_CONTRACT_ENTRYPOINT ; arguments=[entrypoint_exp;_]} ->
    let _ = match entrypoint_exp.expression_content with
     | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
     | _ -> raise.raise @@ Errors.entrypoint_ann_not_literal entrypoint_exp.location
    in
    (true, dat, e)
  | _ -> (true,dat,e)

