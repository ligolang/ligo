open Ast_typed.Types
open Trace

type contract_pass_data = {
  contract_type : Helpers.contract_type ;
  main_name : string ;
}

let annotation_or_label annot label = String.capitalize_ascii (Option.value ~default:label (Ast_typed.Helpers.remove_empty_annotation annot))

let check_entrypoint_annotation_format ~raise ep (exp: expression) =
  match String.split_on_char '%' ep with
    | [ "" ; ep'] ->
      let cap = String.capitalize_ascii ep' in
      if String.equal cap ep' then raise.raise @@ Errors.bad_format_entrypoint_ann ep exp.location
      else cap
    | _ -> raise.raise @@ Errors.bad_format_entrypoint_ann ep exp.location 


let self_typing ~raise : contract_pass_data -> expression -> bool * contract_pass_data * expression = fun dat e ->
  let bad_self_err () = Errors.bad_self_type
    e.type_expression
    {e.type_expression with
      type_content =
        T_constant {
          language=Stage_common.Backends.michelson;
          injection=Ligo_string.verbatim Stage_common.Constant.contract_name;
          parameters=[dat.contract_type.parameter]
        }
    }
    e.location
  in
  match e.expression_content , e.type_expression with
  | (E_constant {cons_name=C_SELF ; arguments=[entrypoint_exp]} , {type_content = T_constant {language=_;injection;parameters=[t]} ; _}) when String.equal (Ligo_string.extract injection) Stage_common.Constant.contract_name ->
    let entrypoint =
      match entrypoint_exp.expression_content with
      | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
      | _ -> raise.raise @@ Errors.entrypoint_annotation_not_literal entrypoint_exp.location
    in
    let entrypoint_t =
      match dat.contract_type.parameter.type_content with
      | (T_sum _ as t) when String.equal "Default" entrypoint -> {dat.contract_type.parameter with type_content = t}
      | T_sum cmap ->
        let content = LMap.to_kv_list cmap.content in
        let content = List.map ~f:(fun (Label entrypoint, {michelson_annotation;associated_type;_}) ->
                          (annotation_or_label michelson_annotation entrypoint, associated_type)) content in
        let associated_type = trace_option ~raise (Errors.unmatched_entrypoint entrypoint_exp.location) @@
          List.Assoc.find content ~equal:String.equal entrypoint
        in
        associated_type
      | t -> {dat.contract_type.parameter with type_content = t}
    in
    let () =
      trace_option ~raise (bad_self_err ()) @@
      Ast_typed.assert_type_expression_eq (entrypoint_t , t) in
    (true, dat, e)
  | _ -> (true,dat,e)

let entrypoint_typing ~raise : contract_pass_data -> expression -> bool * contract_pass_data * expression = fun dat e ->
  match e.expression_content with
  | E_constant {cons_name=C_CONTRACT_ENTRYPOINT_OPT|C_CONTRACT_ENTRYPOINT ; arguments=[entrypoint_exp;_]} ->
    let _ = match entrypoint_exp.expression_content with
     | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ~raise (Ligo_string.extract ep) entrypoint_exp
     | _ -> raise.raise @@ Errors.entrypoint_annotation_not_literal entrypoint_exp.location
    in
    (true, dat, e)
  | _ -> (true,dat,e)

let remove_unused ~raise : string -> module_fully_typed -> module_fully_typed = fun main_name prg ->
  let get_fv expr = List.map ~f:(fun v -> v.Location.wrap_content) @@ Helpers.Free_variables.expression expr in
  let get_fmv_expr expr = Helpers.Free_module_variables.expression expr in
  let get_fmv_mod module' = Helpers.Free_module_variables.module' module' in
  let Module_Fully_Typed module' = prg in
  let aux = function
      {Location.wrap_content = Declaration_constant {name = Some name;  _}; _} -> not (String.equal name main_name)
    | _ -> true in
  let prg_decls = List.rev module' in
  let _, prg_decls = List.split_while prg_decls ~f:aux in
  let main_decl, prg_decls = trace_option ~raise (Errors.corner_case "Entrypoint not found") @@ List.uncons prg_decls in
  let main_expr = trace_option ~raise (Errors.corner_case "Entrypoint not found") @@ match main_decl with
      {Location.wrap_content = Declaration_constant {expr; _}; _} -> Some expr
    | _ -> None in
  let rec aux (fv, fmv) acc = function
    | [] -> acc
    | {Location.wrap_content = Declaration_constant {binder; expr; _}; _} as hd :: tl ->
       let binder = binder.wrap_content in
       if List.mem fv binder ~equal:Var.equal then
         let expr_fv = get_fv expr in
         let fv = List.remove_element ~compare:Var.compare binder fv in
         let fv = List.dedup_and_sort ~compare:Var.compare (fv @ expr_fv) in
         aux (fv, fmv) (hd :: acc) tl
       else
         aux (fv, fmv) acc tl
    | {Location.wrap_content = Declaration_module {module_binder = name; module_}; _} as hd :: tl ->
       if List.mem fmv name ~equal:equal_module_variable then
         let expr_fv = get_fmv_mod module_ in
         let fmv = List.remove_element ~compare:compare_module_variable name fmv in
         let fmv = List.dedup_and_sort ~compare:compare_module_variable (fmv @ expr_fv) in
         aux (fv, fmv) (hd :: acc) tl
       else
         aux (fv, fmv) acc tl
    | {Location.wrap_content = Module_alias {alias = name; binders}; _} as hd :: tl ->
       if List.mem fmv name ~equal:equal_module_variable then
         let main_module = List.Ne.hd binders in
         let fmv = List.remove_element ~compare:compare_module_variable name fmv in
         let fmv = List.dedup_and_sort ~compare:compare_module_variable (main_module :: fmv) in
         aux (fv, fmv) (hd :: acc) tl
       else
         aux (fv, fmv) acc tl
    | hd :: tl ->
       aux (fv, fmv) (hd :: acc) tl in
  Module_Fully_Typed (aux (get_fv main_expr, get_fmv_expr main_expr) [main_decl] prg_decls)
