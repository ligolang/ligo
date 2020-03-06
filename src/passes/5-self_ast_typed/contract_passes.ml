open Ast_typed
open Trace

type contract_pass_data = {
  contract_type : Helpers.contract_type ;
  main_name : string ;
}

module Errors = struct
  let corner_case loc message =
    let title () = "corner case" in
    let content () = "we don't have a good error message for this case. we are
striving find ways to better report them and find the use-cases that generate
them. please report this to the developers." in
    let data = [
      ("location" , fun () -> loc) ;
      ("message" , fun () -> message) ;
    ] in
    error ~data title content
  
  let bad_self_type expected got loc () =
    let title = thunk "bad self type" in
    let message () = Format.asprintf "expected %a but got %a" Ast_typed.PP.type_expression expected Ast_typed.PP.type_expression got in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()
  
  let bad_format_entrypoint_ann ep loc () =
    let title = thunk "bad entrypoint format" in
    let message () = Format.asprintf "entrypoint \"%s\" is badly formatted. We expect \"%%Bar\" for entrypoint Bar and \"%%Default\" when no entrypoint used" ep in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
      ("hint" , fun () -> Format.asprintf "try %%%s" ep)
    ] in
    error ~data title message ()

end

let check_entrypoint_annotation_format ep exp =
  match String.split_on_char '%' ep with
    | [ "" ; ep'] -> ok @@ ep'
    | _ -> fail @@ Errors.bad_format_entrypoint_ann ep exp.location 


let self_typing : contract_pass_data -> expression -> (bool * contract_pass_data * expression) result = fun dat e ->
  let bad_self_err () = Errors.bad_self_type
    e.type_expression 
    {e.type_expression with type_content = T_operator (TC_contract dat.contract_type.parameter)}
    e.location
  in
  match e.expression_content , e.type_expression with
  | E_constant {cons_name=C_SELF ; arguments=[entrypoint_exp]}, {type_content = T_operator (TC_contract t) ; type_meta=_} ->
    let%bind entrypoint = match entrypoint_exp.expression_content with
        | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ep entrypoint_exp
        | E_variable v -> (
          match Ast_typed.Environment.get_opt v e.environment with
          | Some {type_value = _; source_environment = _ ; definition = ED_declaration {expr ; free_variables = _}} -> (
            match expr.expression_content with
              | E_literal (Literal_string ep) -> check_entrypoint_annotation_format ep entrypoint_exp
              | _ -> fail @@ Errors.corner_case __LOC__ "SELF argument do not resolve to a string"
          )
          | _ -> fail @@ Errors.corner_case __LOC__ "SELD argument not found in the environment"
        )
        | _ -> fail @@ Errors.corner_case __LOC__ "SELF argument is not a string or a variable" in
    let%bind entrypoint_t = match dat.contract_type.parameter.type_content with
      | T_sum cmap -> trace_option (simple_error "No constructor matches the entrypoint annotation")
                      @@ Stage_common.Types.CMap.find_opt (Constructor entrypoint) cmap
      | t -> ok {dat.contract_type.parameter with type_content = t} in
    let%bind () =
      trace_strong (bad_self_err ()) @@
      Ast_typed.assert_type_expression_eq (entrypoint_t , t) in
    ok (true, dat, e)
  | E_constant {cons_name=C_SELF ; arguments=[_]}, {type_content=_ ; type_meta=_} ->
    fail (bad_self_err ())
  | _ -> ok (true,dat,e)
