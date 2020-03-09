open Ast_typed
open Trace

type contract_pass_data = {
  contract_type : Helpers.contract_type ;
  main_name : string ;
}

module Errors = struct
  let bad_self_type expected got loc () =
    let title = thunk "bad self type" in
    let message () = Format.asprintf "expected %a but got %a" Ast_typed.PP.type_expression expected Ast_typed.PP.type_expression got in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()
  
  let bad_format_entrypoint_ann ep loc () =
    let title = thunk "bad entrypoint format" in
    let message () = Format.asprintf "entrypoint \"%s\" is badly formatted. We expect \"%%bar\" for entrypoint Bar and \"%%default\" when no entrypoint used" ep in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
    ] in
    error ~data title message ()

  let entrypoint_annotation_not_literal loc () =
    let title = thunk "entrypoint annotation must be a string literal" in
    let message () = Format.asprintf "" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
    ] in
    error ~data title message ()
  
  let unmatched_entrypoint loc () =
    let title = thunk "No constructor matches the entrypoint annotation" in
    let message () = Format.asprintf "" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
    ] in
    error ~data title message ()

end

let check_entrypoint_annotation_format ep exp =
  match String.split_on_char '%' ep with
    | [ "" ; ep'] ->
      let cap = String.capitalize_ascii ep' in
      if String.equal cap ep' then fail @@ Errors.bad_format_entrypoint_ann ep exp.location
      else ok cap
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
      | _ -> fail @@ Errors.entrypoint_annotation_not_literal entrypoint_exp.location in
    let%bind entrypoint_t = match dat.contract_type.parameter.type_content with
      | T_sum cmap -> trace_option (Errors.unmatched_entrypoint entrypoint_exp.location)
                      @@ Stage_common.Types.CMap.find_opt (Constructor entrypoint) cmap
      | t -> ok {dat.contract_type.parameter with type_content = t} in
    let%bind () =
      trace_strong (bad_self_err ()) @@
      Ast_typed.assert_type_expression_eq (entrypoint_t , t) in
    ok (true, dat, e)
  | _ -> ok (true,dat,e)
