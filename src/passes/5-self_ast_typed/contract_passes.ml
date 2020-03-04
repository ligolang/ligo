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
  
end

let self_typing : contract_pass_data -> expression -> (bool * contract_pass_data * expression) result = fun dat e ->
  let bad_self_err () = Errors.bad_self_type
    e.type_expression 
    {e.type_expression with type_content = T_operator (TC_contract dat.contract_type.parameter)}
    e.location
  in
  match e.expression_content , e.type_expression with
  | E_constant {cons_name=C_SELF ; arguments=[]}, {type_content = T_operator (TC_contract t) ; type_meta=_} ->
    let%bind () =
      trace_strong (bad_self_err ()) @@
      Ast_typed.assert_type_expression_eq (dat.contract_type.parameter,t) in
    ok (true, dat, e)
  | E_constant {cons_name=C_SELF ; arguments=[]}, {type_content=_ ; type_meta=_} ->
    fail (bad_self_err ())
  | _ -> ok (true,dat,e)
