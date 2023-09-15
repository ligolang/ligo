open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
module ModRes = Preprocessor.ModRes

include Flag.With_arg (struct
  type flag = ModRes.t option
end)

(* [resolve_contract_file ~mod_res ~source_file ~contract_file] tries to resolve
   [contract_file] w.r.t. to process directory
   if that fails it tries to resolve it as a relative path w.r.t. directory of [source_file]
   if that fails it tries to resolve it as a package path using [mod_res] *)
let resolve_contract_file ~mod_res ~source_file ~contract_file =
  match Caml.Sys.file_exists contract_file with
  | true -> contract_file
  | false ->
    (match source_file with
    | Some source_file ->
      let d = Filename.dirname source_file in
      let s = Filename.concat d contract_file in
      (match Caml.Sys.file_exists s with
      | true -> s
      | false -> ModRes.Helpers.resolve ~file:contract_file mod_res)
    | None -> ModRes.Helpers.resolve ~file:contract_file mod_res)


let get_file_from_location loc =
  let open Simple_utils.Option in
  let* reg = Location.get_file loc in
  let file = reg#file in
  if String.(file = "") then None else Some file


let input_all ~raise ~loc source_file =
  try In_channel.(with_file source_file ~f:input_all) with
  | Sys_error msg -> raise.error (sys_error loc msg)
  | e -> Caml.raise e


let compile ~raise =
  let mod_res = get_flag () in
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_raw_code { language = "of_file"; code } as e ->
      (match get_e code with
      | E_literal (Literal_string path) ->
        let source_file = get_file_from_location loc in
        let path = Simple_utils.Ligo_string.extract path in
        let source_file =
          resolve_contract_file ~mod_res ~source_file ~contract_file:path
        in
        let s = input_all ~raise ~loc source_file in
        e_verbatim ~loc s
      | _ -> make_e ~loc e)
    | E_raw_code { language = "create_contract_of_file"; code } as e ->
      (match get_e code with
      | E_literal (Literal_string path) ->
        let source_file = get_file_from_location loc in
        let path = Simple_utils.Ligo_string.extract path in
        let source_file =
          resolve_contract_file ~mod_res ~source_file ~contract_file:path
        in
        let s = input_all ~raise ~loc source_file in
        let michelson = Format.sprintf "{ UNPAIR 3 ; CREATE_CONTRACT %s ; PAIR }" s in
        let code = e_verbatim ~loc michelson in
        let ty_storage = Ty_variable.fresh ~loc ~name:"storage" () in
        let ty_src =
          t_prod
            ~loc
            ( t_option ~loc (t_constant ~loc "key_hash")
            , [ t_constant ~loc "tez"; t_var ~loc ty_storage ] )
        in
        let ty_tgt =
          t_prod ~loc (t_constant ~loc "operation", [ t_constant ~loc "address" ])
        in
        let ty_type_ = t_fun ~loc (ty_src, ty_tgt) in
        let code = e_annot ~loc (code, ty_type_) in
        let code = e_raw_code ~loc { language = "Michelson"; code } in
        let storage = Variable.fresh ~loc ~name:"storage" () in
        let expr_storage = e_variable ~loc storage in
        let tez = Variable.fresh ~loc ~name:"tez" () in
        let expr_tez = e_variable ~loc tez in
        let key_hash = Variable.fresh ~loc ~name:"key_hash" () in
        let expr_key_hash = make_e ~loc (E_variable key_hash) in
        let args = e_tuple ~loc (expr_key_hash, [ expr_tez; expr_storage ]) in
        let code = e_application ~loc { lamb = code; args } in
        let code =
          e_lambda
            ~loc
            { binder = Ligo_prim.Param.make storage None
            ; output_type = None
            ; result = code
            }
        in
        let code =
          e_lambda
            ~loc
            { binder = Ligo_prim.Param.make tez None; output_type = None; result = code }
        in
        let code =
          e_lambda
            ~loc
            { binder = Ligo_prim.Param.make key_hash None
            ; output_type = None
            ; result = code
            }
        in
        e_type_abstraction ~loc { type_binder = ty_storage; result = code }
      | _ -> make_e ~loc e)
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
