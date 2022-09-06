open Ligo_prim
open Simple_utils
open Trace
open Helpers
open Errors

(**
  check_view_type checks against michelson restriction (usually defined in tezos/src/proto_alpha/lib_protocol/script_ir_translator.ml)
**)
let check_view_type ~raise : err_data:(Value_var.t * Ast_typed.type_expression Binder.t) -> contract_type -> Ast_typed.type_expression -> unit =
  fun ~err_data:(main_name,view_binder) {storage = c_storage ; _} view_ty ->
    let view_loc = Value_var.get_location view_binder.var in
    let arg,v_storage,return =
      match Ast_typed.get_t_arrow view_ty with
      | Some { type1 = tin ; type2  = return } -> (
        match Ast_typed.get_t_tuple tin with
        | Some [ arg ; storage ] -> arg , storage , return
        | _ -> raise.error (expected_pair_in_view view_loc)
      )
      | None -> raise.error @@ bad_view_io main_name view_loc
    in
    let () = trace_option ~raise (storage_view_contract view_loc main_name view_binder.var c_storage v_storage) @@
      Ast_typed.assert_type_expression_eq (c_storage,v_storage) in
      let type_check err (t: Ast_typed.type_expression) : unit =
      let aux (t: Ast_typed.type_expression) =
        match t.type_content with
        | T_constant { injection = Big_map       ; _ }
        | T_constant { injection = Sapling_state ; _ }
        | T_constant { injection = Operation     ; _ }
        | T_constant { injection = Ticket        ; _ } -> raise.error err
        | _ -> ()
      in
      Helpers.iter_type_expression aux t
    in
    let () = type_check (type_view_io_out view_loc return) return in
    let () = type_check (type_view_io_in view_loc arg) arg in
    ()
