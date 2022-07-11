open Simple_utils
open Trace
open Helpers
open Errors

(**
  check_view_type checks against michelson restriction (usually defined in tezos/src/proto_alpha/lib_protocol/script_ir_translator.ml)
**)
let check_view_type ~raise : err_data:(Location.t*Ast_typed.expression_variable*Ast_typed.expression_variable) -> contract_type -> view_type -> unit =
  fun ~err_data:(loc,main_name,view_name) {storage = c_storage ; _} {arg ; storage = v_storage ; return} ->
    let () = trace_option ~raise (storage_view_contract loc main_name view_name c_storage v_storage) @@
      Ast_typed.assert_type_expression_eq (c_storage,v_storage) in
      let type_check err (t: Ast_typed.type_expression) : unit =
      let aux (t: Ast_typed.type_expression) =
        let open Stage_common.Constant in
        match t.type_content with
        | T_constant { injection = Big_map       ; _ }
        | T_constant { injection = Sapling_state ; _ }
        | T_constant { injection = Operation     ; _ }
        | T_constant { injection = Ticket        ; _ } -> raise.error err
        | _ -> ()
      in
      Helpers.iter_type_expression aux t
    in
    let () = type_check (type_view_io_out loc return) return in
    let () = type_check (type_view_io_in loc arg) arg in
    ()
