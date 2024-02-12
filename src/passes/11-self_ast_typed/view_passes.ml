open Ligo_prim
module Row = Ast_typed.Row
open Simple_utils
open Trace
open Errors

(**
  check_view_type checks against michelson restriction (usually defined in tezos/src/proto_alpha/lib_protocol/script_ir_translator.ml)
**)
let check_view_type ~raise
    :  err_data:Module_var.t * Ast_typed.type_expression Binder.t
    -> Ast_typed.contract_sig -> Ast_typed.type_expression -> unit
  =
 fun ~err_data:(main_name, view_binder) { storage = c_storage; _ } view_ty ->
  let view_loc = Value_var.get_location @@ Binder.get_var view_binder in
  let arg, v_storage, return =
    match Ast_typed.should_uncurry_view ~storage_ty:c_storage view_ty with
    | `Yes v | `No v -> v
    | `Bad_storage v_storage ->
      raise.error
      @@ storage_view_contract
           view_loc
           main_name
           (Binder.get_var view_binder)
           c_storage
           v_storage
    | `Bad_not_function -> raise.error @@ bad_view_io main_name view_loc
    | `Bad -> raise.error @@ bad_view_storage main_name c_storage view_loc
  in
  let () =
    trace_option
      ~raise
      (storage_view_contract
         view_loc
         main_name
         (Binder.get_var view_binder)
         c_storage
         v_storage)
    @@ Ast_typed.assert_type_expression_eq (c_storage, v_storage)
  in
  let rec type_check err (t : Ast_typed.type_expression) : unit =
    let self = type_check err in
    match t.type_content with
    | T_variable _ -> ()
    | T_constant { injection = Big_map; _ }
    | T_constant { injection = Sapling_state; _ }
    | T_constant { injection = Operation; _ }
    | T_constant { injection = Ticket; _ } -> raise.error err
    | T_constant x -> List.iter ~f:self x.parameters
    | T_sum (row, _) | T_record row -> Row.iter self row
    | T_arrow _ ->
      (* lambdas are always OK *)
      ()
    | T_singleton _ -> ()
    | T_abstraction x -> self x.type_
    | T_for_all x -> self x.type_
  in
  let () = type_check (type_view_io_out view_loc return) return in
  let () = type_check (type_view_io_in view_loc arg) arg in
  ()


let iter ~raise module_ =
  (* Use mapper as a cheap form for iter *)
  ignore
  @@ Helpers.Declaration_mapper.map_module
       (fun decl ->
         match Location.unwrap decl with
         | Ast_typed.D_module
             { module_binder
             ; module_ =
                 { module_content = M_struct module_
                 ; signature = { sig_sort = Ss_contract { storage; parameter }; _ }
                 ; _
                 }
             ; _
             } ->
           List.iter module_ ~f:(fun decl ->
               match Ast_typed.Helpers.fetch_view_type decl with
               | None -> ()
               | Some (type_, binder) ->
                 check_view_type
                   ~raise
                   ~err_data:(module_binder, binder)
                   { parameter; storage }
                   type_);
           decl
         | _ -> decl)
       module_


let program ~raise (program : Ast_typed.program) = iter ~raise program.pr_module
