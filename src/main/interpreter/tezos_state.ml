open Trace
open Proto_alpha_utils
module Tezos_alpha_test_helpers = Ligo_008_PtEdo2Zk_test_helpers
open Ligo_interpreter_exc

type block = Tezos_alpha_test_helpers.Block.t
type last_originations = (Memory_proto_alpha.Protocol.Alpha_context.Contract.t * Memory_proto_alpha.Protocol.Alpha_context.Contract.t list) list
type storage_tys = (Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.t * Ast_typed.type_expression) list

type context = {
  alpha_context : Memory_proto_alpha.Protocol.Alpha_context.t option;
  threaded_context : block ;
  last_originations : last_originations ;
  storage_tys : storage_tys ;
  baker : Memory_proto_alpha.Protocol.Alpha_context.Contract.t ;
  source : Memory_proto_alpha.Protocol.Alpha_context.Contract.t ;
  bootstrapped : Memory_proto_alpha.Protocol.Alpha_context.Contract.t list ;
  bootstrapped_contracts : Ligo_interpreter.Types.bootstrap_contract list ;
}

type state_error = Tezos_error_monad.TzCore.error list
type add_operation_outcome =
  | Success of context
  | Fail of state_error

let compare_account_ = Memory_proto_alpha.Protocol.Alpha_context.Contract.compare
let compare_account a b = (compare_account_ a b) = 0
let ligo_to_canonical ~loc (x: unit Tezos_utils.Michelson.michelson) =
  let open Tezos_micheline.Micheline in
  let x = inject_locations (fun _ -> 0) (strip_locations x) in
  let x = strip_locations x in
  let* x = Trace.trace_alpha_tzresult (throw_obj_exc loc) @@
    Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.prims_of_strings x
  in
  ok (Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Script.lazy_expr x)

let ligo_to_precanonical ~loc (x: unit Tezos_utils.Michelson.michelson) =
  let open Tezos_micheline.Micheline in
  let x = inject_locations (fun _ -> 0) (strip_locations x) in
  let x = strip_locations x in
  let* x = Trace.trace_alpha_tzresult (throw_obj_exc loc) @@
    Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.prims_of_strings x in
  ok x

let canonical_to_ligo x =
  x |> Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.strings_of_prims
    |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())


let get_alpha_context (ctxt : context) =
  ctxt.alpha_context

let get_timestamp (ctxt : context) =
  ctxt.threaded_context.header.shell.timestamp

let get_big_map ~loc (ctxt : context) id key key_ty  =
  let open Tezos_raw_protocol_008_PtEdo2Zk in
  let id = Alpha_context.Big_map.Id.parse_z id in
  let* key_ty_michelson =
    Trace.trace_tzresult_lwt Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings key_ty in
  let* (Ex_ty key_ty) =
    Trace.trace_tzresult_lwt Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty key_ty_michelson in
  let* key_michelson =
    Trace.trace_tzresult_lwt Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings key in
  let* key =
    Trace.trace_tzresult_lwt Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_data key_michelson key_ty in
  let* fctxt = Trace.trace_option (Errors.generic_error loc "Not an alpha context yet?") @@ get_alpha_context ctxt in
  let* hash,_ = Trace.trace_alpha_tzresult_lwt (throw_obj_exc loc) @@ Script_ir_translator.hash_data fctxt key_ty key in
  let exec_get = Lwt_main.run @@
                 Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_services.Contract.big_map_get Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.threaded_context id hash in
  let error = Errors.generic_error loc "Unexpected response when accessing element from big_map identifier" in
  match exec_get with
  | Ok x -> ok (Some x)
  | Error [err] ->
     begin
     let err = Error_monad.json_of_error err in
     match err with
     | `O kvs ->
        begin
        match List.Assoc.find kvs "id" ~equal:String.equal with
        | Some (`String id) when String.equal id "RPC_context.Not_found" -> ok None
        | _ -> fail @@ error
        end
     | _ -> fail @@ error
     end
  | Error _ ->  fail @@ error

let get_storage ~loc ctxt addr =
  let* st_v = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_services.Contract.storage Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.threaded_context addr
  in
  let* st_ty = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_services.Contract.script Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.threaded_context addr
  in
  let* (x,_) = Trace.trace_alpha_tzresult (throw_obj_exc loc) @@
    Memory_proto_alpha.Protocol.Script_repr.force_decode st_ty.code
  in
  let* (_parameter_ty, storage_ty, _, _) = Trace.trace_alpha_tzresult (throw_obj_exc loc) @@
    Tezos_protocol_008_PtEdo2Zk.Protocol.Script_ir_translator.parse_toplevel ~legacy:false x
  in
  let storage_ty = Tezos_micheline.Micheline.(inject_locations (fun _ -> ()) (strip_locations storage_ty)) in
  let storage_ty = Tezos_micheline.Micheline.strip_locations storage_ty in
  let storage_ty = canonical_to_ligo storage_ty in
  ok (st_v, storage_ty)

let get_balance ~loc (ctxt :context) addr =
  Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    Tezos_alpha_test_helpers.Context.Contract.balance (B ctxt.threaded_context) addr
(*
let decode_op ~loc : bytes -> (Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation,_) result = fun b ->
  Trace.trace_decoding_error (fun _ -> Errors.generic_error loc "Error while decoding operation") @@
  Data_encoding.Binary.of_bytes Memory_proto_alpha.Protocol.Alpha_context.Operation.internal_operation_encoding b *)

let get_contract_rejection_data :
  state_error -> (Memory_proto_alpha.Protocol.Alpha_context.Contract.t * unit Tezos_utils.Michelson.michelson) option =
  fun errs ->
    let open Tezos_protocol_008_PtEdo2Zk.Protocol in
    let open Script_interpreter in
    let open Environment in
    match errs with
    | [ Ecoproto_error (Runtime_contract_error (contract,_)) ; Ecoproto_error (Reject (_,x,_)) ] ->
      let x = canonical_to_ligo x in
      Some (contract,x)
    | _ -> None

let unwrap_baker ~loc : Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> (Tezos_crypto.Signature.Public_key_hash.t , _ ) result =
  fun x ->
    Trace.trace_option (Errors.generic_error loc "The baker is not an implicit account") @@ Memory_proto_alpha.Protocol.Alpha_context.Contract.is_implicit x

let script_of_compiled_code ~loc (contract : unit Tezos_utils.Michelson.michelson) (storage : unit Tezos_utils.Michelson.michelson) : (Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Script.t, _) result  =
  let* contract = ligo_to_canonical ~loc contract in
  let* storage = ligo_to_canonical ~loc storage in
  ok @@ Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Script.{
    code = contract ;
    storage = storage ;
  }

let script_repr_of_compiled_code ~loc (contract : unit Tezos_utils.Michelson.michelson) (storage : unit Tezos_utils.Michelson.michelson) : (Tezos_protocol_008_PtEdo2Zk.Protocol.Script_repr.t, _) result  =
  let* contract = ligo_to_canonical ~loc contract in
  let* storage = ligo_to_canonical ~loc storage in
  ok @@ Tezos_protocol_008_PtEdo2Zk.Protocol.Script_repr.{
    code = contract ;
    storage = storage ;
  }

let set_timestamp ~loc ({threaded_context;baker;_} as context :context) (timestamp:Z.t) =
  let open Tezos_alpha_test_helpers in
  let* baker = unwrap_baker ~loc baker in
  let (timestamp:Time.Protocol.t) = Time.Protocol.of_seconds (Z.to_int64 timestamp) in
  let* incr = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    Incremental.begin_construction ~timestamp ~policy:Block.(By_account baker) threaded_context
  in
  let* threaded_context = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    Incremental.finalize_block incr
  in
  ok { context with threaded_context; alpha_context = Some (Incremental.alpha_ctxt incr) }

let extract_origination_from_result :
  type a .
    Memory_proto_alpha.Protocol.Alpha_context.Contract.t ->
    a Tezos_protocol_008_PtEdo2Zk.Protocol.Apply_results.contents_result ->
    last_originations =
  fun src x ->
  let open Tezos_raw_protocol_008_PtEdo2Zk in
  match x with
  | Manager_operation_result { operation_result = Applied (Transaction_result _) ; internal_operation_results } ->
    let aux (x:Apply_results.packed_internal_operation_result) =
      match x with
      | Internal_operation_result ({source ; _},Applied (Origination_result x)) -> [(source, x.originated_contracts)]
      | _ -> []
    in
    List.concat @@ List.map ~f:aux internal_operation_results
  | Manager_operation_result { operation_result = Applied (Origination_result x) ; internal_operation_results=_ } ->
    [(src, x.originated_contracts)]
  | _ -> []

let get_last_originations : Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> Tezos_protocol_008_PtEdo2Zk.Protocol.operation_receipt -> last_originations =
  fun top_src x ->
    let open Tezos_raw_protocol_008_PtEdo2Zk in
    match x with
    | No_operation_metadata -> []
    | Operation_metadata { contents } -> (
      let rec aux : type a . last_originations -> a Apply_results.contents_result_list -> last_originations =
        fun acc x ->
          match x with
          | Cons_result (hd, tl) -> (
            let x = extract_origination_from_result top_src hd in
            aux (acc @ x) tl
          )
          | Single_result x -> (
            let x = extract_origination_from_result top_src x in
            x @ acc
          )
      in
      aux [] contents
    )

let bake_op ~loc (ctxt:context) operation =
  let open Tezos_alpha_test_helpers in
  (* let open Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context in *)
  let* baker = unwrap_baker ~loc ctxt.baker in
  let* incr = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    Incremental.begin_construction ~policy:Block.(By_account baker) ctxt.threaded_context
  in
  let incr = Incremental.add_operation incr operation in
  match Lwt_main.run @@ incr with
  | Ok incr ->
    let* last_op = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
      Incremental.get_last_operation_result incr
    in
    let last_originations = get_last_originations ctxt.source last_op in
    let* threaded_context = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
      Incremental.finalize_block incr
    in
    ok (Success {ctxt with threaded_context ; last_originations ; alpha_context = Some (Incremental.alpha_ctxt incr) })
  | Error errs -> ok (Fail errs)


let transfer ~loc (ctxt:context) ?entrypoint dst parameter amt : (add_operation_outcome, _) result =
  let open Tezos_alpha_test_helpers in
  let* parameters = ligo_to_canonical ~loc parameter in
  let* operation = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    (* TODO: fee? *)
    let amt = Int64.of_int (Z.to_int amt) in
    Op.transaction ~fee:(Test_tez.Tez.of_int 23) ~parameters ?entrypoint (B ctxt.threaded_context) ctxt.source dst (Test_tez.Tez.of_mutez_exn amt)
  in
  bake_op ~loc ctxt operation

let originate_contract ~loc (ctxt :context) (contract : unit Tezos_utils.Michelson.michelson) (storage : unit Tezos_utils.Michelson.michelson) (amt : Z.t) =
  let open Tezos_alpha_test_helpers in
  let amt = Test_tez.Tez.of_mutez (Int64.of_int (Z.to_int amt)) in
  let* script = script_of_compiled_code ~loc contract storage in
  let* (operation, dst) = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    (* TODO : fee ? *)
    Op.origination (B ctxt.threaded_context) ctxt.source ?credit:amt ~fee:(Test_tez.Tez.of_int 10) ~script
  in
  let* res = bake_op ~loc ctxt operation in
  ok (dst, res)

let get_bootstrapped_contract (n : int) =
  (* TODO-er: this function repeats work each time called... improve *)
  let rec foldnat s e = function
      0 -> e
    | k -> foldnat s (s e) (k - 1) in
  let open Tezos_raw_protocol_008_PtEdo2Zk.Contract_repr in
  let origination_nonce = foldnat incr_origination_nonce (initial_origination_nonce (Tezos_crypto.Operation_hash.hash_bytes [Bytes.of_string "Un festival de GADT."])) n in
  let contract = to_b58check (originated_contract origination_nonce) in
  let contract = Tezos_protocol_008_PtEdo2Zk.Protocol.Alpha_context.Contract.of_b58check contract in
  Trace.trace_alpha_tzresult (fun _ -> Errors.generic_error Location.generated "Error parsing address") @@ contract

let init_ctxt ?(loc=Location.generated) ?(initial_balances=[]) ?(n=2) bootstrapped_contracts =
  let open Tezos_raw_protocol_008_PtEdo2Zk in
  let* initial_contracts = bind_map_list (fun (mutez, contract, storage, _) ->
                      let* contract = script_repr_of_compiled_code ~loc contract storage in
                      ok (Tez_repr.of_mutez_exn (Int64.of_int mutez),contract)) bootstrapped_contracts in
  let* storage_tys = bind_mapi_list (fun i (_, _, _,storage_ty) ->
                      let* contract = get_bootstrapped_contract i in
                      ok (contract, storage_ty)) bootstrapped_contracts in
  let* (threaded_context, acclst) = Trace.trace_tzresult_lwt (throw_obj_exc loc) @@
    Tezos_alpha_test_helpers.Context.init ~initial_balances ~initial_contracts n
  in
  match acclst with
  | baker::source::_ ->
    ok { threaded_context ; baker ; source ; bootstrapped = acclst ; last_originations = [] ; storage_tys ; alpha_context = None ; bootstrapped_contracts }
  | _ ->
    fail (Errors.bootstrap_not_enough loc)
