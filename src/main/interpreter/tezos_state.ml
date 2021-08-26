open Trace
open Proto_alpha_utils
module Tezos_alpha_test_helpers = Ligo_009_PsFLoren_test_helpers
open Ligo_interpreter_exc
module Tezos_client = Tezos_client_009_PsFLoren
module Tezos_protocol = Tezos_protocol_009_PsFLoren
module Tezos_raw_protocol = Tezos_raw_protocol_009_PsFLoren

type block = Tezos_alpha_test_helpers.Block.t
type last_originations = (Memory_proto_alpha.Protocol.Alpha_context.Contract.t * Memory_proto_alpha.Protocol.Alpha_context.Contract.t list) list
type storage_tys = (Tezos_protocol.Protocol.Alpha_context.Contract.t * Ast_typed.type_expression) list
type parameter_tys = (Tezos_protocol.Protocol.Alpha_context.Contract.t * Ast_typed.type_expression) list

type context = {
  alpha_context : Memory_proto_alpha.Protocol.Alpha_context.t;
  threaded_context : block ;
  last_originations : last_originations ;
  storage_tys : storage_tys ;
  parameter_tys : parameter_tys ;
  baker : Memory_proto_alpha.Protocol.Alpha_context.Contract.t ;
  source : Memory_proto_alpha.Protocol.Alpha_context.Contract.t ;
  bootstrapped : Memory_proto_alpha.Protocol.Alpha_context.Contract.t list ;
  next_bootstrapped_contracts : Ligo_interpreter.Types.bootstrap_contract list ;
}

type state_error = Tezos_error_monad.TzCore.error list
type add_operation_outcome =
  | Success of context
  | Fail of state_error

let compare_account_ = Memory_proto_alpha.Protocol.Alpha_context.Contract.compare
let compare_account a b = (compare_account_ a b) = 0
let ligo_to_precanonical ~raise ~loc ~calltrace (x: unit Tezos_utils.Michelson.michelson) =
  let open Tezos_micheline.Micheline in
  let x = inject_locations (fun _ -> 0) (strip_locations x) in
  let x = strip_locations x in
  let x = Trace.trace_alpha_tzresult ~raise (throw_obj_exc loc calltrace) @@
    Tezos_protocol.Protocol.Michelson_v1_primitives.prims_of_strings x in
  x
let ligo_to_canonical ~raise ~loc ~calltrace (x: unit Tezos_utils.Michelson.michelson) =
  let x = ligo_to_precanonical ~raise ~loc ~calltrace x in
  (Tezos_protocol.Protocol.Alpha_context.Script.lazy_expr x)

let canonical_to_ligo x =
  x |> Tezos_protocol.Protocol.Michelson_v1_primitives.strings_of_prims
    |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())

let alpha_context_of_block ~raise ~loc ~calltrace (threaded_context : block) =
  let open Tezos_raw_protocol in
  let timestamp = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
                     Alpha_services.Delegate.Minimal_valid_time.get Tezos_alpha_test_helpers.Block.rpc_ctxt threaded_context 0 0 in
  Trace.trace_alpha_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Alpha_context.prepare
      ~level:threaded_context.header.shell.level
      ~predecessor_timestamp:threaded_context.header.shell.timestamp
      ~timestamp
      ~fitness:threaded_context.header.shell.fitness
      threaded_context.context

let get_alpha_context (ctxt : context) =
  ctxt.alpha_context

let get_timestamp (ctxt : context) =
  ctxt.threaded_context.header.shell.timestamp

let get_big_map ~raise ~loc ~calltrace (ctxt : context) id key key_ty  =
  let open Tezos_raw_protocol in
  let id = Alpha_context.Big_map.Id.parse_z id in
  let key_ty_michelson =
    Trace.trace_tzresult_lwt ~raise Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings key_ty in
  let (Ex_ty key_ty) =
    Trace.trace_tzresult_lwt ~raise Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty key_ty_michelson in
  let key_michelson =
    Trace.trace_tzresult_lwt ~raise Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings key in
  let key =
    Trace.trace_tzresult_lwt ~raise Main_errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_data key_michelson key_ty in
  let fctxt = get_alpha_context ctxt in
  let hash,_ = Trace.trace_alpha_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@ Script_ir_translator.hash_data fctxt key_ty key in
  let exec_get = Lwt_main.run @@
                 Tezos_protocol.Protocol.Alpha_services.Contract.big_map_get Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.threaded_context id hash in
  let error = Errors.generic_error loc "Unexpected response when accessing element from big_map identifier" in
  match exec_get with
  | Ok x -> (Some x)
  | Error [err] ->
     begin
     let err = Error_monad.json_of_error err in
     match err with
     | `O kvs ->
        begin
        match List.Assoc.find kvs "id" ~equal:String.equal with
        | Some (`String id) when String.equal id "RPC_context.Not_found" -> None
        | _ -> raise.raise @@ error
        end
     | _ -> raise.raise @@ error
     end
  | Error _ ->  raise.raise @@ error

let contract_exists ~raise ~loc ~calltrace ctxt contract =
  let ctxt = get_alpha_context ctxt in
  Trace.trace_alpha_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Memory_proto_alpha.Protocol.Alpha_context.Contract.exists ctxt contract

let get_storage ~raise ~loc ~calltrace ctxt addr =
  let st_v = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_protocol.Protocol.Alpha_services.Contract.storage Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.threaded_context addr
  in
  let st_ty = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_protocol.Protocol.Alpha_services.Contract.script Tezos_alpha_test_helpers.Block.rpc_ctxt ctxt.threaded_context addr
  in
  let (x,_) = Trace.trace_alpha_tzresult ~raise (throw_obj_exc loc calltrace) @@
    Memory_proto_alpha.Protocol.Script_repr.force_decode st_ty.code
  in
  let (_parameter_ty, storage_ty, _, _) = Trace.trace_alpha_tzresult ~raise (throw_obj_exc loc calltrace) @@
    Tezos_protocol.Protocol.Script_ir_translator.parse_toplevel ~legacy:false x
  in
  let storage_ty = Tezos_micheline.Micheline.(inject_locations (fun _ -> ()) (strip_locations storage_ty)) in
  let storage_ty = Tezos_micheline.Micheline.strip_locations storage_ty in
  let storage_ty = canonical_to_ligo storage_ty in
  (st_v, storage_ty)

let get_balance ~raise ~loc ~calltrace (ctxt :context) addr =
  Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_alpha_test_helpers.Context.Contract.balance (B ctxt.threaded_context) addr

let get_contract ~raise ~loc ~calltrace (ctxt :context) addr =
  Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_alpha_test_helpers.Context.Contract.balance (B ctxt.threaded_context) addr

let get_contract_rejection_data :
  state_error -> (Memory_proto_alpha.Protocol.Alpha_context.Contract.t * unit Tezos_utils.Michelson.michelson) option =
  fun errs ->
    let open Tezos_protocol_009_PsFLoren.Protocol in
    let open Script_interpreter in
    let open Environment in
    match errs with
    | [ Ecoproto_error (Runtime_contract_error (contract,_)) ; Ecoproto_error (Reject (_,x,_)) ] ->
      let x = canonical_to_ligo x in
      Some (contract,x)
    | _ -> None

let unwrap_baker ~raise ~loc : Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> Tezos_crypto.Signature.Public_key_hash.t  =
  fun x ->
    Trace.trace_option ~raise (Errors.generic_error loc "The baker is not an implicit account") @@ Memory_proto_alpha.Protocol.Alpha_context.Contract.is_implicit x

let script_of_compiled_code ~raise ~loc ~calltrace (contract : unit Tezos_utils.Michelson.michelson) (storage : unit Tezos_utils.Michelson.michelson) : Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Script.t  =
  let contract = ligo_to_canonical ~raise ~loc ~calltrace contract in
  let storage = ligo_to_canonical ~raise ~loc ~calltrace storage in
  Tezos_protocol.Protocol.Alpha_context.Script.{
    code = contract ;
    storage = storage ;
  }

let script_repr_of_compiled_code ~raise ~loc ~calltrace (contract : unit Tezos_utils.Michelson.michelson) (storage : unit Tezos_utils.Michelson.michelson) : Tezos_protocol_009_PsFLoren.Protocol.Script_repr.t  =
  let contract = ligo_to_canonical ~raise ~loc ~calltrace contract in
  let storage = ligo_to_canonical ~raise ~loc ~calltrace storage in
  Tezos_protocol.Protocol.Script_repr.{
    code = contract ;
    storage = storage ;
  }

let set_timestamp ~raise ~loc ~calltrace ({threaded_context;baker;_} as context :context) (timestamp:Z.t) =
  let open Tezos_alpha_test_helpers in
  let baker = unwrap_baker ~raise ~loc baker in
  let (timestamp:Time.Protocol.t) = Time.Protocol.of_seconds (Z.to_int64 timestamp) in
  let incr = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Incremental.begin_construction ~timestamp ~policy:Block.(By_account baker) threaded_context
  in
  let threaded_context = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Incremental.finalize_block incr
  in
  { context with threaded_context; alpha_context = Incremental.alpha_ctxt incr }

let extract_origination_from_result :
  type a .
    Memory_proto_alpha.Protocol.Alpha_context.Contract.t ->
    a Tezos_protocol.Protocol.Apply_results.contents_result ->
    last_originations =
  fun src x ->
  let open Tezos_raw_protocol in
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

let get_last_originations : Memory_proto_alpha.Protocol.Alpha_context.Contract.t -> Tezos_protocol.Protocol.operation_receipt -> last_originations =
  fun top_src x ->
    let open Tezos_raw_protocol in
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

let bake_op ~raise ~loc ~calltrace (ctxt:context) operation =
  let open Tezos_alpha_test_helpers in
  let baker = unwrap_baker ~raise ~loc ctxt.baker in
  let incr = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Incremental.begin_construction ~policy:Block.(By_account baker) ctxt.threaded_context
  in
  let incr = Incremental.add_operation incr operation in
  match Lwt_main.run @@ incr with
  | Ok incr ->
    let last_op = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
      Incremental.get_last_operation_result incr
    in
    let last_originations = get_last_originations ctxt.source last_op in
    let threaded_context = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
      Incremental.finalize_block incr
    in
    (Success {ctxt with threaded_context ; last_originations ; alpha_context = Incremental.alpha_ctxt incr })
  | Error errs -> (Fail errs)


let transfer ~raise ~loc ~calltrace (ctxt:context) ?entrypoint dst parameter amt : add_operation_outcome =
  let open Tezos_alpha_test_helpers in
  let parameters = ligo_to_canonical ~raise ~loc ~calltrace parameter in
  let operation : Tezos_raw_protocol_009_PsFLoren__Alpha_context.packed_operation = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    (* TODO: fee? *)
    let amt = Int64.of_int (Z.to_int amt) in
    Op.transaction ~fee:(Test_tez.Tez.of_int 23) ~parameters ?entrypoint (B ctxt.threaded_context) ctxt.source dst (Test_tez.Tez.of_mutez_exn amt)
  in
  bake_op ~raise ~loc ~calltrace ctxt operation

let originate_contract ~raise ~loc ~calltrace (ctxt :context) (contract : unit Tezos_utils.Michelson.michelson) (storage : unit Tezos_utils.Michelson.michelson) (amt : Z.t) =
  let open Tezos_alpha_test_helpers in
  let amt = Test_tez.Tez.of_mutez (Int64.of_int (Z.to_int amt)) in
  let script = script_of_compiled_code ~raise ~loc ~calltrace contract storage in
  let (operation, dst) = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    (* TODO : fee ? *)
    Op.origination (B ctxt.threaded_context) ctxt.source ?credit:amt ~fee:(Test_tez.Tez.of_int 10) ~script
  in
  let res = bake_op ~raise ~loc ~calltrace ctxt operation in
  (dst, res)

let get_bootstrapped_contract ~raise (n : int) =
  (* TODO-er: this function repeats work each time called... improve *)
  let rec foldnat s e = function
      0 -> e
    | k -> foldnat s (s e) (k - 1) in
  let open Tezos_raw_protocol.Contract_repr in
  let origination_nonce = foldnat incr_origination_nonce (initial_origination_nonce (Tezos_crypto.Operation_hash.hash_bytes [Bytes.of_string "Un festival de GADT."])) n in
  let contract = to_b58check (originated_contract origination_nonce) in
  let contract = Tezos_protocol.Protocol.Alpha_context.Contract.of_b58check contract in
  Trace.trace_alpha_tzresult ~raise (fun _ -> Errors.generic_error Location.generated "Error parsing address") @@ contract

let init_ctxt ~raise ?(loc=Location.generated) ?(calltrace=[]) ?(initial_balances=[]) ?(n=2) bootstrapped_contracts =
  let open Tezos_raw_protocol in
  let initial_contracts = List.map ~f:(fun (mutez, contract, storage, _, _) ->
                      let contract = script_of_compiled_code ~raise ~loc ~calltrace contract storage in
                      (Alpha_context.Tez.of_mutez_exn (Int64.of_int mutez),contract)) bootstrapped_contracts in
  let storage_tys = List.mapi ~f:(fun i (_, _, _, _, storage_ty) ->
                      let contract = get_bootstrapped_contract ~raise i in
                      (contract, storage_ty)) bootstrapped_contracts in
  let parameter_tys = List.mapi ~f:(fun i (_, _, _, parameter_ty, _) ->
                      let contract = get_bootstrapped_contract ~raise i in
                      (contract, parameter_ty)) bootstrapped_contracts in
  let (threaded_context, acclst) = Trace.trace_tzresult_lwt ~raise (throw_obj_exc loc calltrace) @@
    Tezos_alpha_test_helpers.Context.init ~initial_balances ~initial_contracts n in
  let alpha_context,_ = alpha_context_of_block ~raise ~loc ~calltrace threaded_context in
  match acclst with
  | baker::source::_ ->
    { threaded_context ; baker ; source ; bootstrapped = acclst ; last_originations = [] ; storage_tys ; parameter_tys ; alpha_context ; next_bootstrapped_contracts = [] }
  | _ ->
    raise.raise (Errors.bootstrap_not_enough loc)
