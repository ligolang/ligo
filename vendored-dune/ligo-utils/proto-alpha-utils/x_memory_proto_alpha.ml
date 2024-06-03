open! Core
module Michelson = Tezos_utils.Michelson
include Memory_proto_alpha

let (<@) f g x = f (g x)

let init_environment = Init_proto_alpha.init_environment
let dummy_environment = Init_proto_alpha.dummy_environment
let test_environment = Init_proto_alpha.test_environment

open Protocol
open Script_typed_ir
open Script_ir_translator
open Script_interpreter

module X = struct
  open Alpha_context
  open Script_tc_errors
  open Alpha_environment.Error_monad

  let rec stack_ty_eq
      : type ta tb ra rb.
        context
        -> int
        -> (ta, ra) stack_ty
        -> (tb, rb) stack_ty
        -> (((ta, ra) stack_ty, (tb, rb) stack_ty) eq * context) tzresult
    =
   fun ctxt lvl ta tb ->
    let error_details = Informative 0 in
    match ta, tb with
    | Item_t (tva, ra), Item_t (tvb, rb) ->
      let ( >>? ) v f =
        let open Result_syntax in
        let* x = v in
        f x
      in
      let x = ty_eq ~error_details tva tvb in
      Gas_monad.run ctxt x
      >>? fun (x, ctxt) ->
      x
      >>? fun Eq ->
      stack_ty_eq ctxt (lvl + 1) ra rb
      >>? fun (Eq, ctxt) : (((ta, ra) stack_ty, (tb, rb) stack_ty) eq * context) tzresult ->
      Ok (Eq, ctxt)
    | Bot_t, Bot_t -> Ok (Eq, ctxt)
    | _, _ -> error Bad_stack_length

  module Unparse_costs = Michelson_v1_gas.Cost_of.Unparsing

  type ex_typed_value =
    | Ex_typed_value : (('a, 'comparable) Script_typed_ir.ty * 'a) -> ex_typed_value

  module Interp_costs = Michelson_v1_gas.Cost_of
end

open X_error_monad

let dummy_environment_result ?environment () =
  Option.value
    ~default:(Lwt.map Result.return @@ dummy_environment ())
    (Option.map ~f:(fun env -> Lwt.return (Ok env)) environment)

let dummy_tezos_context ?tezos_context () =
  Option.value
    ~default:
      (Lwt.map (fun env -> Ok env.Init_proto_alpha.tezos_context) @@ dummy_environment ())
    (Option.map ~f:(fun ctxt -> Lwt.return (Ok ctxt)) tezos_context)

let stack_ty_eq
    (type a ra b rb)
    ?tezos_context
    (a : (a, ra) stack_ty)
    (b : (b, rb) stack_ty)
  =
  let open Lwt_result.Let_syntax in
  let%bind tezos_context = dummy_tezos_context ?tezos_context () in
  let%bind Eq, _ = Lwt.return @@ alpha_wrap (X.stack_ty_eq tezos_context 0 a b) in
  Lwt_result.return Eq

let ty_eq (type a b) ?tezos_context (a : (a, _) ty) (b : (b, _) ty)
    : ((_, _) eq, Tezos_base.TzPervasives.tztrace) Lwt_result.t
  =
  let open Lwt_result.Let_syntax in
  let%bind tezos_context = dummy_tezos_context ?tezos_context () in
  let error_details = Script_tc_errors.Informative 0 in
  let%bind x, _ =
    Lwt.return
    @@ alpha_wrap
         (let x = Script_ir_translator.ty_eq ~error_details a b in
          Gas_monad.run tezos_context x)
  in
  Lwt.return @@ alpha_wrap x

(* should not need lwt *)
let canonical_of_strings michelson =
  let michelson, errs =
    Tezos_client_019_PtParisB.Michelson_v1_macros.expand_rec michelson
  in
  match errs with
  | _ :: _ -> Lwt.return (Error errs)
  | [] ->
    Lwt.return
      (alpha_wrap
         (Michelson_v1_primitives.prims_of_strings
            (Tezos_micheline.Micheline.strip_locations michelson)))

let prims_of_strings michelson =
  let open Lwt_result_syntax in
  let* michelson = canonical_of_strings michelson in
  return (Tezos_micheline.Micheline.root michelson)

let lazy_expr expr =
  let open Alpha_context in
  Script.lazy_expr expr

let parse_michelson_fail
    (type aft aftr)
    ?tezos_context
    ~(top_level : tc_context)
    michelson
    ?(legacy = false)
    ?type_logger
    (bef : ('a, 'b) Script_typed_ir.stack_ty)
    (aft : (aft, aftr) Script_typed_ir.stack_ty)
    : (('a, 'b, aft, aftr) descr, error trace) result Lwt.t
  =
  let ( >>=? ) = Lwt_result_syntax.( let* ) in
  let ( >>? ) = Result_syntax.( let* ) in
  dummy_tezos_context ?tezos_context ()
  >>=? fun tezos_context ->
  canonical_of_strings michelson
  >>=? fun michelson ->
  Alpha_context.Global_constants_storage.expand tezos_context michelson
  >>=? (fun (tezos_context, michelson) ->
         let michelson = Tezos_micheline.Micheline.root michelson in
         let elab_conf =
           Script_ir_translator_config.
             { type_logger; legacy; keep_extra_types_for_interpreter_logging = false }
         in
         parse_instr ~elab_conf top_level tezos_context michelson bef)
  >>=?? fun (j, _) ->
  match j with
  | Typed descr ->
    Lwt.return
      (alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft)
      >>? fun (Eq, _) ->
      let descr : (_, _, aft, aftr) descr = { descr with aft } in
      Ok descr)
  | Failed { descr } -> Lwt.return (Ok (descr aft))

let parse_michelson_data ?tezos_context michelson ty =
  dummy_tezos_context ?tezos_context ()
  >>=?? fun tezos_context ->
  let elab_conf =
    Script_ir_translator_config.
      { type_logger = None
      ; legacy = false
      ; keep_extra_types_for_interpreter_logging = false
      }
  in
  parse_data
    tezos_context
    ty
    michelson
    ~elab_conf
    ~allow_forged_tickets:true
    ~allow_forged_lazy_storage_id:true
  >>=?? fun (data, _) -> Lwt_result_syntax.return data

let parse_michelson_ty
    ?tezos_context
    ?(allow_operation = true)
    ?(allow_contract = true)
    ?(allow_lazy_storage = true)
    ?(allow_ticket = true)
    michelson
  =
  dummy_tezos_context ?tezos_context ()
  >>=?? fun tezos_context ->
  Lwt.return
  @@ parse_ty
       tezos_context
       ~allow_operation
       michelson
       ~legacy:false
       ~allow_contract
       ~allow_lazy_storage
       ~allow_ticket
  >>=?? fun (ty, _) -> Lwt_result_syntax.return ty

let strings_of_prims michelson =
  let michelson = Tezos_micheline.Micheline.strip_locations michelson in
  let michelson = Michelson_v1_primitives.strings_of_prims michelson in
  Tezos_micheline.Micheline.root michelson

let node_to_canonical m =
  let open Tezos_micheline.Micheline in
  let x = inject_locations (fun _ -> 0) (strip_locations m) in
  let x = strip_locations x in
  Michelson_v1_primitives.prims_of_strings x

let unparse_michelson_data ?tezos_context ty value =
  dummy_tezos_context ?tezos_context ()
  >>=?? fun tezos_context ->
  unparse_data tezos_context Readable ty value
  >>=?? fun (michelson, _) ->
  let michelson = Tezos_micheline.Micheline.inject_locations (fun _ -> 0) michelson in
  Lwt_result_syntax.return (strings_of_prims michelson)

let unparse_michelson_ty ?tezos_context ty =
  dummy_tezos_context ?tezos_context ()
  >>=?? fun tezos_context ->
  Lwt.return @@ Script_ir_unparser.unparse_ty ~loc:() tezos_context ty
  >>=?? fun (michelson, _) -> Lwt_result_syntax.return (strings_of_prims michelson)

type options =
  { tezos_context : Alpha_context.t
  ; source : Alpha_context.Contract.t
  ; payer : Alpha_context.Contract.t
  ; self : Alpha_context.Contract.t
  ; amount : Alpha_context.Tez.t
  ; chain_id : Environment.Chain_id.t
  ; balance : Alpha_context.Tez.t
  ; now : Script_timestamp.t
  ; level : Script_int.n Script_int.num
  }

let t_unit =
  Tezos_micheline.Micheline.(
    strip_locations (Prim (0, Michelson_v1_primitives.T_unit, [], [])))

let default_self =
  force_ok_alpha
    ~msg:"bad default self"
    (Alpha_context.Contract.of_b58check "KT1DUMMYDUMMYDUMMYDUMMYDUMMYDUMu2oHG")

let begin_validation_and_application ctxt chain_id mode ~predecessor =
  let open Lwt_result_syntax in
  let* validation_state = Main.begin_validation ctxt chain_id mode ~predecessor in
  let* application_state = Main.begin_application ctxt chain_id mode ~predecessor in
  return (validation_state, application_state)

(* fake bake a block in order to set the predecessor timestamp *)
let fake_bake tezos_context chain_id now : Alpha_context.t Lwt.t =
  let ( >>= ) = Lwt_syntax.( let* ) in
  let ( >>=? ) = Lwt_result_syntax.( let* ) in
  Lwt.map (force_ok ~msg:("bad init" ^ __LOC__)) (Init_proto_alpha.Context_init.init 1)
  >>= fun ((_, header, hash), _, _) ->
  let tezos_context = (Alpha_context.finalize tezos_context header.fitness).context in
  let contents = Init_proto_alpha.Context_init.contents ~predecessor_hash:hash () in
  let protocol_data =
    let open! Alpha_context.Block_header in
    { contents; signature = Tezos_crypto.Signature.zero }
  in
  let tezos_context =
    Lwt.map
      (force_ok ~msg:("bad block " ^ __LOC__))
      ((let predecessor_timestamp =
          match
            Alpha_context.Timestamp.of_seconds_string
              (Z.to_string (Script_timestamp.to_zint now))
          with
          | Some t -> t
          | _ -> Stdlib.failwith "bad timestamp"
        in
        let timestamp =
          match
            Alpha_context.Timestamp.of_seconds_string
              (Z.to_string (Z.add (Z.of_int 20) (Script_timestamp.to_zint now)))
          with
          | Some t -> t
          | _ -> Stdlib.failwith "bad timestamp"
        in
        let predecessor_context = tezos_context in
        let header = { header with timestamp = predecessor_timestamp } in
        let predecessor = hash in
        begin_validation_and_application
          tezos_context
          Alpha_environment.Chain_id.zero
          (Construction
             { predecessor_hash = hash; timestamp; block_header_data = protocol_data })
          ~predecessor:header)
      >>= fun x ->
      Lwt.return @@ Alpha_environment.wrap_tzresult x
      >>=? fun (_, state) -> Lwt_result_syntax.return state.ctxt)
  in
  tezos_context

let register_constant tezos_context constant =
  Alpha_context.Global_constants_storage.register tezos_context constant

let make_options
    ?(env : Init_proto_alpha.environment option)
    ?(tezos_context : Alpha_context.t option)
    ?(constants = [])
    ?(now : Script_timestamp.t option)
    ?(sender : Alpha_context.Contract.t option)
    ?(self = default_self)
    ?(parameter_ty = t_unit)
    ?(source : Alpha_context.Contract.t option)
    ?(amount = Alpha_context.Tez.one)
    ?(balance = Alpha_context.Tez.zero)
    ?(chain_id = Environment.Chain_id.zero)
    ()
    : options Lwt.t
  =
  let open Alpha_context in
  let open Michelson_v1_primitives in
  let open Tezos_micheline in
  let open Micheline in
  let open Lwt.Let_syntax in
  let%bind env =
    Option.value ~default:(dummy_environment ()) (Option.map ~f:Lwt.return env)
  in
  let tezos_context = Option.value ~default:env.tezos_context tezos_context in
  let now = Option.value ~default:(Script_timestamp.now env.tezos_context) now in
  let sender =
    Option.value ~default:(List.nth_exn env.identities 0).implicit_contract sender
  in
  let source =
    Option.value ~default:(List.nth_exn env.identities 1).implicit_contract source
  in
  let dummy_script =
    let parameter_ty = root parameter_ty in
    Script.lazy_expr
    @@ strip_locations
    @@ Seq
         ( 0
         , [ Prim (0, K_parameter, [ parameter_ty ], [])
           ; Prim (0, K_storage, [ Prim (0, T_unit, [], []) ], [])
           ; Prim
               ( 0
               , K_code
               , [ Seq
                     ( 0
                     , [ Prim (0, I_CDR, [], [])
                       ; Prim (0, I_NIL, [ Prim (0, T_operation, [], []) ], [])
                       ; Prim (0, I_PAIR, [], [])
                       ] )
                 ]
               , [] )
           ] )
  in
  let dummy_storage =
    Micheline.strip_locations @@ Micheline.Prim (0, Michelson_v1_primitives.D_Unit, [], [])
  in
  let lazy_dummy_storage = Script.lazy_expr dummy_storage in
  let script = Script.{ code = dummy_script; storage = lazy_dummy_storage } in
  let%bind tezos_context =
    let self =
      match self with
      | Implicit hash -> Contract_hash.zero
      | Originated hash -> hash
    in
    Lwt.map (force_ok ~msg:("bad options " ^ __LOC__) <@ alpha_wrap)
    @@ Alpha_context.Contract.raw_originate
         tezos_context
         ~prepaid_bootstrap_storage:false
         self
         ~script:(script, None)
  in
  (* fake bake to set the predecessor timestamp *)
  let time_between_blocks = 1 in
  let level =
    (Level.current tezos_context).level
    |> Raw_level.to_int32
    |> Script_int.of_int32
    |> Script_int.abs
  in
  let%bind tezos_context =
    fake_bake
      tezos_context
      chain_id
      (Script_timestamp.sub_delta now (Script_int.of_int time_between_blocks))
  in
  (* Update the Tezos context by registering the global constants *)
  let%map tezos_context =
    Lwt_list.fold_left_s
      (fun ctxt cnt ->
        let%map ctxt, _, _ =
          Lwt.map (force_ok ~msg:("bad constants " ^ __LOC__) <@ alpha_wrap)
          @@ register_constant ctxt cnt
        in
        ctxt)
      tezos_context
      constants
  in
  { tezos_context
  ; source = sender
  ; payer = source
  ; self
  ; amount
  ; chain_id
  ; balance
  ; now = Script_timestamp.now tezos_context
  ; level
  }

let no_trace_logger = None

let interpret ?options (instr : ('a, 'b, 'c, 'd) kdescr) bef : (_ * _) tzresult Lwt.t =
  let open Lwt.Let_syntax in
  let%bind { tezos_context; source; self; payer; amount; chain_id; balance; now; level } =
    Option.value ~default:(make_options ()) (Option.map ~f:Lwt.return options)
  in
  let self =
    match self with
    | Implicit hash -> Contract_hash.zero
    | Originated hash -> hash
  in
  let payer =
    match payer with
    | Implicit hash -> hash
    | Originated hash -> Tezos_crypto.Signature.Public_key_hash.zero
  in
  let sender = Alpha_context.Destination.Contract source in
  let step_constants = { sender; self; payer; amount; chain_id; balance; now; level } in
  Script_interpreter.Internals.step_descr
    no_trace_logger
    tezos_context
    step_constants
    instr
    bef
    (EmptyCell, EmptyCell)
  >>=?? fun (stack, _, _) -> Lwt_result_syntax.return stack

let unparse_ty_michelson ty =
  Lwt.bind (dummy_environment ()) (fun env ->
      Lwt.return @@ Script_ir_unparser.unparse_ty ~loc:() env.tezos_context ty
      >>=?? fun (n, _) -> Lwt_result_syntax.return n)

type typecheck_res =
  | Type_checked
  | Err_parameter
  | Err_storage
  | Err_contract
  | Err_gas
  | Err_unknown

let typecheck_contract ?environment contract =
  let ( >>= ) = Lwt_syntax.( let* ) in
  let ( >>=? ) = Lwt_result_syntax.( let* ) in
  dummy_environment_result ?environment ()
  >>=?? fun env ->
  let contract' = Tezos_micheline.Micheline.strip_locations contract in
  let legacy = false in
  Script_ir_translator.typecheck_code ~show_types:true ~legacy env.tezos_context contract'
  >>= fun x ->
  match x with
  | Ok _ -> Lwt_result_syntax.return @@ contract
  | Error errs -> Lwt.return @@ Error (Alpha_environment.wrap_tztrace errs)

let typecheck_map_contract ?environment contract =
  let ( >>= ) = Lwt_syntax.( let* ) in
  let ( >>=? ) = Lwt_result_syntax.( let* ) in
  dummy_environment_result ?environment ()
  >>=?? fun env ->
  let contract' = Tezos_micheline.Micheline.strip_locations contract in
  let legacy = false in
  Script_ir_translator.typecheck_code ~show_types:true ~legacy env.tezos_context contract'
  >>= fun x ->
  match x with
  | Ok (map, _) -> Lwt_result_syntax.return @@ (map, contract)
  | Error errs -> Lwt.return @@ Error (Alpha_environment.wrap_tztrace errs)

(* This function checks that running `view_code` on a stack with `input_ty` and `storage_ty`, it
   ends up with `output_ty` (and also verifies any restriction enforced in views) *)
let typecheck_view ?environment input_ty output_ty storage_ty view_code =
  let ( >>= ) = Lwt_syntax.( let* ) in
  let ( >>=? ) = Lwt_result_syntax.( let* ) in
  dummy_environment_result ?environment ()
  >>=?? fun env ->
  let legacy = false in
  let elab_conf =
    Script_ir_translator_config.
      { type_logger = None; legacy; keep_extra_types_for_interpreter_logging = false }
  in
  let view = { view_code; input_ty; output_ty } in
  parse_michelson_ty ~tezos_context:env.tezos_context storage_ty
  >>= fun storage_ty ->
  match storage_ty with
  | Ok (Ex_ty storage_ty) ->
    Script_ir_translator.parse_view ~elab_conf env.tezos_context storage_ty view
    >>= fun x ->
    (match x with
    | Ok (map, _) -> Lwt_result_syntax.return @@ ()
    | Error errs -> Lwt.return @@ Error (Alpha_environment.wrap_tztrace errs))
  | Error errs -> Lwt.return @@ Error errs

type 'a interpret_res =
  | Succeed of 'a
  | Fail of Script_repr.expr

let failure_interpret ?options (instr : ('a, 's, 'b, 'u) descr) (bef : 'a) stackb
    : _ interpret_res tzresult Lwt.t
  =
  let ( >>= ) = Lwt_syntax.( let* ) in
  Option.value ~default:(make_options ()) (Option.map ~f:Lwt.return options)
  >>= fun options ->
  let { tezos_context; source; self; payer; amount; chain_id; balance; now; level } =
    options
  in
  let descr = instr in
  let kinstr = descr.instr.apply (IHalt descr.loc) in
  let kdescr = { kloc = descr.loc; kbef = descr.bef; kaft = descr.aft; kinstr } in
  let instr = kdescr in
  let step_constants =
    let self =
      match self with
      | Implicit hash -> Contract_hash.zero
      | Originated hash -> hash
    in
    let payer =
      match payer with
      | Implicit hash -> hash
      | Originated hash -> Tezos_crypto.Signature.Public_key_hash.zero
    in
    let sender = Alpha_context.Destination.Contract source in
    { sender; self; payer; amount; chain_id; balance; now; level }
  in
  Script_interpreter.Internals.step_descr
    no_trace_logger
    tezos_context
    step_constants
    instr
    bef
    stackb
  >>= fun x ->
  match x with
  | Ok (s, _, _ctxt) -> Lwt_result_syntax.return @@ Succeed s
  | Error errs ->
    (match Alpha_environment.wrap_tztrace errs with
    | Alpha_environment.(Ecoproto_error (Reject (_, expr, _)) :: _t) ->
      Lwt_result_syntax.return @@ Fail expr (* This catches failwith errors *)
    | _ -> Lwt.return @@ Error (Alpha_environment.wrap_tztrace errs))

let pack (data_ty : ('a, _) ty) (data : 'a) : bytes tzresult Lwt.t =
  Lwt.bind (dummy_environment ()) (fun env ->
      pack_data env.tezos_context data_ty data
      >>=?? fun (packed, _) -> Lwt_result.return packed)

let strings_of_prims = Michelson_v1_primitives.strings_of_prims

let to_bytes michelson =
  Lwt.map
    (fun michelson ->
      let canonical = Tezos_micheline.Micheline.strip_locations michelson in
      Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding canonical)
    (Lwt.map
       (X_error_monad.force_ok ~msg:"Internal error: could not serialize Michelson")
       (prims_of_strings michelson))

let to_hex michelson = Lwt.map Hex.of_bytes (to_bytes michelson)

(*
  original function: `expr_to_address_in_context` in `/tezos/src/proto_alpha/lib_protocol/global_constants_storage.ml`
  modified to just get the hash out of a script without any need for the raw context
*)
let expr_to_address_in_context : Script_repr.expr -> Script_expr_hash.t option =
 fun expr ->
  let lexpr = Script_repr.lazy_expr expr in
  match Script_repr.force_bytes lexpr with
  | Ok b -> Some (Script_expr_hash.hash_bytes [ b ])
  | Error _ -> None
