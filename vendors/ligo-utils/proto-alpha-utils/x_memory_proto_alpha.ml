module Michelson = Tezos_utils.Michelson

include Memory_proto_alpha
let init_environment = Init_proto_alpha.init_environment
let dummy_environment = Init_proto_alpha.dummy_environment


open Protocol
open Script_typed_ir
open Script_ir_translator
open Script_interpreter

module X = struct
  open Alpha_context
  open Script_tc_errors
  open Alpha_environment.Error_monad

  let rec stack_ty_eq
    : type ta tb. context -> int -> ta stack_ty -> tb stack_ty ->
      ((ta stack_ty, tb stack_ty) eq * context) tzresult
    = fun ctxt lvl ta tb ->
      match ta, tb with
      | Item_t (tva, ra, _), Item_t (tvb, rb, _) ->
        ty_eq ctxt tva tvb |>
        record_trace (Bad_stack_item lvl) >>? fun (Eq, ctxt) ->
        stack_ty_eq ctxt (lvl + 1) ra rb >>? fun (Eq, ctxt) ->
        (Ok (Eq, ctxt) : ((ta stack_ty, tb stack_ty) eq * context) tzresult)
      | Empty_t, Empty_t -> Ok (Eq, ctxt)
      | _, _ -> error Bad_stack_length



  open Script_typed_ir
  open Protocol.Environment.Error_monad
  module Unparse_costs = Michelson_v1_gas.Cost_of.Unparse
  open Protocol.Environment

  type ex_typed_value =
    Ex_typed_value : ('a Script_typed_ir.ty * 'a) -> ex_typed_value

  (* TODO copied from Script_ir_translator *)
  let has_big_map : type t. t Script_typed_ir.ty -> bool = function
    | Unit_t _ ->
        false
    | Int_t _ ->
        false
    | Nat_t _ ->
        false
    | Signature_t _ ->
        false
    | String_t _ ->
        false
    | Bytes_t _ ->
        false
    | Mutez_t _ ->
        false
    | Key_hash_t _ ->
        false
    | Key_t _ ->
        false
    | Timestamp_t _ ->
        false
    | Address_t _ ->
        false
    | Bool_t _ ->
        false
    | Lambda_t (_, _, _) ->
        false
    | Set_t (_, _) ->
        false
    | Big_map_t (_, _, _) ->
        true
    | Contract_t (_, _) ->
        false
    | Operation_t _ ->
        false
    | Chain_id_t _ ->
        false
    | Pair_t (_, _, _, has_big_map) ->
        has_big_map
    | Union_t (_, _, _, has_big_map) ->
        has_big_map
    | Option_t (_, _, has_big_map) ->
        has_big_map
    | List_t (_, _, has_big_map) ->
        has_big_map
    | Map_t (_, _, _, has_big_map) ->
        has_big_map

  let rec ty_of_comparable_ty : type a s. (a, s) comparable_struct -> a ty =
    function
    | Int_key tname ->
        Int_t tname
    | Nat_key tname ->
        Nat_t tname
    | String_key tname ->
        String_t tname
    | Bytes_key tname ->
        Bytes_t tname
    | Mutez_key tname ->
        Mutez_t tname
    | Bool_key tname ->
        Bool_t tname
    | Key_hash_key tname ->
        Key_hash_t tname
    | Timestamp_key tname ->
        Timestamp_t tname
    | Address_key tname ->
        Address_t tname
    | Pair_key ((l, al), (r, ar), tname) ->
        Pair_t
          ( (ty_of_comparable_ty l, al, None),
            (ty_of_comparable_ty r, ar, None),
            tname,
            false )

module Interp_costs = Michelson_v1_gas.Cost_of
type ex_descr_stack = Ex_descr_stack : (('a, 'b) descr * 'a stack) -> ex_descr_stack

let unparse_stack ctxt (stack, stack_ty) =
  (* We drop the gas limit as this function is only used for debugging/errors. *)
  let ctxt = Gas.set_unlimited ctxt in
  let rec unparse_stack
    : type a. a stack * a stack_ty -> (Script.expr * string option) list tzresult Lwt.t
    = function
      | Empty, Empty_t -> return_nil
      | Item (v, rest), Item_t (ty, rest_ty, annot) ->
          unparse_data ctxt Readable ty v >>=? fun (data, _ctxt) ->
          unparse_stack (rest, rest_ty) >>=? fun rest ->
          let annot = match Script_ir_annot.unparse_var_annot annot with
            | [] -> None
            | [ a ] -> Some a
            | _ -> assert false in
          let data = Micheline.strip_locations data in
          return ((data, annot) :: rest) in
  unparse_stack (stack, stack_ty)

end

open X_error_monad

let stack_ty_eq (type a b)
    ?(tezos_context = dummy_environment.tezos_context)
    (a:a stack_ty) (b:b stack_ty) =
  alpha_wrap (X.stack_ty_eq tezos_context 0 a b) >>? fun (Eq, _) ->
  ok Eq

let ty_eq (type a b)
    ?(tezos_context = dummy_environment.tezos_context)
    (a:a ty) (b:b ty)
  =
  alpha_wrap (Script_ir_translator.ty_eq tezos_context a b) >>? fun (Eq, _) ->
  ok Eq

(* should not need lwt *)
let prims_of_strings michelson =
  let (michelson, errs) =
    Tezos_client_ligo006_PsCARTHA.Michelson_v1_macros.expand_rec michelson in
  match errs with
  | _ :: _ ->
    Lwt.return (Error errs)
  | [] ->
  Lwt.return
    (alpha_wrap
       (Michelson_v1_primitives.prims_of_strings
          (Tezos_micheline.Micheline.strip_locations michelson))) >>=? fun michelson ->
  return (Tezos_micheline.Micheline.root michelson)

let parse_michelson (type aft)
    ?(tezos_context = dummy_environment.tezos_context)
    ?(top_level = Lambda) (michelson:Michelson.t)
    ?type_logger
    (bef:'a Script_typed_ir.stack_ty) (aft:aft Script_typed_ir.stack_ty)
  =
  prims_of_strings michelson >>=? fun michelson ->
  parse_instr
    ?type_logger
    top_level tezos_context
    michelson bef ~legacy:false >>=?? fun (j, _) ->
  match j with
  | Typed descr -> (
      Lwt.return (
        alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
        let descr : (_, aft) Script_typed_ir.descr = {descr with aft} in
        Ok descr
      )
    )
  | _ -> Lwt.return @@ error_exn (Failure "Typing instr failed")

let parse_michelson_fail (type aft)
    ?(tezos_context = dummy_environment.tezos_context)
    ?(top_level = Lambda) (michelson:Michelson.t)
    ?type_logger
    (bef:'a Script_typed_ir.stack_ty) (aft:aft Script_typed_ir.stack_ty)
  =
  prims_of_strings michelson >>=? fun michelson ->
  parse_instr
    ?type_logger
    top_level tezos_context
    michelson bef ~legacy:false >>=?? fun (j, _) ->
  match j with
  | Typed descr -> (
      Lwt.return (
        alpha_wrap (X.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
        let descr : (_, aft) Script_typed_ir.descr = {descr with aft} in
        Ok descr
      )
    )
  | Failed { descr } ->
      Lwt.return (Ok (descr aft))

let parse_michelson_data
    ?(tezos_context = dummy_environment.tezos_context)
    michelson ty =
  parse_data tezos_context ty michelson ~legacy:false >>=?? fun (data, _) ->
  return data

let parse_michelson_ty
    ?(tezos_context = dummy_environment.tezos_context)
    ?(allow_big_map = true) ?(allow_operation = true) ?(allow_contract = true)
    michelson =
  Lwt.return @@ parse_ty tezos_context ~allow_big_map ~allow_operation michelson ~legacy:false ~allow_contract >>=?? fun (ty, _) ->
  return ty

let strings_of_prims michelson =
  let michelson = Tezos_micheline.Micheline.strip_locations michelson in
  let michelson = Michelson_v1_primitives.strings_of_prims michelson in
  Tezos_micheline.Micheline.root michelson

let unparse_michelson_data
    ?(tezos_context = dummy_environment.tezos_context)
    ty value : Michelson.t tzresult Lwt.t =
  unparse_data tezos_context
    Readable ty value >>=?? fun (michelson, _) ->
  return (strings_of_prims michelson)

let unparse_michelson_ty
    ?(tezos_context = dummy_environment.tezos_context)
    ty : Michelson.t tzresult Lwt.t =
  Script_ir_translator.unparse_ty tezos_context ty >>=?? fun (michelson, _) ->
  return (strings_of_prims michelson)

type options = {
  tezos_context: Alpha_context.t ;
  source: Alpha_context.Contract.t ;
  payer: Alpha_context.Contract.t ;
  self: Alpha_context.Contract.t ;
  amount: Alpha_context.Tez.t ;
  chain_id: Environment.Chain_id.t ;
  balance : Alpha_context.Tez.t;
  now : Alpha_context.Script_timestamp.t;
}

let make_options
    ?(tezos_context = dummy_environment.tezos_context)
    ?(now = Alpha_context.Script_timestamp.now dummy_environment.tezos_context)
    ?(sender = (List.nth dummy_environment.identities 0).implicit_contract)
    ?(self = (List.nth dummy_environment.identities 0).implicit_contract)
    ?(source = (List.nth dummy_environment.identities 1).implicit_contract)
    ?(amount = Alpha_context.Tez.one)
    ?(balance = Alpha_context.Tez.zero)
    ?(chain_id = Environment.Chain_id.zero)
    ()
  =
  {
    tezos_context ;
    source = sender ;
    payer = source ;
    self ;
    amount ;
    chain_id ;
    balance ;
    now ;
  }

let default_options = make_options ()

let interpret ?(options = default_options) (instr:('a, 'b) descr) (bef:'a stack) : 'b stack tzresult Lwt.t  =
  let {
    tezos_context ;
    source ;
    self ;
    payer ;
    amount ;
    chain_id ;
    balance ;
    now ;
  } = options in
  let step_constants = { source ; self ; payer ; amount ; chain_id ; balance ; now } in
  Script_interpreter.step tezos_context step_constants instr bef >>=??
  fun (stack, _) -> return stack

let unparse_ty_michelson ty =
  Script_ir_translator.unparse_ty dummy_environment.tezos_context ty >>=??
  fun (n,_) -> return n

type typecheck_res =
  | Type_checked
  | Err_parameter | Err_storage | Err_contract
  | Err_gas
  | Err_unknown

let typecheck_contract contract =
  let contract' = Tezos_micheline.Micheline.strip_locations contract in
  Script_ir_translator.typecheck_code dummy_environment.tezos_context contract' >>= fun x ->
  match x with
  | Ok _ -> return @@ contract
  | Error errs -> Lwt.return @@ Error (List.map (alpha_error_wrap) errs)

let assert_equal_michelson_type ty1 ty2 =
  (* alpha_wrap (Script_ir_translator.ty_eq tezos_context a b) >>? fun (Eq, _) -> *)
  alpha_wrap (Script_ir_translator.ty_eq dummy_environment.tezos_context ty1 ty2)

type 'a interpret_res =
  | Succeed of 'a stack
  | Fail of Script_repr.expr

let failure_interpret
    ?(options = default_options)
    (instr:('a, 'b) descr)
    (bef:'a stack) : 'b interpret_res tzresult Lwt.t =
  let {
    tezos_context ;
    source ;
    self ;
    payer ;
    amount ;
    chain_id ;
    balance ;
    now ;
  } = options in
  let step_constants = { source ; self ; payer ; amount ; chain_id ; balance ; now } in
  Script_interpreter.step tezos_context step_constants instr bef >>= fun x ->
  match x with
  | Ok (s , _ctxt) -> return @@ Succeed s
  | Error ((Reject (_, expr, _))::_t) -> return @@ Fail expr (* This catches failwith errors *)
  | Error errs -> Lwt.return @@ Error (List.map (alpha_error_wrap) errs)

let pack (data_ty: 'a ty) (data: 'a) : bytes tzresult Lwt.t =
  pack_data dummy_environment.tezos_context data_ty data >>=?? fun (packed,_) -> return packed

let strings_of_prims = Michelson_v1_primitives.strings_of_prims

let to_hex : Michelson.t -> Hex.t = fun michelson ->
  let michelson =
    X_error_monad.force_lwt ~msg:"Internal error: could not serialize Michelson"
      (prims_of_strings michelson) in
  let canonical = Tezos_micheline.Micheline.strip_locations michelson in
  let bytes = Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding canonical in
  Hex.of_bytes bytes