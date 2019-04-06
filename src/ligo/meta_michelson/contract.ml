open Misc

open Tezos_utils.Error_monad
open Memory_proto_alpha
open Alpha_context

open Script_ir_translator
open Script_typed_ir

module Option = Tezos_utils.Option
module Cast = Tezos_utils.Cast

type ('param, 'storage) toplevel = {
  param_type : 'param ty ;
  storage_type : 'storage ty ;
  code : ('param * 'storage, packed_internal_operation list * 'storage) lambda
}

type ex_toplevel =
    Ex_toplevel : ('a, 'b) toplevel -> ex_toplevel

let get_toplevel ?environment toplevel_path claimed_storage_type claimed_parameter_type =
  let toplevel_str = Streams.read_file toplevel_path in
  contextualize ?environment ~msg:"toplevel" @@ fun {tezos_context = context ; _ } ->
  let toplevel_expr = Cast.tl_of_string toplevel_str in
  let (param_ty_node, storage_ty_node, code_field) =
    force_ok_alpha ~msg:"parsing toplevel" @@
    parse_toplevel toplevel_expr in
  let (Ex_ty param_type, _) =
    force_ok_alpha ~msg:"parse arg ty" @@
    Script_ir_translator.parse_ty context ~allow_big_map:false ~allow_operation:false param_ty_node in
  let (Ex_ty storage_type, _) =
    force_ok_alpha ~msg:"parse storage ty" @@
    parse_storage_ty context storage_ty_node in
  let _ = force_ok_alpha ~msg:"storage eq" @@ Script_ir_translator.ty_eq context storage_type claimed_storage_type in
  let _ = force_ok_alpha ~msg:"param eq" @@ Script_ir_translator.ty_eq context param_type claimed_parameter_type in
  let param_type_full = Pair_t ((claimed_parameter_type, None, None),
                                (claimed_storage_type, None, None), None) in
  let ret_type_full =
    Pair_t ((List_t (Operation_t None, None), None, None),
            (claimed_storage_type, None, None), None) in
  parse_returning (Toplevel { storage_type = claimed_storage_type ; param_type = claimed_parameter_type })
    context (param_type_full, None) ret_type_full code_field >>=?? fun (code, _) ->
  Error_monad.return {
    param_type = claimed_parameter_type;
    storage_type = claimed_storage_type;
    code ;
  }

let make_toplevel code storage_type param_type =
  { param_type ; storage_type ; code }

module type ENVIRONMENT = sig
  val identities : identity list
  val tezos_context : t
end

type ex_typed_stack = Ex_typed_stack : ('a stack_ty * 'a Script_interpreter.stack) -> ex_typed_stack

open Error_monad

module Step (Env: ENVIRONMENT) = struct
  open Env

  type config = {
    source : Contract.t option ;
    payer : Contract.t option ;
    self : Contract.t option ;
    visitor : (Script_interpreter.ex_descr_stack -> unit) option ;
    timestamp : Script_timestamp.t option ;
    debug_visitor : (ex_typed_stack -> unit) option ;
    amount : Tez.t option ;
  }

  let no_config = {
    source = None ;
    payer = None ;
    self = None ;
    visitor = None ;
    debug_visitor = None ;
    timestamp = None ;
    amount = None ;
  }

  let of_param base param = match param with
    | None -> base
    | Some _ as x -> x

  let make_config ?base_config ?source ?payer ?self ?visitor ?debug_visitor ?timestamp ?amount () =
    let base_config = Option.unopt ~default:no_config base_config in {
      source = Option.first_some source base_config.source ;
      payer = Option.first_some payer base_config.payer ;
      self = Option.first_some self base_config.self ;
      visitor = Option.first_some visitor base_config.visitor ;
      debug_visitor = Option.first_some debug_visitor base_config.debug_visitor ;
      timestamp = Option.first_some timestamp base_config.timestamp ;
      amount = Option.first_some amount base_config.amount ;
    }

  open Error_monad

  let debug_visitor ?f () =
    let open Script_interpreter in
    let aux (Ex_descr_stack (descr, stack)) =
      (match (descr.instr, descr.bef) with
       | Nop, Item_t (String_t _, stack_ty, _) -> (
           let (Item (s, stack)) = stack in
           if s = "_debug"
           then (
             match f with
             | None -> Format.printf "debug: %s\n%!" @@ Cast.stack_to_string stack_ty stack
             | Some f -> f (Ex_typed_stack(stack_ty, stack))
           ) else ()
         )
       | _ -> ()) ;
      () in
    aux

  let step_lwt ?(config=no_config) (stack:'a Script_interpreter.stack) (code:('a, 'b) descr) =
    let source = Option.unopt
        ~default:(List.nth identities 0).implicit_contract config.source in
    let payer = Option.unopt
        ~default:(List.nth identities 1).implicit_contract config.payer in
    let self = Option.unopt
        ~default:(List.nth identities 2).implicit_contract config.self in
    let amount = Option.unopt ~default:(Tez.one) config.amount in
    let visitor =
      let default = debug_visitor ?f:config.debug_visitor () in
      Option.unopt ~default config.visitor in
    let tezos_context = match config.timestamp with
      | None -> tezos_context
      | Some s -> Alpha_context.Script_timestamp.set_now tezos_context s in
    Script_interpreter.step tezos_context ~source ~payer ~self ~visitor amount code stack >>=?? fun (stack, _) ->
    return stack

  let step_1_2 ?config (a:'a) (descr:('a * end_of_stack, 'b * ('c * end_of_stack)) descr) =
    let open Script_interpreter in
    step_lwt ?config (Item(a, Empty)) descr >>=? fun (Item(b, Item(c, Empty))) ->
    return (b, c)

  let step_3_1 ?config (a:'a) (b:'b) (c:'c)
      (descr:('a * ('b * ('c * end_of_stack)), 'd * end_of_stack) descr) =
    let open Script_interpreter in
    step_lwt ?config (Item(a, Item(b, Item(c, Empty)))) descr >>=? fun (Item(d, Empty)) ->
    return d

  let step_2_1 ?config (a:'a) (b:'b) (descr:('a * ('b * end_of_stack), 'c * end_of_stack) descr) =
    let open Script_interpreter in
    step_lwt ?config (Item(a, Item(b, Empty))) descr >>=? fun (Item(c, Empty)) ->
    return c

  let step_1_1 ?config (a:'a) (descr:('a * end_of_stack, 'b * end_of_stack) descr) =
    let open Script_interpreter in
    step_lwt ?config (Item(a, Empty)) descr >>=? fun (Item(b, Empty)) ->
    return b

  let step_value ?config (a:'a) (descr:('a * end_of_stack, 'a * end_of_stack) descr) =
    step_1_1 ?config a descr

  let step ?config stack code =
    force_lwt ~msg:"running a step" @@ step_lwt ?config stack code

end

let run_lwt_full ?source ?payer ?self toplevel storage param {identities ; tezos_context = context} =
  let { code ; _ } : (_, _) toplevel = toplevel in

  let source = Option.unopt
      ~default:(List.nth identities 0).implicit_contract source in
  let payer = Option.unopt
      ~default:(List.nth identities 1).implicit_contract payer in
  let self = Option.unopt
      ~default:(List.nth identities 2).implicit_contract self in
  let amount = Tez.one in

  Script_interpreter.interp context ~source ~payer ~self amount code (param, storage)
  >>=?? fun ((ops, storage), new_ctxt) ->
  let gas = Alpha_context.Gas.consumed ~since:context ~until:new_ctxt in
  return (storage, ops, gas)

let run_lwt ?source ?payer ?self toplevel storage param env =
  run_lwt_full ?source ?payer ?self toplevel storage param env >>=? fun (storage, _ops, _gas) ->
  return storage

let run ?environment toplevel storage param =
  contextualize ?environment ~msg:"run toplevel" @@ run_lwt toplevel storage param

let run_node ?environment toplevel storage_node param_node =
  contextualize ?environment ~msg:"run toplevel" @@ fun {tezos_context = context ; _} ->
  let {param_type ; storage_type ; _ } = toplevel in
  parse_data context param_type param_node >>=?? fun (param, _) ->
  parse_data context storage_type storage_node >>=?? fun (storage, _) ->
  let storage = run toplevel storage param in
  unparse_data context Readable storage_type storage >>=?? fun (storage_node, _) ->
  return storage_node

let run_str toplevel storage_str param_str =
  let param_node = Cast.node_of_string param_str in
  let storage_node = Cast.node_of_string storage_str in
  run_node toplevel storage_node param_node

type input = {
  toplevel_path : string ;
  storage : string ;
  parameter : string
}

let parse_json json_str : input =
  let json = force_ok_str ~msg:"main_contract: invalid json" @@ Tezos_utils.Data_encoding.Json.from_string json_str in
  let json = match json with
    | `O json ->  json
    | _ -> raise @@ Failure "main_contract: not recorD"
  in
  let open Json in
  let toplevel_path = force_string ~msg:"main_contract, top_level" @@ List.assoc "top_level" json in
  let parameter = force_string ~msg:"main_contract, param" @@ List.assoc "param" json in
  let storage = force_string ~msg:"main_contract, storage" @@ List.assoc "storage" json in
  { toplevel_path ; storage ; parameter }

let generate_json (storage_node:Script.node) : string =
  let storage_expr = Tezos_micheline.Micheline.strip_locations storage_node in
  let json = Data_encoding.Json.construct Script.expr_encoding storage_expr in
  Format.fprintf Format.str_formatter "%a" Data_encoding.Json.pp json ;
  Format.flush_str_formatter ()

module Types = struct
  open Script_typed_ir

  let union a b = Union_t ((a, None), (b, None), None)
  let assert_union = function
    | Union_t ((a, _), (b, _), _) -> (a, b)
    | _ -> assert false

  let pair a b = Pair_t ((a, None, None), (b, None, None), None)
  let assert_pair = function
    | Pair_t ((a, _, _), ((b, _, _)), _) -> (a, b)
    | _ -> assert false
  let assert_pair_ex ?(msg="assert pair") (Ex_ty ty) = match ty with
    | Pair_t ((a, _, _), ((b, _, _)), _) -> (Ex_ty a, Ex_ty b)
    | _ -> raise (Failure msg)

  let unit = Unit_t None

  let bytes = Bytes_t None
  let bytes_k = Bytes_key None

  let nat = Nat_t None
  let int = Int_t None
  let nat_k = Nat_key None
  let int_k = Int_key None

  let big_map k v = Big_map_t (k, v, None)

  let signature = Signature_t None

  let bool = Bool_t None

  let mutez = Mutez_t None

  let string = String_t None
  let string_k = String_key None

  let key = Key_t None

  let list a = List_t (a, None)
  let assert_list = function
    | List_t (a, _) -> a
    | _ -> assert false

  let option a = Option_t ((a, None), None, None)
  let assert_option = function
    | Option_t ((a, _), _, _) -> a
    | _ -> assert false

  let address = Address_t None

  let lambda a b = Lambda_t (a, b, None)
  let assert_lambda = function
    | Lambda_t (a, b, _) -> (a, b)
    | _ -> assert false
  type ex_lambda = Ex_lambda : (_, _) lambda ty -> ex_lambda
  let is_lambda : type a . a ty -> ex_lambda option = function
    | Lambda_t (_, _, _) as x -> Some (Ex_lambda x)
    | _ -> None

  let timestamp = Timestamp_t None
  let timestamp_k = Timestamp_key None

  let map a b = Map_t (a, b, None)

  let assert_type (_:'a ty) (_:'a) = ()
end

module Values = struct
  let empty_map t = empty_map t

  let empty_big_map key_type comparable_key_ty value_type : ('a, 'b) big_map = {
    key_type ; value_type ; diff = empty_map comparable_key_ty ;
  }

  let int n = Script_int.of_int n

  let nat n = Script_int.abs @@ Script_int.of_int n
  let nat_to_int n = Option.unopt_exn (Failure "nat_to_int") @@ Script_int.to_int n

  let tez n = Option.unopt_exn (Failure "Values.tez") @@ Tez.of_mutez @@ Int64.of_int n

  let left a = L a

  let right b = R b
end
