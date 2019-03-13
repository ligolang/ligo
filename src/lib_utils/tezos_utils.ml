module Stdlib_unix = Tezos_stdlib_unix
module Crypto = Tezos_crypto
module Data_encoding = Tezos_data_encoding
module Error_monad = X_error_monad
module Signature = Tezos_base.TzPervasives.Signature
module Time = Tezos_base.TzPervasives.Time
module List = X_list
module Option = Tezos_base.TzPervasives.Option
module Cast = Cast
module Micheline = X_tezos_micheline
module Tuple = Tuple

module Memory_proto_alpha = struct
  include Memory_proto_alpha
  let init_environment = Init_proto_alpha.init_environment
  let dummy_environment = Init_proto_alpha.dummy_environment

  open X_error_monad
  open Script_typed_ir
  open Script_ir_translator
  open Script_interpreter

  let stack_ty_eq (type a b)
      ?(tezos_context = dummy_environment.tezos_context)
      (a:a stack_ty) (b:b stack_ty) =
    alpha_wrap (Script_ir_translator.stack_ty_eq tezos_context 0 a b) >>? fun (Eq, _) ->
    ok Eq

  let ty_eq (type a b)
      ?(tezos_context = dummy_environment.tezos_context)
      (a:a ty) (b:b ty)
    =
    alpha_wrap (Script_ir_translator.ty_eq tezos_context a b) >>? fun (Eq, _) ->
    ok Eq

  let parse_michelson (type aft)
        ?(tezos_context = dummy_environment.tezos_context)
        ?(top_level = Lambda) (michelson:Micheline.Michelson.t)
        (bef:'a Script_typed_ir.stack_ty) (aft:aft Script_typed_ir.stack_ty)
    =
    let michelson = Micheline.Michelson.strip_annots michelson in
    let michelson = Micheline.Michelson.strip_nops michelson in
    parse_instr
      top_level tezos_context
      michelson bef >>=?? fun (j, _) ->
    match j with
    | Typed descr -> (
      Lwt.return (
          alpha_wrap (Script_ir_translator.stack_ty_eq tezos_context 0 descr.aft aft) >>? fun (Eq, _) ->
          let descr : (_, aft) Script_typed_ir.descr = {descr with aft} in
          Ok descr
        )
    )
    | _ -> Lwt.return @@ error_exn (Failure "Typing instr failed")

  let parse_michelson_data
      ?(tezos_context = dummy_environment.tezos_context)
      michelson ty =
    let michelson = Micheline.Michelson.strip_annots michelson in
    let michelson = Micheline.Michelson.strip_nops michelson in
    parse_data tezos_context ty michelson >>=?? fun (data, _) ->
    return data

  let parse_michelson_ty
      ?(tezos_context = dummy_environment.tezos_context)
      ?(allow_big_map = true) ?(allow_operation = true)
      michelson =
    let michelson = Micheline.Michelson.strip_annots michelson in
    let michelson = Micheline.Michelson.strip_nops michelson in
    Lwt.return @@ parse_ty tezos_context ~allow_big_map ~allow_operation michelson >>=?? fun (ty, _) ->
    return ty

  let unparse_michelson_data
      ?(tezos_context = dummy_environment.tezos_context)
      ?mapper ty value : Micheline.Michelson.t tzresult Lwt.t =
    Script_ir_translator.unparse_data tezos_context ?mapper
      Readable ty value >>=?? fun (michelson, _) ->
    return michelson

  let interpret
      ?(tezos_context = dummy_environment.tezos_context)
      ?(source = (List.nth dummy_environment.identities 0).implicit_contract)
      ?(self = (List.nth dummy_environment.identities 0).implicit_contract)
      ?(payer = (List.nth dummy_environment.identities 1).implicit_contract)
      ?visitor
      (instr:('a, 'b) descr) (bef:'a stack) : 'b stack tzresult Lwt.t  =
    Script_interpreter.step tezos_context ~source ~self ~payer ?visitor Alpha_context.Tez.one instr bef >>=??
    fun (stack, _) -> return stack

end
