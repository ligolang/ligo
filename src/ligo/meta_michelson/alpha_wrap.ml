open Tezos_utils.Error_monad

let dummy_environment = force_lwt ~msg:"getting dummy env" @@ Misc.init_environment ()

let tc = dummy_environment.tezos_context

module Proto_alpha = Tezos_utils.Memory_proto_alpha
open Proto_alpha
open Alpha_context
open Alpha_environment

let pack ty v = fst @@ force_lwt_alpha ~msg:"packing" @@ Script_ir_translator.pack_data tc ty v
let unpack_opt (type a) : a Script_typed_ir.ty -> MBytes.t -> a option = fun ty bytes ->
  force_lwt ~msg:"unpacking : parse" (
    if Compare.Int.(MBytes.length bytes >= 1) &&
       Compare.Int.(MBytes.get_uint8 bytes 0 = 0x05) then
      let bytes = MBytes.sub bytes 1 (MBytes.length bytes - 1) in
      match Data_encoding.Binary.of_bytes Script.expr_encoding bytes with
      | None -> return None
      | Some expr ->
        Script_ir_translator.parse_data tc ty (Micheline.root expr) >>=?? fun x -> return (Some (fst x))
    else
      return None
  )

let unpack ty a = match unpack_opt ty a with
  | None -> raise @@ Failure "unpacking : of_bytes"
  | Some x -> x

let blake2b b = Alpha_environment.Raw_hashes.blake2b b
