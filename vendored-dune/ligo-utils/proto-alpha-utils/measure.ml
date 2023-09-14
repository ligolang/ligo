open X_error_monad
open Tezos_micheline.Micheline
open Memory_proto_alpha.Protocol

let measure michelson =
  let open Lwt_result_syntax in
  let* michelson = X_memory_proto_alpha.prims_of_strings michelson in
  let canonical = strip_locations michelson in
  let bytes = Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding canonical in
  return (Bytes.length bytes)
