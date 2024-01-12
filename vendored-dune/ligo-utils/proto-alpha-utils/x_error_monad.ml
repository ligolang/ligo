include Tezos_error_monad.Error_monad
include Tezos_utils.Error_monad
open Memory_proto_alpha

let alpha_wrap a = Alpha_environment.wrap_tzresult a
let alpha_error_wrap x = Memory_proto_alpha.Alpha_environment.Ecoproto_error x
let force_ok_alpha ~msg a = force_ok ~msg @@ alpha_wrap a

let assert_error res =
  let open Lwt_result_syntax in
  let* res in
  match res with
  | Ok _ -> assert false
  | Error _ -> return_unit

let ( >>=?? ) a f =
  let open Lwt_syntax in
  let* a in
  match alpha_wrap a with
  | Ok result -> f result
  | Error errs -> Lwt.return (Error errs)
