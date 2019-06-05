include Simple_utils.Trace

module AE = Memory_proto_alpha.Alpha_environment
module TP = Tezos_base__TzPervasives

let of_tz_error (err:X_error_monad.error) : error_thunk =
  let str () = X_error_monad.(to_string err) in
  error (thunk "alpha error") str

let of_alpha_tz_error err = of_tz_error (AE.Ecoproto_error err)

let trace_alpha_tzresult err : 'a AE.Error_monad.tzresult -> 'a result =
  function
  | Result.Ok x -> ok x
  | Error errs -> fail @@ thunk @@ patch_children (List.map of_alpha_tz_error errs) (err ())

let trace_alpha_tzresult_lwt error (x:_ AE.Error_monad.tzresult Lwt.t) : _ result =
  trace_alpha_tzresult error @@ Lwt_main.run x

let trace_tzresult err =
  function
  | Result.Ok x -> ok x
  | Error errs -> fail @@ thunk @@ patch_children (List.map of_tz_error errs) (err ())

(* TODO: should be a combination of trace_tzresult and trace_r *)
let trace_tzresult_r err_thunk_may_fail =
  function
  | Result.Ok x -> ok x
  | Error _errs ->
      (* let tz_errs = List.map of_tz_error errs in *)
      match err_thunk_may_fail () with
      | Simple_utils.Trace.Ok (err, annotations) -> ignore annotations; Error (err)
      | Error errors_while_generating_error ->
          (* TODO: the complexity could be O(n*n) in the worst case,
             this should use some catenable lists. *)
          Error (errors_while_generating_error)

let trace_tzresult_lwt err (x:_ TP.Error_monad.tzresult Lwt.t) : _ result =
  trace_tzresult err @@ Lwt_main.run x

let trace_tzresult_lwt_r err (x:_ TP.Error_monad.tzresult Lwt.t) : _ result =
  trace_tzresult_r err @@ Lwt_main.run x

