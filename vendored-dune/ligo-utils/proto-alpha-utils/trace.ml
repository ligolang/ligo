open Core
include Simple_utils.Trace
module AE = Memory_proto_alpha.Alpha_environment
module TP = Tezos_error_monad.Error_monad

type tezos_alpha_error = [ `Tezos_alpha_error of TP.error ]

let of_tz_error (err : X_error_monad.error) : tezos_alpha_error = `Tezos_alpha_error err

let trace_decoding_error
    :  (Data_encoding.Binary.read_error -> 'err)
    -> ('a, Data_encoding.Binary.read_error) Stdlib.result -> ('a, 'err) result
  =
 fun f err ->
  match err with
  | Ok x -> Ok x
  | Error err -> Error (f err)

let trace_alpha_tzresult
    :  raise:('b, 'w) raise -> (tezos_alpha_error list -> 'b)
    -> 'a AE.Error_monad.tzresult -> 'a
  =
 fun ~raise tracer err ->
  match err with
  | Ok x -> x
  | Error errs -> raise.error @@ tracer (List.map ~f:of_tz_error @@ AE.wrap_tztrace errs)

let trace_alpha_shell_tzresult
    :  raise:('b, 'w) raise -> (tezos_alpha_error list -> 'b)
    -> 'a AE.Error_monad.shell_tzresult -> 'a
  =
 fun ~raise tracer err ->
  match err with
  | Ok x -> x
  | Error errs -> raise.error @@ tracer (List.map ~f:of_tz_error @@ errs)

let trace_tzresult
    :  raise:('b, 'w) raise -> (tezos_alpha_error list -> _)
    -> ('a, TP.error list) Stdlib.result -> 'a
  =
 fun ~raise tracer err ->
  match err with
  | Ok x -> x
  | Error errs -> raise.error @@ tracer (List.map ~f:of_tz_error errs)

let tz_result_to_bool : ('a, TP.error list) Stdlib.result -> bool =
 fun err ->
  match err with
  | Ok _ -> true
  | Error _ -> false

let warn_on_tzresult
    :  raise:(_, 'a) raise -> (TP.error list -> 'a) -> ('b, TP.error list) Stdlib.result
    -> unit
  =
 fun ~raise f_warning err ->
  match err with
  | Ok _ -> ()
  | Error errs -> raise.warning (f_warning errs)
