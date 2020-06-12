include Simple_utils.Trace

module AE = Memory_proto_alpha.Alpha_environment
module TP = Tezos_base__TzPervasives

type tezos_alpha_error =  [`Tezos_alpha_error of TP.error]

let of_tz_error (err:X_error_monad.error) : tezos_alpha_error =
  `Tezos_alpha_error err

let of_alpha_tz_error err = of_tz_error (AE.Ecoproto_error err)

let trace_alpha_tzresult :
  (tezos_alpha_error list -> 'b) -> 'a AE.Error_monad.tzresult -> ('a, 'b) result =
  fun tracer err -> match err with
  | Ok x -> ok x
  | Error errs ->
    fail @@ tracer (List.map of_alpha_tz_error errs)

let trace_alpha_tzresult_lwt tracer (x:_ AE.Error_monad.tzresult Lwt.t) : _ result =
  trace_alpha_tzresult tracer @@ Lwt_main.run x

let trace_tzresult :
  (tezos_alpha_error list -> _) -> ('a, TP.error list) Pervasives.result -> ('a, _) result =
  fun tracer err -> match err with
  | Ok x -> ok x
  | Error errs -> fail @@ tracer (List.map of_tz_error errs)

let trace_tzresult_lwt err (x:_ TP.Error_monad.tzresult Lwt.t) : _ result =
  trace_tzresult err @@ Lwt_main.run x
