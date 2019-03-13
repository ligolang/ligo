module Error_monad = Tezos_error_monad.Error_monad
include Error_monad

let to_string err =
  let json = json_of_error err in
  Tezos_data_encoding.Json.to_string json

let print err =
  Format.printf "%s\n" @@ to_string err

let force_ok ?(msg = "") = function
  | Ok x -> x
  | Error errs ->
    Format.printf "Errors :\n";
    List.iter print errs ;
    raise @@ Failure ("force_ok : " ^ msg)

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let force_ok_str ?(msg = "") = function
  | Ok x -> x
  | Error err ->
    Format.printf "Error : %s\n" err;
    raise @@ Failure ("force_ok : " ^ msg)

open Memory_proto_alpha

let (>>??) = Alpha_environment.Error_monad.(>>?)

let alpha_wrap a = Alpha_environment.wrap_error a

let force_ok_alpha ~msg a = force_ok ~msg @@ alpha_wrap a

let force_lwt ~msg a = force_ok ~msg @@ Lwt_main.run a

let force_lwt_alpha ~msg a = force_ok ~msg @@ alpha_wrap @@ Lwt_main.run a

let assert_error () = function
  | Ok _ -> fail @@ failure "assert_error"
  | Error _ -> return ()

let (>>=??) a f =
  a >>= fun a ->
  match alpha_wrap a with
  | Ok result -> f result
  | Error errs -> Lwt.return (Error errs)


