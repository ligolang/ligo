open Core
module Error_monad = Tezos_error_monad.Error_monad

let to_string err =
  let json = Error_monad.json_of_error err in
  Data_encoding.Json.to_string json

let print err =
  Format.printf "%s\n" @@ to_string err;
  Format.print_flush ()

let force_ok ?(msg = "") = function
  | Ok x -> x
  | Error errs ->
    Format.printf "Errors :\n";
    List.iter ~f:print errs;
    print_endline ("Errors: " ^ (List.length errs |> string_of_int));
    List.iter ~f:(fun err -> print_endline (to_string err)) errs;
    raise @@ Failure ("force_ok : " ^ msg)

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let force_ok_str ?(msg = "") = function
  | Ok x -> x
  | Error err ->
    Format.printf "Error : %s\n" err;
    raise @@ Failure ("force_ok : " ^ msg)
