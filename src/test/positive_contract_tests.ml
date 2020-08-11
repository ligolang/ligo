open Trace
open Test_helpers

let ends_with suffix str =
  let str_len = String.length str in
  let suffix_len = String.length suffix in
  if str_len < suffix_len
  then false
  else
    String.equal suffix (String.sub str (str_len - suffix_len) suffix_len)

(* test that everything in src/test/contracts/positive typechecks and
   compiles (assuming entry point "main") *)
let positive_contract_tests =
  String.split_on_char ' ' (Sys.getenv "POSITIVE_CONTRACTS") |>
  List.filter (fun path -> not (ends_with ".md" path)) |>
  List.map
    (fun path ->
       let run () =
         let%bind prog = Ligo.Compile.Utils.type_file path "auto" Env in
         let%bind _michelson = typed_program_to_michelson prog "main" in
         ok () in
       test ("src/test/"^path) run)

let main = test_suite "Positive contracts" positive_contract_tests
