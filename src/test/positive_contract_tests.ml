open Test_helpers

let ends_with suffix str =
  let str_len = String.length str in
  let suffix_len = String.length suffix in
  if str_len < suffix_len
  then false
  else
    String.equal suffix (String.sub str ~pos:(str_len - suffix_len) ~len:suffix_len)

(* test that everything in src/test/contracts/positive typechecks and
   compiles (assuming entry point "main") *)
let positive_contract_tests =
  String.split ~on:' ' (match Sys.getenv "POSITIVE_CONTRACTS" with Some e -> e | None -> "") |>
  List.filter ~f:(fun path -> not (ends_with ".md" path)) |>
  List.map
    ~f:(fun path ->
      let run ~raise () =
        Test_helpers.compile_main ~raise path ()
      in
      test_w ("src/test/"^path) run)

let main = test_suite "Positive contracts" (positive_contract_tests)
