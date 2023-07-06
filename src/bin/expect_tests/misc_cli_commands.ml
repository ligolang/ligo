open Cli_expect

(* list-declarations *)

let%expect_test _ =
  run_ligo_good
    [ "info"
    ; "list-declarations"
    ; "../../test/contracts/entry_contract_for_list_declaration.jsligo"
    ; "--only-ep"
    ];
  [%expect
    {|
    ../../test/contracts/entry_contract_for_list_declaration.jsligo declarations:
    Foo.$main
    Foo.reset
    Foo.decrement
    Foo.increment |}]
