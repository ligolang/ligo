open Cli_expect

(* avoid pretty printing *)
let () = Core_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_syntax.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_syntax.ligo", line 1, characters 16-17:
      1 | type foo is bar - 42
    Ill-formed contract.
    At this point, if the top-level declaration is complete, one of the
    following is expected:
      * another declaration starting with the keyword 'type', 'const',
        'function', 'recursive' or 'module';
      * the end of the file.
    Note: 'var' declarations are valid only in blocks. |} ]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression" ; "jsligo" ; "Bytes.X()" ] ;
  [%expect {|
    Ill-formed selection of a value in a module.
    At this point, the selection symbol '.' is expected, followed by the
    qualified name of a value. |} ]

