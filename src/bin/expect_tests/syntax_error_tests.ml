open Cli_expect

(* avoid pretty printing *)

let () = Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "jsligo"; "Bytes.X()" ];
  [%expect
    {|
    Ill-formed selection of a value in a module.
    At this point, the selection symbol '.' is expected, followed by the
    qualified name of a value. |}]
