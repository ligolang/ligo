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

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "export_attr_const.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/export_attr_const.jsligo", line 2, characters 12-18:
        1 | export /* @no_mutation */
        2 | const toto: D.titi = E.toto;

       Module "D" not found. |}]
