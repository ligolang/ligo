open Cli_expect

(* avoid pretty printing *)

let () = Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "jsligo"; "Bytes.X()" ];
  [%expect
    {|
    Ill-formed selection in nested namespaces.
    At this point, the selection operator '.' is expected. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "export_attr_const.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/export_attr_const.jsligo", line 2, characters 12-18:
        1 | @no_mutation export
        2 | const toto: D.titi = E.toto;
                        ^^^^^^

       Module "D" not found. |}]
