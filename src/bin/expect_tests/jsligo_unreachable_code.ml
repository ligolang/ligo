open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "jsligo_unreachable_code.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 8, characters 6-71:
      7 |   return "end";
      8 |   let _x = "Unreachable post-return code that should trigger a warning"
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 | };

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 17, characters 8-73:
     16 |     return "b";
     17 |     let _x = "Unreachable post-return code that should trigger a warning"
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     18 |   }

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 34, characters 6-76:
     33 |       return "end";
     34 |       output = "Unreachable post-return code that should trigger a warnings";
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     35 |     case 4:

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 38, characters 6-73:
     37 |       break;
     38 |       return "Unreachable post-break code that should trigger a warnings";
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     39 |     default:

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 27, characters 6-75:
     26 |       break;
     27 |       output = "Unreachable post-break code that should trigger a warnings";
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     28 |     case 2:

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 47, characters 2-70:
     46 |   return "end";
     47 |   return "Unreachable post-return code that should trigger a warnings"
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     48 | };

    Warning: Unreachable code.
    { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]
