open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "jsligo_unreachable_code.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 9, characters 6-71:
      8 |   return "end";
      9 |   let _x = "Unreachable post-return code that should trigger a warning";
     10 | }

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 18, characters 8-73:
     17 |     return "b";
     18 |     let _x = "Unreachable post-return code that should trigger a warning";
     19 |   }

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 33, characters 6-76:
     32 |       return "end";
     33 |       output = "Unreachable post-return code that should trigger a warnings";
     34 |     case 4:

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 37, characters 6-73:
     36 |       break;
     37 |       return "Unreachable post-break code that should trigger a warnings";
     38 |     default:

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 28, characters 6-75:
     27 |       break;
     28 |       output = "Unreachable post-break code that should trigger a warnings";
     29 |     case 2:

    Warning: Unreachable code.
    File "../../test/contracts/jsligo_unreachable_code.jsligo", line 46, characters 2-70:
     45 |   return "end";
     46 |   return "Unreachable post-return code that should trigger a warnings";
     47 | }

    Warning: Unreachable code.
    { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]
