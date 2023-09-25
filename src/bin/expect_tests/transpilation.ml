open Cli_expect

(* ---------- Basic transpilation test -------------------------------------- *)
let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; "--to-syntax"; "jsligo" ];
  [%expect
    {|
    Invalid file extension for '../../test/contracts/example.ligo'.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

(* ---------- Tests for CLI options for syntax ------------------------------ *)

(* Should fail since neither [--to-syntax] nor output file is provided *)
let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo" ];
  [%expect
    {|
    Invalid file extension for '../../test/contracts/example.ligo'.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

(* Should suceed since output file is explicitely provided,
   even if [--to-syntax] is not *)
let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; "-o"; "dest.jsligo" ];
  [%expect
    {|
    Invalid file extension for '../../test/contracts/example.ligo'.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

(* ---------- Tests for (un)supported syntaxes ------------------------------ *)

let%expect_test _ =
  run_ligo_bad
    [ "transpile"; "contract"; test "example.ligo"; "--to-syntax"; "pascaligo" ];
  [%expect
    {|
    Invalid file extension for '../../test/contracts/example.ligo'.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; "--to-syntax"; "cameligo" ];
  [%expect
    {|
    Invalid file extension for '../../test/contracts/example.ligo'.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

let%expect_test _ =
  run_ligo_bad
    [ "transpile"; "contract"; test "example.mligo"; "--to-syntax"; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Source and destination of transpilation are the same (cameligo). |}]

let%expect_test _ =
  run_ligo_bad
    [ "transpile"; "contract"; test "example.mligo"; "--to-syntax"; "pascaligo" ];
  [%expect
    {|
    Invalid syntax.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.mligo"; "--to-syntax"; "jsligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from cameligo to jsligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad
    [ "transpile"; "contract"; test "example.jsligo"; "--to-syntax"; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from jsligo to cameligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad
    [ "transpile"; "contract"; test "example.jsligo"; "--to-syntax"; "pascaligo" ];
  [%expect
    {|
    Invalid syntax.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.jsligo"; "--to-syntax"; "jsligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Source and destination of transpilation are the same (jsligo). |}]

(* ---------- Tests for warning disabling ----------------------------------- *)

let contract_warn_shadowing = test "transpile_warn_shadowing.jsligo"

(* Adding the [--transpiled] flag should disable the error message about shadowed variables *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract_warn_shadowing ];
  [%expect
    {|
    File "../../test/contracts/transpile_warn_shadowing.jsligo", line 13, characters 6-7:
     12 |
     13 | const x = 33;
                ^
     14 |

    Duplicate identifier. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "--transpiled"; contract_warn_shadowing ];
  [%expect {| { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]
