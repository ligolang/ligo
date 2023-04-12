open Cli_expect

(* ---------- Basic transpilation test -------------------------------------- *)
let%expect_test _ =
  run_ligo_good [ "transpile"; "contract"; test "example.ligo"; "--to-syntax" ; "jsligo" ];
  [%expect
    {|
    # 1 "../../test/contracts/example.ligo"
    # 1 "../../test/contracts/example.ligo"
    type storage = int;

    type parameter =
      ["Increment", int] | ["Decrement", int] | ["Reset"];

    type @return = [list<operation>, storage];

    const add = (store: storage, delta: int): storage =>
      store + delta;

    const sub = (store: storage, delta: int): storage =>
      store - delta;

    const main = (action: parameter, store: storage): @return =>
      [
        list([]) as list<operation>,
        match(
          action,
          {
            Increment: n => add(store, n),
            Decrement: n => sub(store, n),
            Reset: () => 0
          }
        )
      ]; |}]


(* ---------- Tests for CLI options for syntax ------------------------------ *)

(* Should fail since neither [--to-syntax] nor output file is provided *)
let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; ];
  [%expect {|
    Transpilation target syntax is not specified.
    Please provide it using the --to-syntax option
    or by specifying an output file with the -o option |}]

(* Should suceed since output file is explicitely provided,
   even if [--to-syntax] is not *)
let%expect_test _ =
  run_ligo_good [ "transpile"; "contract"; test "example.ligo"; "-o" ; "dest.jsligo" ];
  [%expect {| |}]

(* ---------- Tests for (un)supported syntaxes ------------------------------ *)

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; "--to-syntax" ; "pascaligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Source and destination of transpilation are the same (pascaligo). |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; "--to-syntax" ; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from pascaligo to cameligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.mligo"; "--to-syntax" ; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Source and destination of transpilation are the same (cameligo). |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.mligo"; "--to-syntax" ; "pascaligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from cameligo to pascaligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.mligo"; "--to-syntax" ; "jsligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from cameligo to jsligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.jsligo"; "--to-syntax" ; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from jsligo to cameligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.jsligo"; "--to-syntax" ; "pascaligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from jsligo to pascaligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.jsligo"; "--to-syntax" ; "jsligo" ];
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
    File "../../test/contracts/transpile_warn_shadowing.jsligo", line 12, characters 6-7:
     11 | const x = 42;
     12 | const x = 33;
     13 |

    Duplicate identifier. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "--transpiled"; contract_warn_shadowing ];
  [%expect {| { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]