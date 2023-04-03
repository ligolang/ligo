open Cli_expect

let%expect_test _ =
  run_ligo_good [ "transpile"; "contract"; test "example.ligo"; "jsligo" ];
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

let example_jsligo = "../../test/examples/jsligo/arithmetic-contract.jsligo"

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; "pascaligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Source and destination of transpilation are the same (pascaligo). |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.ligo"; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from pascaligo to cameligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.mligo"; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Source and destination of transpilation are the same (cameligo). |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.mligo"; "pascaligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from cameligo to pascaligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.mligo"; "jsligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from cameligo to jsligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.jsligo"; "cameligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from jsligo to cameligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.jsligo"; "pascaligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Syntactic-level transpilation from jsligo to pascaligo is not supported. |}]

let%expect_test _ =
  run_ligo_bad [ "transpile"; "contract"; test "example.jsligo"; "jsligo" ];
  [%expect
    {|
    Invalid syntaxes.
    Source and destination of transpilation are the same (jsligo). |}]

let contract_warn_shadowing = test "transpile_warn_shadowing.jsligo"

(* Adding the [--transpiled] flag should disable the error message about shadowed variables *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract_warn_shadowing ];
  [%expect
    {|
    File "../../test/contracts/transpile_warn_shadowing.jsligo", line 12, characters 10-12:
     11 | const x = 42;
     12 | const x = 33;
     13 |

    Cannot redeclare block-scoped variable. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "--transpiled"; contract_warn_shadowing ];
  [%expect {| { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]
