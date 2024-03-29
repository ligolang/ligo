open Cli_expect

let test basename = "./" ^ basename
let pwd = Caml.Sys.getcwd ()
let () = Caml.Sys.chdir "../../test/contracts/polymorphism/"

let%expect_test _ =
  run_ligo_good
    [ "compile"; "storage"; test "monomorphisation_raw.mligo"; "foo"; "-m"; "C" ];
  [%expect {| 0x0502000000110320074303680100000004686168610327 |}];
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "foo2"
    ; "--init-file"
    ; test "monomorphisation_raw.mligo"
    ];
  [%expect {| An error occurred while evaluating an expression: "hehe" |}];
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "foo3"
    ; "--init-file"
    ; test "monomorphisation_raw.mligo"
    ];
  [%expect {| 0x09 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "List.mymap (fun (x : int) -> abs x) [1;2]"
    ; "--init-file"
    ; test "module_type.mligo"
    ];
  [%expect {| { 1 ; 2 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "monomorphisation_let.mligo" ];
  [%expect {| { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "zip [1;2;3] [4n;5n;6n]"
    ; "--init-file"
    ; test "comb.mligo"
    ];
  [%expect {| { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "zip (zip [1;2;3] [4n;5n;6n]) [\"a\";\"b\";\"c\"]"
    ; "--init-file"
    ; test "comb.mligo"
    ];
  [%expect {| { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "let (x, y) = diag 4 in x + y"
    ; "--init-file"
    ; test "comb.mligo"
    ];
  [%expect {| 8 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "v"; "--init-file"; test "comb.mligo" ];
  [%expect {| { Pair "a" "a" ; Pair "b" "b" } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "w"; "--init-file"; test "comb.mligo" ];
  [%expect {| { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "(zip(list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as nat)]))"
    ; "--init-file"
    ; test "comb.jsligo"
    ];
  [%expect {| { Pair 1 4 ; Pair 2 5 ; Pair 3 6 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "identity.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { DROP ; PUSH int 1 ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "(zip((zip(list([1,2,3])))(list([(4 as nat),(5 as nat),(6 as \
       nat)]))))(list([\"a\",\"b\",\"c\"]))"
    ; "--init-file"
    ; test "comb.jsligo"
    ];
  [%expect {| { Pair (Pair 1 4) "a" ; Pair (Pair 2 5) "b" ; Pair (Pair 3 6) "c" } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "ctrct.mligo" ];
  [%expect {| { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "test.mligo" ];
  [%expect
    {|
    File "./test.mligo", line 9, characters 13-27:
      8 | let test =
      9 |   let orig = Test.originate (contract_of C) 0 0tez in
                       ^^^^^^^^^^^^^^
     10 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "./test.mligo", line 10, characters 10-27:
      9 |   let orig = Test.originate (contract_of C) 0 0tez in
     10 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
                    ^^^^^^^^^^^^^^^^^
     11 |   assert (Test.get_storage orig.addr = 42)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "./test.mligo", line 11, characters 2-8:
     10 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
     11 |   assert (Test.get_storage orig.addr = 42)
            ^^^^^^
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "./test.mligo", line 11, characters 10-26:
     10 |   let _ = Test.transfer_exn orig.addr (Main 42) 0tez in
     11 |   assert (Test.get_storage orig.addr = 42)
                    ^^^^^^^^^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "bar"; "--init-file"; test "modules.mligo" ];
  [%expect {|
    (Pair (Some 1) (Some "hello")) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "foo"; "--init-file"; test "use_nelist.mligo" ];
  [%expect {|
    { 2 ; 4 ; 6 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "bar"; "--init-file"; test "use_nelist.mligo" ];
  [%expect {|
    12 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "v"
    ; "--init-file"
    ; test "cases_annotation1.mligo"
    ];
  [%expect {|
    "hello" |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "v"
    ; "--init-file"
    ; test "cases_annotation2.mligo"
    ];
  [%expect {|
    "hello" |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "solve 5"
    ; "--init-file"
    ; test "use_monad.mligo"
    ];
  [%expect {| { Pair 3 4 5 ; Pair 4 3 5 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "solve 5"
    ; "--init-file"
    ; test "use_monad_set.mligo"
    ];
  [%expect {| { Pair 3 4 5 ; Pair 4 3 5 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "auto"
    ; "solve(10)"
    ; "--init-file"
    ; test "use_monad.jsligo"
    ];
  [%expect {|
    { Pair 3 4 5 ; Pair 6 8 10 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "map (fun (f : (string -> int -> int)) -> f \"hello\" 4) (uhms : (string -> int -> \
       int) list)"
    ; "--init-file"
    ; test "map.mligo"
    ];
  [%expect {|
    { 4 ; 4 } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "bar 5"
    ; "--init-file"
    ; test "use_error.mligo"
    ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "List.map (fun (x : int) -> x + 1) [1;2]"
    ; "--init-file"
    ; test "map.mligo"
    ];
  [%expect {|
    CONS(2 , CONS(3 , LIST_EMPTY())) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "module_k.mligo" ];
  [%expect
    {|
    File "./module_k.mligo", line 9, characters 2-8:
      8 |   let v = H.k 1 2 in
      9 |   assert (v = 1)
            ^^^^^^
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_helpers exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "use_rec.jsligo" ];
  [%expect
    {|
    File "./use_rec.jsligo", line 4, characters 14-28:
      3 | const _test = (_t : unit) : int => {
      4 |    let orig = Test.originate(contract_of (Contract), 0, 0 as tez);
                        ^^^^^^^^^^^^^^
      5 |    return orig.size;
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value 51. |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "foo"; "--init-file"; test "lambda.mligo" ];
  [%expect {|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; test "record_sapling.mligo" ];
  [%expect
    {|
    { parameter string ;
      storage (pair (pair %state int (sapling_state 8)) (string %name)) ;
      code { UNPAIR ; SWAP ; CAR ; PAIR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "try_transfer (\"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\" : address) 0 (Build_state \
       (Map.empty :(address, tokenValue) map))"
    ; "--init-file"
    ; test "map_or_big_map.mligo"
    ];
  [%expect {|
    (Some { Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" 0 }) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "x"; "--init-file"; test "same_vars.mligo" ];
  [%expect {| 4 |}]

let () =
  Caml.Sys.chdir pwd;
  Caml.Sys.chdir "../../test/contracts/negative/polymorphism/"


let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; test "annotate2.mligo" ];
  [%expect
    {|
    File "./annotate2.mligo", line 1, characters 11-13:
      1 | let f (x : _a) = x
                     ^^

    Type "_a" not found. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "f"
    ; "--init-file"
    ; test "annotate_arrow.mligo"
    ];
  [%expect
    {|
    File "./annotate_arrow.mligo", line 1, characters 0-36:
      1 | let f (_:unit) (_:nat option) = None
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Cannot monomorphise the expression.
    The inferred type was "[_]unit -> ∀ a . [_]option (nat) -> option (a)".
    Hint: Try adding additional annotations. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; test "constants.mligo" ];
  [%expect
    {|
    File "./constants.mligo", line 5, characters 14-45:
      4 |
      5 | let m = merge (Map.empty : (int, string) foo)
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This expression has type "map (int , string)", but an expression was expected of type
    "map (^a ,
    ^a)".
    Type "string" is not compatible with type "int". |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "f"
    ; "--init-file"
    ; test "cases_annotation.mligo"
    ];
  [%expect
    {|
    { LAMBDA
        (pair bool string)
        string
        { UNPAIR ;
          SWAP ;
          PUSH int 2 ;
          PUSH int 40 ;
          ADD ;
          SWAP ;
          DIG 2 ;
          IF { LAMBDA
                 string
                 (lambda int string)
                 { LAMBDA (pair string int) string { CAR } ; DUP 2 ; APPLY ; SWAP ; DROP } }
             { LAMBDA
                 string
                 (lambda int string)
                 { LAMBDA (pair string int) string { CAR } ; DUP 2 ; APPLY ; SWAP ; DROP } } ;
          SWAP ;
          EXEC ;
          SWAP ;
          EXEC } ;
      DUP 2 ;
      APPLY ;
      SWAP ;
      DROP } |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "bar 0"
    ; "--init-file"
    ; test "use_error.mligo"
    ];
  [%expect {|
    An error occurred while evaluating an expression: "Division by zero" |}]

(* Unresolved polymorphism *)

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "unresolved/contract.mligo" ];
  [%expect
    {xxx|
    File "./unresolved/contract.mligo", line 6, characters 29-31:
      5 |     let b                = List.length ys in
      6 |     [], (a + b + List.length [])
                                       ^^

    Underspecified type "^a".
    Please add additional annotations.
    Hint: "^a" represent placeholder type(s). |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "unresolved/contract2.mligo" ];
  [%expect
    {xxx|
    File "./unresolved/contract2.mligo", line 4, characters 13-15:
      3 | let main (_ : int list) (_ : nat) : (operation list * nat) =
      4 |     [], (one [])
                       ^^

    Underspecified type "^a".
    Please add additional annotations.
    Hint: "^a" represent placeholder type(s). |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "storage"; test "unresolved/storage.mligo"; "s" ];
  [%expect
    {xxx|
    File "./unresolved/storage.mligo", line 1, characters 20-22:
      1 | let s = List.length []
                              ^^
      2 |

    Underspecified type "^a".
    Please add additional annotations.
    Hint: "^a" represent placeholder type(s). |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "parameter"; test "unresolved/parameter.mligo"; "p" ];
  [%expect
    {xxx|
    File "./unresolved/parameter.mligo", line 1, characters 8-10:
      1 | let p = []
                  ^^
      2 |

    Underspecified type "^a".
    Please add additional annotations.
    Hint: "^a" represent placeholder type(s). |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "cameligo"; "[]" ];
  [%expect
    {|
    Underspecified type "^a".
    Please add additional annotations.
    Hint: "^a" represent placeholder type(s). |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "monomorphisation_fail.mligo" ];
  [%expect
    {|
    File "./monomorphisation_fail.mligo", line 1, characters 0-28:
      1 | let f (_ : unit) s = ([], s)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      2 |

    Cannot monomorphise the expression.
    The inferred type was "[_]unit -> ∀ b . ∀ c . [s]b -> ( list (c) * b )".
    Hint: Try adding additional annotations. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; test "monomorphisation_fail2.mligo" ];
  [%expect
    {|
    File "./monomorphisation_fail2.mligo", line 2, character 2 to line 7, character 3:
      1 | let nested (type a) =
      2 |   let x (type b) =
            ^^^^^^^^^^^^^^^^
      3 |     let y (type c) =
          ^^^^^^^^^^^^^^^^^^^^
      4 |       let z = (failwith ("nested") : a -> b -> c) in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |       z in
          ^^^^^^^^^^
      6 |     y in
          ^^^^^^^^
      7 |   x
          ^^^
      8 |

    Cannot monomorphise the expression. |}]

let () = Caml.Sys.chdir pwd
