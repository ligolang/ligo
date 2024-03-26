open Cli_expect

let contract = test
let contract_resource name = test ("res/" ^ name)
let bad_contract = bad_test

(* avoid pretty printing *)
let () = Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "timelock_flip.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (chest %initialize) (or (bytes %guess) (chest_key %finish))) ;
      storage (pair (nat %level) (chest %chest) (bytes %guess) (bytes %result)) ;
      code { UNPAIR ;
             IF_LEFT
               { SWAP ; DROP ; PUSH bytes 0xa0 ; DUP ; DIG 2 ; LEVEL ; PAIR 4 }
               { IF_LEFT
                   { PUSH nat 10 ;
                     DUP 3 ;
                     CAR ;
                     ADD ;
                     LEVEL ;
                     COMPARE ;
                     LE ;
                     IF { PUSH bytes 0xb0 ; SWAP ; DUP 3 ; GET 3 ; DIG 3 ; CAR ; PAIR 4 }
                        { DROP } }
                   { PUSH nat 1024 ;
                     DUP 3 ;
                     GET 3 ;
                     DIG 2 ;
                     OPEN_CHEST ;
                     IF_NONE
                       { PUSH bytes 0x10 }
                       { DUP 2 ;
                         GET 5 ;
                         SWAP ;
                         COMPARE ;
                         EQ ;
                         IF { PUSH bytes 0x00 } { PUSH bytes 0x01 } } ;
                     DUP 2 ;
                     GET 5 ;
                     DUP 3 ;
                     GET 3 ;
                     DIG 3 ;
                     CAR ;
                     PAIR 4 } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "deprecated.mligo" ];
  [%expect
    {|
    File "../../test/contracts/deprecated.mligo", line 5, characters 74-75:
      4 | module C = struct
      5 |   [@entry] let foo (() : unit) (m : int) : operation list * int = [], m + f ()
                                                                                    ^
      6 | end
    :
    Warning: deprecated value.
    Replace me by...
    g!
    mail: foo@bar.com

    File "../../test/contracts/deprecated.mligo", line 8, characters 11-19:
      7 |
      8 | let test = Test.log (f ())
                     ^^^^^^^^
      9 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/deprecated.mligo", line 8, characters 21-22:
      7 |
      8 | let test = Test.log (f ())
                               ^
      9 |
    :
    Warning: deprecated value.
    Replace me by...
    g!
    mail: foo@bar.com

    File "../../test/contracts/deprecated.mligo", line 13, characters 12-20:
     12 |
     13 | let test2 = Test.log (h () + i ())
                      ^^^^^^^^
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/deprecated.mligo", line 13, characters 22-23:
     12 |
     13 | let test2 = Test.log (h () + i ())
                                ^
    :
    Warning: deprecated value.
    this is h, but only h or i will trigger

    1
    6
    Everything at the top-level was executed.
    - test exited with value ().
    - test2 exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "deprecated.mligo"; "-m"; "C" ];
  [%expect
    {|
    File "../../test/contracts/deprecated.mligo", line 5, characters 74-75:
      4 | module C = struct
      5 |   [@entry] let foo (() : unit) (m : int) : operation list * int = [], m + f ()
                                                                                    ^
      6 | end
    :
    Warning: deprecated value.
    Replace me by...
    g!
    mail: foo@bar.com

    { parameter unit ;
      storage int ;
      code { CDR ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "interfaces.extra.jsligo"; "-m"; "Impl" ];
  [%expect
    {|
    { parameter (or (int %extra) (int %add)) ;
      storage int ;
      code { UNPAIR ; IF_LEFT { SUB } { ADD } ; NIL operation ; PAIR } } |}];
  run_ligo_good
    [ "compile"; "contract"; contract "interfaces.include.jsligo"; "-m"; "ImplAll" ];
  [%expect
    {|
    { parameter
        (or (unit %other4)
            (or (int %juju)
                (or (unit %other3) (or (unit %other2) (or (unit %other1) (unit %transfer)))))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT
               { DROP }
               { IF_LEFT
                   { DROP }
                   { IF_LEFT
                       { DROP }
                       { IF_LEFT { DROP } { IF_LEFT { DROP } { DROP } } } } } ;
             NIL operation ;
             PAIR } ;
      view "v1" unit int { CDR } ;
      view "v2" unit int { CDR } } |}];
  run_ligo_good
    [ "compile"; "contract"; contract "interfaces.include.jsligo"; "-m"; "ImplAllNoJuju" ];
  [%expect
    {|
    { parameter
        (or (unit %other4)
            (or (unit %other3) (or (unit %other2) (or (unit %other1) (unit %transfer))))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT
               { DROP }
               { IF_LEFT
                   { DROP }
                   { IF_LEFT { DROP } { IF_LEFT { DROP } { DROP } } } } ;
             NIL operation ;
             PAIR } ;
      view "v1" unit int { CDR } ;
      view "v2" unit int { CDR } } |}];
  run_ligo_good
    [ "compile"; "contract"; contract "interfaces.include.mligo"; "-m"; "FA0EXTImpl" ];
  [%expect
    {|
    { parameter (or (unit %transfer2) (unit %transfer)) ;
      storage unit ;
      code { LAMBDA
               (pair unit unit)
               (pair (list operation) unit)
               { DROP ; UNIT ; NIL operation ; PAIR } ;
             SWAP ;
             UNPAIR ;
             IF_LEFT { PAIR ; EXEC } { PAIR ; EXEC } } } |}];
  run_ligo_bad
    [ "compile"; "contract"; bad_contract "interfaces.optional.jsligo"; "-m"; "FAAll" ];
  [%expect
    {|
    File "../../test/contracts/negative/interfaces.optional.jsligo", line 17, character 0 to line 23, character 1:
     16 |
     17 | namespace Impl implements FA0Ext, FA1 {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     18 |   type t = int;
          ^^^^^^^^^^^^^^^
     19 |

     20 |   @entry const transfer = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     21 |   @entry const other1 = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     22 |   @entry const other2 = (_u : unit, s : t) : [list<operation>, t] => [list([]), s];
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     23 | };
          ^
     24 |

    Type "t" declared in signature but not found. |}];
  run_ligo_good [ "run"; "test"; contract "interfaces.include.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/interfaces.include.jsligo", line 68, characters 13-27:
     67 | const test = do {
     68 |   let orig = Test.originate(contract_of(ImplAll), ImplAll.foo(42), 0tez);
                       ^^^^^^^^^^^^^^
     69 |   let p : parameter_of ImplAll = Other4();
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/interfaces.include.jsligo", line 70, characters 2-19:
     69 |   let p : parameter_of ImplAll = Other4();
     70 |   Test.transfer_exn(orig.addr, p, 1mutez);
            ^^^^^^^^^^^^^^^^^
     71 | }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value 1297n. |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "jsligo"; "_"; "--init-file"; contract "wildcards.jsligo" ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "h2"
    ; "--init-file"
    ; contract "wildcard_in_type.jsligo"
    ];
  [%expect
    {|
    { LAMBDA (pair int int) int { UNPAIR ; ADD } ;
      DUP 2 ;
      APPLY ;
      SWAP ;
      DROP } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "foo"
    ; "--init-file"
    ; contract "function_ascr.jsligo"
    ];
  [%expect {| {} |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "y"
    ; "--init-file"
    ; contract "import_export/h.jsligo"
    ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "va"
    ; "--init-file"
    ; contract "off_view_expression.mligo"
    ; "--function-body"
    ];
  [%expect {|
    { UNPAIR ; SIZE ; ADD } |}];
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "vb"
    ; "--init-file"
    ; contract "off_view_expression.mligo"
    ; "--function-body"
    ];
  [%expect {|
    { UNPAIR ; SIZE ; ADD ; INT } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "f(list([1,2,3,4,5]))"
    ; "--init-file"
    ; contract "implicit_cast.jsligo"
    ];
  [%expect {|
    3 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "b2"
    ; "--init-file"
    ; contract "implicit_cast.mligo"
    ];
  [%expect {|
    True |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "ball"
    ; "--init-file"
    ; contract "implicit_cast.mligo"
    ];
  [%expect {|
    True |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "add2(x, x)"
    ; "--init-file"
    ; contract "modules.jsligo"
    ];
  [%expect {|
    84 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "FA1.2.interface.mligo"; "-m"; "FA12_ENTRIES" ];
  [%expect
    {|
    { parameter
        (or (pair %getTotalSupply (unit %request) (contract %callback nat))
            (or (pair %getBalance (address %owner) (contract %callback nat))
                (or (pair %getAllowance
                       (pair %request (address %owner) (address %spender))
                       (contract %callback nat))
                    (or (pair %approve (address %spender) (nat %value))
                        (pair %transfer (address %from) (address %to) (nat %value)))))) ;
      storage
        (pair (big_map %tokens address nat)
              (big_map %allowances (pair (address %owner) (address %spender)) nat)
              (nat %total_supply)) ;
      code { UNPAIR ;
             IF_LEFT
               { DUP 2 ;
                 NIL operation ;
                 DIG 2 ;
                 CDR ;
                 PUSH mutez 0 ;
                 DIG 4 ;
                 GET 4 ;
                 TRANSFER_TOKENS ;
                 CONS }
               { IF_LEFT
                   { DUP 2 ;
                     NIL operation ;
                     DUP 3 ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 5 ;
                     CAR ;
                     DIG 5 ;
                     CAR ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     TRANSFER_TOKENS ;
                     CONS }
                   { IF_LEFT
                       { DUP 2 ;
                         NIL operation ;
                         DUP 3 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 5 ;
                         GET 3 ;
                         DIG 5 ;
                         CAR ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         TRANSFER_TOKENS ;
                         CONS }
                       { IF_LEFT
                           { DUP 2 ;
                             GET 3 ;
                             DUP 2 ;
                             CAR ;
                             SENDER ;
                             PAIR ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             CDR ;
                             COMPARE ;
                             GT ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             DUP 4 ;
                             GET ;
                             IF_NONE { PUSH nat 0 } {} ;
                             COMPARE ;
                             GT ;
                             AND ;
                             IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                             DIG 2 ;
                             CDR ;
                             DIG 3 ;
                             DIG 3 ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             COMPARE ;
                             EQ ;
                             IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
                             DIG 3 ;
                             UPDATE ;
                             UPDATE 3 }
                           { DUP 2 ;
                             GET 3 ;
                             DUP 3 ;
                             CAR ;
                             DUP 3 ;
                             CAR ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF { SWAP }
                                { SENDER ;
                                  DUP 4 ;
                                  CAR ;
                                  PAIR ;
                                  DUP 4 ;
                                  GET 4 ;
                                  DUP 4 ;
                                  DUP 3 ;
                                  GET ;
                                  IF_NONE { PUSH nat 0 } {} ;
                                  SUB ;
                                  ISNAT ;
                                  IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                                  DIG 3 ;
                                  PUSH nat 0 ;
                                  DUP 3 ;
                                  COMPARE ;
                                  EQ ;
                                  IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                                  DIG 2 ;
                                  UPDATE } ;
                             DUP 3 ;
                             GET 4 ;
                             DUP 3 ;
                             DUP 5 ;
                             CAR ;
                             GET ;
                             IF_NONE { PUSH nat 0 } {} ;
                             SUB ;
                             ISNAT ;
                             IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                             DIG 2 ;
                             PUSH nat 0 ;
                             DUP 3 ;
                             COMPARE ;
                             EQ ;
                             IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                             DUP 4 ;
                             CAR ;
                             UPDATE ;
                             DUP 3 ;
                             GET 4 ;
                             DUP 2 ;
                             DUP 5 ;
                             GET 3 ;
                             GET ;
                             IF_NONE { PUSH nat 0 } {} ;
                             ADD ;
                             DIG 4 ;
                             DIG 2 ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             COMPARE ;
                             EQ ;
                             IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
                             DIG 4 ;
                             GET 3 ;
                             UPDATE ;
                             UPDATE 1 ;
                             SWAP ;
                             UPDATE 3 } ;
                         NIL operation } } } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "t"
    ; "--init-file"
    ; contract "jsligo_uppercase_generic.jsligo"
    ];
  [%expect {|
    { 1 ; 2 ; 3 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "jsligo_list_concat.jsligo" ];
  [%expect
    {|
    { parameter (list int) ;
      storage (list int) ;
      code { UNPAIR ;
             NIL int ;
             SWAP ;
             ITER { CONS } ;
             ITER { CONS } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "export_attribute.jsligo"; "-m"; "Foo" ];
  [%expect
    {|
    { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "of_file.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             UNIT ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter unit ;
                 storage unit ;
                 code { DROP ; UNIT ; NIL operation ; PAIR } } ;
             SWAP ;
             DROP ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "create_contract_of_file.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { CAR ;
             PUSH mutez 1000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter unit ;
                 storage unit ;
                 code { DROP ; UNIT ; NIL operation ; PAIR } } ;
             SWAP ;
             DROP ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "create_contract_of_file.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { CAR ;
             PUSH mutez 1000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter unit ;
                 storage unit ;
                 code { DROP ; UNIT ; NIL operation ; PAIR } } ;
             SWAP ;
             DROP ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "of_file.mligo" ];
  [%expect
    {xxx|
    File "../../test/contracts/negative/of_file.mligo", line 4, characters 5-30:
      3 |   ({| { PUSH unit Unit ; PUSH mutez 300000000 ; NONE key_hash ; CREATE_CONTRACT (codestr $0) ; PAIR } |}
      4 |      [%of_file "./removed.tz"]
               ^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |    : operation * address)]

    Found a system error: ./removed.tz: No such file or directory. |xxx}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_of_file.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/create_contract_of_file.jsligo", line 3, characters 21-59:
      2 | const main = (u : unit, _ : unit) : [list<operation>, unit] => {
      3 |   let [op, _addr] = (create_contract_of_file `./removed.tz`)(None(), 1tez, u);
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   return [list([op]), []]

    Found a system error: ./removed.tz: No such file or directory. |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "s"; "--init-file"; contract "of_file.mligo" ];
  [%expect
    {xxx|
    "let s = [%of_file \"./of_file.mligo\"]\n\nlet m () =\n  [%michelson\n  ({| { PUSH unit Unit ; PUSH mutez 300000000 ; NONE key_hash ; CREATE_CONTRACT (codestr $0) ; PAIR } |}\n     [%of_file \"./interpreter_tests/contract_under_test/compiled.tz\"]\n   : operation * address)]\n\n[@entry]\nlet main (_ : unit) (_ : unit) : operation list * unit =\n  let op, _ = m () in\n  [op], ()\n" |xxx}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "[j, k]"
    ; "--init-file"
    ; contract "option.jsligo"
    ];
  [%expect {|
    (Pair 42 1) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "const.mligo" ];
  [%expect
    {|
    File "../../test/contracts/const.mligo", line 1, characters 31-32:
      1 | let const = fun (type a b) (a, b : a * b) : a -> a
                                         ^
      2 |
    :
    Warning: unused variable "b".
    Hint: replace it by "_b" to prevent this warning.

    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "const1.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "const2.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "vote.mligo" ];
  [%expect
    {|
    { parameter
        (or (or %vote (unit %yea) (unit %nay))
            (pair %reset (string %title) (timestamp %start_time) (timestamp %finish_time))) ;
      storage
        (pair (string %title)
              (nat %yea)
              (nat %nay)
              (set %voters address)
              (timestamp %start_time)
              (timestamp %finish_time)) ;
      code { UNPAIR ;
             IF_LEFT
               { SENDER ;
                 SWAP ;
                 IF_LEFT
                   { DROP ; DUP 2 ; PUSH nat 1 ; DIG 3 ; GET 3 ; ADD ; UPDATE 3 }
                   { DROP ; DUP 2 ; PUSH nat 1 ; DIG 3 ; GET 5 ; ADD ; UPDATE 5 } ;
                 DUP ;
                 GET 7 ;
                 PUSH bool True ;
                 DIG 3 ;
                 UPDATE ;
                 UPDATE 7 }
               { SWAP ;
                 DROP ;
                 DUP ;
                 GET 4 ;
                 DUP 2 ;
                 GET 3 ;
                 EMPTY_SET address ;
                 PUSH nat 0 ;
                 PUSH nat 0 ;
                 DIG 5 ;
                 CAR ;
                 PAIR 6 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "ticket_wallet.mligo" ];
  [%expect
    {|
    { parameter
        (or (ticket %receive unit)
            (pair %send (contract %destination (ticket unit)) (nat %amount) (address %ticketer))) ;
      storage (pair (address %manager) (big_map %tickets address (ticket unit))) ;
      code { UNPAIR ;
             PUSH mutez 0 ;
             AMOUNT ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             SWAP ;
             UNPAIR ;
             DIG 2 ;
             IF_LEFT
               { READ_TICKET ;
                 CAR ;
                 DIG 3 ;
                 NONE (ticket unit) ;
                 DUP 3 ;
                 GET_AND_UPDATE ;
                 IF_NONE
                   { DIG 2 }
                   { DIG 3 ;
                     PAIR ;
                     JOIN_TICKETS ;
                     IF_NONE { PUSH string "impossible?" ; FAILWITH } {} } ;
                 SOME ;
                 DIG 2 ;
                 UPDATE ;
                 SWAP ;
                 PAIR ;
                 NIL operation ;
                 PAIR }
               { DUP 2 ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                 DIG 2 ;
                 NONE (ticket unit) ;
                 DUP 3 ;
                 GET 4 ;
                 GET_AND_UPDATE ;
                 IF_NONE
                   { DROP 3 ; PUSH string "no tickets" ; FAILWITH }
                   { READ_TICKET ;
                     CDR ;
                     CDR ;
                     DUP 4 ;
                     GET 3 ;
                     DUP ;
                     DIG 2 ;
                     SUB ;
                     ISNAT ;
                     IF_NONE { PUSH string "not enough tickets" ; FAILWITH } {} ;
                     SWAP ;
                     PAIR ;
                     SWAP ;
                     SPLIT_TICKET ;
                     IF_NONE
                       { DROP 3 ; PUSH string "impossible?" ; FAILWITH }
                       { UNPAIR ;
                         DUG 2 ;
                         SOME ;
                         DUP 4 ;
                         GET 4 ;
                         UPDATE ;
                         DIG 2 ;
                         CAR ;
                         PUSH mutez 0 ;
                         DIG 3 ;
                         TRANSFER_TOKENS ;
                         SWAP ;
                         DIG 2 ;
                         PAIR ;
                         NIL operation ;
                         DIG 2 ;
                         CONS ;
                         PAIR } } } } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "ticket_builder.mligo" ];
  [%expect
    {|
    File "../../test/contracts/ticket_builder.mligo", line 29, characters 30-36:
     28 |         begin
     29 |           let ((ticketer, _), ticket) =
                                        ^^^^^^
     30 |             (Tezos.read_ticket ticket : (address * (unit * nat)) * unit ticket) in
    :
    Warning: unused variable "ticket".
    Hint: replace it by "_ticket" to prevent this warning.

    { parameter
        (or (ticket %burn unit)
            (pair %mint (contract %destination (ticket unit)) (nat %amount))) ;
      storage address ;
      code { UNPAIR ;
             PUSH mutez 0 ;
             AMOUNT ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             IF_LEFT
               { READ_TICKET ;
                 SWAP ;
                 DROP ;
                 CAR ;
                 SELF_ADDRESS ;
                 SWAP ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                 NIL operation }
               { DUP 2 ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                 DUP ;
                 CDR ;
                 UNIT ;
                 TICKET ;
                 PUSH string "option is None" ;
                 SWAP ;
                 IF_NONE { FAILWITH } { SWAP ; DROP } ;
                 SWAP ;
                 CAR ;
                 PUSH mutez 0 ;
                 DIG 2 ;
                 TRANSFER_TOKENS ;
                 SWAP ;
                 NIL operation ;
                 DIG 2 ;
                 CONS } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "implicit.mligo" ];
  [%expect
    {|
    File "../../test/contracts/implicit.mligo", line 4, characters 8-9:
      3 |   let main (p : key_hash) (s : unit) =
      4 |     let c : unit contract = Tezos.implicit_account p in
                  ^
      5 |     ([] : operation list), unit
    :
    Warning: unused variable "c".
    Hint: replace it by "_c" to prevent this warning.

    File "../../test/contracts/implicit.mligo", line 3, characters 27-28:
      2 |   [@entry]
      3 |   let main (p : key_hash) (s : unit) =
                                     ^
      4 |     let c : unit contract = Tezos.implicit_account p in
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter key_hash ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "capture_big_map.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/capture_big_map.mligo", line 14, characters 38-51:
     13 |
     14 |   let supply (ledger : l) (_ : nat) = ledger.supply
                                                ^^^^^^^^^^^^^
     15 |

    Invalid capturing, term captures the type big_map (address ,
    nat).
    Hint: Uncurry or use tuples instead of high-order functions. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "expression"; "cameligo"; "fun (x : operation) -> fun (y : int) -> x" ];
  [%expect
    {|
    Invalid capturing, term captures the type operation.
    Hint: Uncurry or use tuples instead of high-order functions. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "fun (x : 4 sapling_transaction) -> fun (y : int) -> x"
    ];
  [%expect
    {|
    Invalid capturing, term captures the type sapling_transaction (4).
    Hint: Uncurry or use tuples instead of high-order functions. |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "fun (x : operation -> int contract) -> fun (y : int) -> x"
    ];
  [%expect
    {|
    { LAMBDA
        (pair (lambda operation (contract int)) int)
        (lambda operation (contract int))
        { CAR } ;
      DUP 2 ;
      APPLY ;
      SWAP ;
      DROP } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "amount_lambda.mligo" ];
  (* AMOUNT should occur inside the second lambda, but not the first lambda *)
  [%expect
    {|
    File "../../test/contracts/amount_lambda.mligo", line 5, characters 7-8:
      4 |   let amt : tez = Tezos.get_amount () in
      5 |   fun (x : unit) -> amt
                 ^
      6 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 3, characters 8-9:
      2 |
      3 | let f1 (x : unit) : unit -> tez =
                  ^
      4 |   let amt : tez = Tezos.get_amount () in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 9, characters 39-40:
      8 |
      9 | let f2 (x : unit) : unit -> tez = fun (x : unit) -> Tezos.get_amount ()
                                                 ^
     10 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 9, characters 8-9:
      8 |
      9 | let f2 (x : unit) : unit -> tez = fun (x : unit) -> Tezos.get_amount ()
                  ^
     10 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 12, characters 21-22:
     11 | [@entry]
     12 | let main (b : bool) (s : (unit -> tez)) : operation list * (unit -> tez) =
                               ^
     13 |   (([] : operation list), (if b then f1 () else f2 ()))
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter bool ;
      storage (lambda unit mutez) ;
      code { CAR ;
             IF { AMOUNT ;
                  LAMBDA (pair mutez unit) mutez { CAR } ;
                  DUP 2 ;
                  APPLY ;
                  SWAP ;
                  DROP }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "subtle_nontail_fail.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH bool True ;
             IF { PUSH string "This contract always fails" ; FAILWITH }
                { PUSH string "This contract still always fails" ; FAILWITH } } } |}]

let%expect_test _ =
  (* TODO should not be bad? *)
  run_ligo_good [ "run"; "dry-run"; contract "subtle_nontail_fail.mligo"; "()"; "()" ];
  [%expect {| failed with: "This contract always fails" |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "self_in_lambda.mligo" ];
  [%expect
    {|
      "Tezos.self" must be used directly and cannot be used via another function. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "self_in_lambdarec.mligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/self_in_lambdarec.mligo", line 7, characters 7-19:
        6 |     Tezos.address
        7 |       (Option.unopt (Tezos.get_contract_opt addr : int contract option))
                   ^^^^^^^^^^^^
        8 |
      :
      Warning: deprecated value.
      Use `Option.value_with_error` instead.

      "Tezos.self" must be used directly and cannot be used via another function. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "not_comparable.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/not_comparable.mligo", line 2, characters 16-19:
      1 | [@entry]
      2 | let main (_u : (int set) set) (s : unit) : operation list * unit =
                          ^^^
      3 |   ([] : operation list), s

    This type is used inside:
    File "../../test/contracts/negative/not_comparable.mligo", line 2, characters 15-24:
      1 | [@entry]
      2 | let main (_u : (int set) set) (s : unit) : operation list * unit =
                         ^^^^^^^^^
      3 |   ([] : operation list), s

    The set constructor needs a comparable type argument, but it was given a non-comparable one. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; bad_contract "not_comparable.mligo"; "-m"; "Main2" ];
  [%expect
    {|
    File "../../test/contracts/negative/not_comparable.mligo", line 7, characters 18-21:
      6 |   [@entry]
      7 |   let main (_u : (int set) ticket) (s : unit) : operation list * unit =
                            ^^^
      8 |     ([] : operation list), s

    This type is used inside:
    File "../../test/contracts/negative/not_comparable.mligo", line 7, characters 17-26:
      6 |   [@entry]
      7 |   let main (_u : (int set) ticket) (s : unit) : operation list * unit =
                           ^^^^^^^^^
      8 |     ([] : operation list), s

    The ticket constructor needs a comparable type argument, but it was given a non-comparable one. |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "dry-run"; contract "super-counter.mligo"; "test_param"; "test_storage" ];
  [%expect {| ( LIST_EMPTY() , 3 ) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "redundant_constructors.mligo"
    ];
  [%expect {| (Pair (Left 42) (Left 42)) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "redundant_constructors_but_annotated.mligo"
    ];
  [%expect {| (Pair {} (Left 1)) |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_toplevel.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/create_contract_toplevel.mligo", line 5, character 35 to line 9, character 8:
  4 | let main (_ : string) (store : string) : return =
  5 |   let toto : operation * address = Tezos.create_contract
                                         ^^^^^^^^^^^^^^^^^^^^^
  6 |     (fun (_p : nat) (_s : string) -> (([] : operation list), store))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  7 |     (None: key_hash option)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  8 |     300tz
      ^^^^^^^^^^
  9 |     "un"
      ^^^^^^^^
 10 |   in

Not all free variables could be inlined in Tezos.create_contract usage: gen#385. |}];
  run_ligo_good [ "compile"; "contract"; contract "create_contract_var.mligo" ];
  [%expect
    {|
    File "../../test/contracts/create_contract_var.mligo", line 9, characters 22-23:
      8 |     Tezos.create_contract
      9 |       (fun (p : nat) (s : int) -> (([] : operation list), a))
                                ^
     10 |       (None : key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 9, characters 12-13:
      8 |     Tezos.create_contract
      9 |       (fun (p : nat) (s : int) -> (([] : operation list), a))
                      ^
     10 |       (None : key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 6, characters 10-16:
      5 | [@entry]
      6 | let main (action : string) (store : string) : return =
                    ^^^^^^
      7 |   let toto : operation * address =
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter string ;
      storage string ;
      code { CDR ;
             PUSH int 1 ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage int ;
                 code { DROP ; PUSH int 2 ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_modfv.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/create_contract_modfv.mligo", line 11, characters 22-23:
     10 |     Tezos.create_contract
     11 |       (fun (p : nat) (s : string) -> (([] : operation list), Foo.store))
                                ^
     12 |       (None : key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 11, characters 12-13:
     10 |     Tezos.create_contract
     11 |       (fun (p : nat) (s : string) -> (([] : operation list), Foo.store))
                      ^
     12 |       (None : key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 4, characters 10-16:
      3 | [@entry]
      4 | let main (action : string) (store : string) : return =
                    ^^^^^^
      5 |   module Foo = struct
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 10, character 4 to line 14, character 10:
      9 |   let toto : operation * address =
     10 |     Tezos.create_contract
              ^^^^^^^^^^^^^^^^^^^^^
     11 |       (fun (p : nat) (s : string) -> (([] : operation list), Foo.store))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     12 |       (None : key_hash option)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |       300000000mutez
          ^^^^^^^^^^^^^^^^^^^^
     14 |       "un" in
          ^^^^^^^^^^
     15 |   ([toto.0], store)

    Not all free variables could be inlined in Tezos.create_contract usage: gen#386. |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_no_inline.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 30-31:
      4 |
      5 | let dummy_contract (p : nat) (s : int) : return = (([] : operation list), foo)
                                        ^
      6 |
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 20-21:
      4 |
      5 | let dummy_contract (p : nat) (s : int) : return = (([] : operation list), foo)
                              ^
      6 |
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 11-15:
      8 | let main (action : int) (store : int) : return =
      9 |   let (op, addr) =
                     ^^^^
     10 |     Tezos.create_contract
    :
    Warning: unused variable "addr".
    Hint: replace it by "_addr" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 25-30:
      7 | [@entry]
      8 | let main (action : int) (store : int) : return =
                                   ^^^^^
      9 |   let (op, addr) =
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 10-16:
      7 | [@entry]
      8 | let main (action : int) (store : int) : return =
                    ^^^^^^
      9 |   let (op, addr) =
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 10, character 4 to line 14, character 7:
      9 |   let (op, addr) =
     10 |     Tezos.create_contract
              ^^^^^^^^^^^^^^^^^^^^^
     11 |       dummy_contract
          ^^^^^^^^^^^^^^^^^^^^
     12 |       ((None : key_hash option))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |       300000000mutez
          ^^^^^^^^^^^^^^^^^^^^
     14 |       1 in
          ^^^^^^^
     15 |   let toto : operation list = [op] in

    Not all free variables could be inlined in Tezos.create_contract usage: foo#400. |}];
  run_ligo_good [ "compile"; "contract"; contract "create_contract.mligo" ];
  [%expect
    {|
    File "../../test/contracts/create_contract.mligo", line 7, characters 22-23:
      6 |     Tezos.create_contract
      7 |       (fun (p : nat) (s : string) -> (([] : operation list), "one"))
                                ^
      8 |       (None : key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 7, characters 12-13:
      6 |     Tezos.create_contract
      7 |       (fun (p : nat) (s : string) -> (([] : operation list), "one"))
                      ^
      8 |       (None : key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 4, characters 10-16:
      3 | [@entry]
      4 | let main (action : string) (store : string) : return =
                    ^^^^^^
      5 |   let toto : operation * address =
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter string ;
      storage string ;
      code { CDR ;
             PUSH string "un" ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage string ;
                 code { DROP ; PUSH string "one" ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract.mligo", line 6, characters 4-8:
  5 | [@entry]
  6 | let main (action : parameter) (store : storage) : storage = store + 1
          ^^^^

Not an entrypoint: nat -> int -> int |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract2.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract2.mligo", line 8, characters 4-8:
  7 | [@entry]
  8 | let main (action : parameter) (store : storage) : return = ("bad", store + 1)
          ^^^^

Not an entrypoint: nat -> int -> ( string * int ) |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract3.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract3.mligo", line 8, characters 4-8:
  7 | [@entry]
  8 | let main (action : parameter) (store : storage) : return =
          ^^^^
  9 |   (([] : operation list), "bad")

Not an entrypoint: nat -> int -> ( list (operation) * string ) |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "duplicate_record_field.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/duplicate_record_field.mligo", line 1, characters 9-34:
      1 | type r = { foo : int ; foo : int }
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
      2 |

    Repeated type variable in type.
    Hint: Change the name. |}]

(* uncurrying example *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "uncurry_contract.mligo" ];
  shrink_output [%expect.output];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { LAMBDA (pair unit unit unit unit) unit { DROP ; UNIT } ;
             LAMBDA (pair nat nat) nat { UNPAIR ; MUL } ; |}]

(* old uncurry bugs: *)
let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "let f (y : int) (x : int) (y : int) = (x, y) in f 1 2 3"
    ; "-s"
    ; "cameligo"
    ];
  [%expect {| ( 2 , 3 ) |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "let f (x0 : int) (x1 : int) (x2 : int) (x3 : int) (x4 : int) (x5 : int) (x6 : \
       int) (x7 : int) (x8 : int) (x9 : int) (x10 : int) : int list = [x0; x1; x2; x3; \
       x4; x5; x6; x7; x8; x9; x10] in f 0 1 2 3 4 5 6 7 8 9 10"
    ; "-s"
    ; "cameligo"
    ];
  [%expect
    {|
    CONS(0 ,
         CONS(1 ,
              CONS(2 ,
                   CONS(3 ,
                        CONS(4 ,
                             CONS(5 ,
                                  CONS(6 ,
                                       CONS(7 ,
                                            CONS(8 ,
                                                 CONS(9 ,
                                                      CONS(10 , LIST_EMPTY()))))))))))) |}]

(* uncurrying w/ interpret (old bug) *)
let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "mul 3n 4n"; "--init-file"; contract "uncurry_contract.mligo" ];
  [%expect {| +12 |}]

(* Edo combs example *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "edo_combs.mligo" ];
  [%expect
    {|
    File "../../test/contracts/edo_combs.mligo", line 13, characters 22-23:
     12 | [@entry]
     13 | let main (p : param) (s : int) : operation list * int =
                                ^
     14 |   let {
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (pair (int %x) (int %y) (int %z) (int %w)) ;
      storage int ;
      code { CAR ; UNPAIR 4 ; ADD ; ADD ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_duplicate3.mligo" ];
  [%expect
    {|
    { parameter (pair (nat %ck) (nat %c)) ;
      storage int ;
      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } } |}]

(* warning layout attribute on constructor *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "B 42n"
    ; "--init-file"
    ; contract "warning_layout.mligo"
    ];
  [%expect {|
    (Left 42)
  |}]

(* never test for CameLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "never.mligo" ];
  [%expect
    {|
    { parameter (or (int %increment) (never %extend)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { SWAP ; DROP ; NEVER } ;
             NIL operation ;
             PAIR } } |}]

(* never test for JsLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "never.jsligo" ];
  [%expect
    {|
    { parameter (or (int %increment) (never %extend)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { SWAP ; DROP ; NEVER } ;
             NIL operation ;
             PAIR } } |}]

(* annotations and self *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "self_annotations.mligo" ];
  [%expect
    {|
    { parameter (or (unit %foo) (unit %b)) ;
      storage unit ;
      code { DROP ;
             SELF %foo ;
             PUSH mutez 0 ;
             UNIT ;
             TRANSFER_TOKENS ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "error_self_annotations.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/error_self_annotations.mligo", line 7, characters 10-45:
      6 | let main (_ : param) (_ : unit) : operation list * unit =
      7 |   let c = (Tezos.self ("%a") : unit contract) in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   let op = Tezos.transaction () 0mutez c in

    Invalid entrypoint value.
    The entrypoint value does not match a constructor of the contract parameter. |}]

(* entrypoint check *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_get_entrypoint.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_get_entrypoint.mligo", line 4, character 4 to line 7, character 28:
      3 |   let v =
      4 |     (Tezos.get_entrypoint_opt
              ^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |        "foo"
          ^^^^^^^^^^^^
      6 |        ("tz1fakefakefakefakefakefakefakcphLA5" : address)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |      : unit contract option) in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |   let u : unit =

    Invalid entrypoint "foo". One of the following patterns is expected:
    * "%bar" is expected for entrypoint "Bar"
    * "%default" when no entrypoint is used.
    Valid characters in annotation: ('a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9'). |}]

(* using test in compilation *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "compile_test.mligo"; "-m"; "C" ];
  [%expect {| Invalid usage of a Test primitive: cannot be translated to Michelson. |}]

(* remove unused declarations *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "remove_unused_module.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "remove_unused_toptup.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; PUSH nat 2 ; PUSH nat 1 ; DIG 2 ; ADD ; ADD ; NIL operation ; PAIR } } |}]

(* wrong annotation in Bytes.unpack *)
let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; bad_contract "bad_annotation_unpack.mligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_annotation_unpack.mligo", line 1, characters 9-42:
      1 | let x = (Bytes.unpack (Bytes.pack "hello") : string)
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This expression has type "option (^a)", but an expression was expected of type
    "string".
    Type "option (^a)" is not compatible with type "string".
    Hint: "^a" represent placeholder type(s). |}]

(* check annotations' capitalization *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "annotation_cases.mligo"; "-m"; "A" ];
  [%expect
    {|
    { parameter (pair (nat %AAA) (nat %fooB) (nat %cCC)) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "annotation_cases.mligo"; "-m"; "B" ];
  [%expect
    {|
    { parameter (or (nat %AAA) (or (nat %fooB) (nat %cCC))) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "reuse_variable_name_top.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 2, characters 4-7:
      1 | let dog = 1;
      2 | let dog = true;
              ^^^

    Duplicate identifier. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "reuse_variable_name_block.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/reuse_variable_name_block.jsligo", line 3, characters 8-9:
      2 |     let x = 2;
      3 |     let x = 2;
                  ^
      4 |     return x;

    Duplicate identifier. |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "evaluate-call"; contract "assert.mligo"; "with_error"; "(false, ())" ];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "evaluate-call"
    ; contract "assert.mligo"
    ; "some_with_error"
    ; "(None: unit option)"
    ];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "evaluate-call"
    ; contract "assert.mligo"
    ; "none_with_error"
    ; "(Some (): unit option)"
    ];
  [%expect {| failed with: "my custom error" |}]

(* literal type "casting" inside modules *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "literal_type_cast.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage timestamp ;
      code { DROP ; PUSH timestamp 0 ; NIL operation ; PAIR } } |}]

(* JsLIGO export testing *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_type.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_type.jsligo", line 5, characters 9-16:
        4 |
        5 | type a = Bar.foo
                     ^^^^^^^

      Type "foo" not found. |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_const.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_const.jsligo", line 2, characters 4-15:
        1 | namespace Bar {
        2 |     let foo = 2
                ^^^^^^^^^^^
        3 | }

      Toplevel let declaration is silently changed to const declaration.

      File "../../test/contracts/negative/modules_export_const.jsligo", line 5, characters 0-15:
        4 |
        5 | let a = Bar.foo;
            ^^^^^^^^^^^^^^^

      Toplevel let declaration is silently changed to const declaration.

      File "../../test/contracts/negative/modules_export_const.jsligo", line 5, characters 8-15:
        4 |
        5 | let a = Bar.foo;
                    ^^^^^^^

      Variable "foo" not found. |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_namespace.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_namespace.jsligo", line 3, characters 8-17:
        2 |     namespace Foo {
        3 |         let a = 2;
                    ^^^^^^^^^
        4 |     }

      Toplevel let declaration is silently changed to const declaration.

      File "../../test/contracts/negative/modules_export_namespace.jsligo", line 7, characters 13-20:
        6 |
        7 | import Foo = Bar.Foo
                         ^^^^^^^

       Module "Bar.Foo" not found. |}];
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "y"
    ; "--init-file"
    ; bad_contract "modules_export_importer.jsligo"
    ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_importer.jsligo", line 3, characters 10-13:
        2 |
        3 | const y : M.t = M.x;
                      ^^^

      Type "t" not found. |}]

(* Test compile contract with Big_map.get_and_update for Hangzhou *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "ticket_wallet.mligo" ];
  [%expect
    {|
    { parameter
        (or (ticket %receive unit)
            (pair %send (contract %destination (ticket unit)) (nat %amount) (address %ticketer))) ;
      storage (pair (address %manager) (big_map %tickets address (ticket unit))) ;
      code { UNPAIR ;
             PUSH mutez 0 ;
             AMOUNT ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             SWAP ;
             UNPAIR ;
             DIG 2 ;
             IF_LEFT
               { READ_TICKET ;
                 CAR ;
                 DIG 3 ;
                 NONE (ticket unit) ;
                 DUP 3 ;
                 GET_AND_UPDATE ;
                 IF_NONE
                   { DIG 2 }
                   { DIG 3 ;
                     PAIR ;
                     JOIN_TICKETS ;
                     IF_NONE { PUSH string "impossible?" ; FAILWITH } {} } ;
                 SOME ;
                 DIG 2 ;
                 UPDATE ;
                 SWAP ;
                 PAIR ;
                 NIL operation ;
                 PAIR }
               { DUP 2 ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                 DIG 2 ;
                 NONE (ticket unit) ;
                 DUP 3 ;
                 GET 4 ;
                 GET_AND_UPDATE ;
                 IF_NONE
                   { DROP 3 ; PUSH string "no tickets" ; FAILWITH }
                   { READ_TICKET ;
                     CDR ;
                     CDR ;
                     DUP 4 ;
                     GET 3 ;
                     DUP ;
                     DIG 2 ;
                     SUB ;
                     ISNAT ;
                     IF_NONE { PUSH string "not enough tickets" ; FAILWITH } {} ;
                     SWAP ;
                     PAIR ;
                     SWAP ;
                     SPLIT_TICKET ;
                     IF_NONE
                       { DROP 3 ; PUSH string "impossible?" ; FAILWITH }
                       { UNPAIR ;
                         DUG 2 ;
                         SOME ;
                         DUP 4 ;
                         GET 4 ;
                         UPDATE ;
                         DIG 2 ;
                         CAR ;
                         PUSH mutez 0 ;
                         DIG 3 ;
                         TRANSFER_TOKENS ;
                         SWAP ;
                         DIG 2 ;
                         PAIR ;
                         NIL operation ;
                         DIG 2 ;
                         CONS ;
                         PAIR } } } } } |}]

(* source location comments *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "noop.mligo"
    ; "--michelson-comments"
    ; "location"
    ; "--michelson-comments"
    ; "env"
    ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { { /* _ */ } ;
             CDR ;
             { /* _ */ } ;
             { /* File "../../test/contracts/noop.mligo", line 3, character 2 to line 7, character 28 */
               { /* File "../../test/contracts/noop.mligo", line 3, characters 28-29 */
                 LAMBDA
                   unit
                   unit
                   { { /* x */ } ;
                     { /* File "../../test/contracts/noop.mligo", line 3, characters 28-29 */ } } } ;
               { /* f, _ */ } ;
               { /* File "../../test/contracts/noop.mligo", line 4, character 2 to line 7, character 28 */
                 { /* File "../../test/contracts/noop.mligo", line 4, characters 18-21 */
                   SWAP ;
                   { /* File "../../test/contracts/noop.mligo", line 4, characters 18-19 */ DUP 2 } ;
                   SWAP ;
                   EXEC } ;
                 { /* s2, f */ } ;
                 { /* File "../../test/contracts/noop.mligo", line 5, character 2 to line 7, character 28 */
                   { /* File "../../test/contracts/noop.mligo", line 5, characters 18-22 */
                     { /* File "../../test/contracts/noop.mligo", line 5, characters 20-22 */ } ;
                     { /* File "../../test/contracts/noop.mligo", line 5, characters 18-19 */ DUP 2 } ;
                     SWAP ;
                     EXEC } ;
                   { /* s3, f */ } ;
                   { /* File "../../test/contracts/noop.mligo", line 6, character 2 to line 7, character 28 */
                     { /* File "../../test/contracts/noop.mligo", line 6, characters 10-14 */
                       { /* File "../../test/contracts/noop.mligo", line 6, characters 12-14 */ } ;
                       { /* File "../../test/contracts/noop.mligo", line 6, characters 10-11 */ SWAP } ;
                       SWAP ;
                       EXEC } ;
                     { /* s */ } ;
                     { /* File "../../test/contracts/noop.mligo", line 7, characters 3-27 */
                       { /* File "../../test/contracts/noop.mligo", line 7, characters 26-27 */ } ;
                       { /* File "../../test/contracts/noop.mligo", line 7, characters 3-24 */
                         NIL operation
                             /* File "../../test/contracts/noop.mligo", line 7, characters 3-24 */
                         /* File "../../test/contracts/noop.mligo", line 7, characters 3-24 */ } ;
                       PAIR } } } } } } } |}]

(* JSON source location comments *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "noop.mligo"
    ; "--michelson-format"
    ; "json"
    ; "--michelson-comments"
    ; "location"
    ; "--michelson-comments"
    ; "env"
    ];
  (* hmm, just checking that there is no error, should not use
     ppx_expect... *)
  let _ = [%expect.output] in
  [%expect {||}]

let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "module_contract_simple.mligo"; "999" ];
  [%expect {| 999 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "module_contract_simple.mligo"
    ; "Add 999"
    ; "-e"
    ; "main"
    ];
  [%expect {| (Left 999) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "module_contract_complex.mligo"
    ; "{ number = 999 ; previous_action = Reset }"
    ];
  [%expect {| (Pair 999 (Right (Right Unit))) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "module_contract_complex.mligo"
    ; "Add 999"
    ; "-e"
    ; "main"
    ];
  [%expect {| (Left 999) |}]

(* Global constants *)

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "global_constant.mligo"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ;
             constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "global_constant.mligo"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ;
             constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "global_constant.mligo"
    ; "()"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ; "-e"
    ; "main"
    ];
  [%expect {| Unit |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant.mligo"
    ; "v"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| 128 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant.mligo"
    ; "42"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant.mligo"
    ; "42"
    ; "--file-constants"
    ; contract_resource "const.json"
    ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "v"
    ; "--init-file"
    ; contract "global_constant.mligo"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| 128 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "v"
    ; "--init-file"
    ; contract "global_constant.mligo"
    ; "--file-constants"
    ; contract_resource "const.json"
    ];
  [%expect {|
    128 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant_lambda.mligo"
    ; "s"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| (Pair 1 { PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant_lambda.mligo"
    ; "s"
    ; "--file-constants"
    ; contract_resource "const.json"
    ];
  [%expect {| (Pair 1 { PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "constant"
    ; "cameligo"
    ; "fun (x : int) -> if x > 3 then x * 2 else x * String.length \"fja\" + 1"
    ];
  [%expect
    {|
    Michelson constant as JSON string:
    "{ PUSH int 3 ;\n  DUP 2 ;\n  COMPARE ;\n  GT ;\n  IF { PUSH int 2 ; SWAP ; MUL }\n     { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }"
    This string can be passed in `--constants` argument when compiling a contract.

    Remember to register it in the network, e.g.:
    > tezos-client register global constant "{ PUSH int 3 ;
      DUP 2 ;
      COMPARE ;
      GT ;
      IF { PUSH int 2 ; SWAP ; MUL }
         { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }" from bootstrap1

    Constant hash:
    exprtr7GE1A1cR39zNGRGF44aGfAX23tC7szWrnLzs9fkUhasLEcQT |}]

(* Test pairing_check and bls12_381_g1/g2/fr literals *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "test"
    ; "--init-file"
    ; contract "pairing_check.mligo"
    ];
  [%expect {| Unit |}]

(* Test decompilation of bls12_381_g1/g2/fr literals *)
let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "(alpha, beta, input_x)"
    ; "--init-file"
    ; contract "pairing_check.mligo"
    ];
  [%expect
    {|
    ( bls12_381_g1 0x024142bc89bf29017a38d0ee97711098639aa0bbc5b54b5104cc88b1c0fd09330fb8341e3da91e7a50f0da5c988517db0f52df51f745392ecdd3ffbb50f8a25fcdec6f48886b650de26821e244cb8ab69d49722d290a420ce1284b909d3e15a0 ,
      bls12_381_g2 0x0050b3ab4877c99ce7f180e879d91eb4df24b1e20ed88f1fdde42f91dfe0e7e451aa35d1457dd15ab507fc8f2b3180550ca7b4ea9b67810e346456c35060c8d542f37ee5fe2b1461e2f02fefac55a9863e94cab5c16befad3b866a42ee20835b1351f3f9c20a05586c1d647d756efb5c575d7ab23fbf5b3e1a6ffe024633a63a668a01fcab440866035ea2c0d4bfe30a1242f67119650e2aa605289ade2684287192382d6a01d7865fcd9e1507264a80f387b6441e37438c888159827a4efa67 ,
      bls12_381_fr 0xe406000000000000000000000000000000000000000000000000000000000000 ) |}]

(* Example contracts from getting-started *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment.mligo" ];
  [%expect
    {|
    { parameter (or (int %increment) (or (int %decrement) (unit %reset))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { IF_LEFT { SWAP ; SUB } { DROP 2 ; PUSH int 0 } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment.jsligo" ];
  [%expect
    {|
    { parameter (or (int %increment) (or (int %decrement) (unit %reset))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { IF_LEFT { SWAP ; SUB } { DROP 2 ; PUSH int 0 } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "increment.ligo" ];
  [%expect
    {|
    Invalid file extension for '../../test/contracts/increment.ligo'.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "pascaligo"; "1 + 1" ];
  [%expect
    {|
    Invalid syntax.
    PascaLIGO is deprecated.
    Hint: You can use LIGO 0.73.0 with the --deprecated flag. |}]

(* Test compiling a contract with a get_entrypoint_opt to a capitalized entrypoint *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "get_capitalized_entrypoint.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             SENDER ;
             CONTRACT %Upper unit ;
             IF_NONE
               { PUSH string "lol" ; FAILWITH }
               { PUSH mutez 0 ;
                 UNIT ;
                 TRANSFER_TOKENS ;
                 UNIT ;
                 NIL operation ;
                 DIG 2 ;
                 CONS ;
                 PAIR } } } |}]

(* Test compiling parameter in a file which uses test primitives *)
let%expect_test _ =
  run_ligo_good
    [ "compile"; "parameter"; contract "increment_with_test.mligo"; "z.1"; "-e"; "main" ];
  [%expect {| (Left 32) |}]

(* Test compiling storage in a file which uses test primitives *)
let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "increment_with_test.mligo"; "z.0 + 10" ];
  [%expect {| 42 |}]

(* Test compiling expression with curried recursive function *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "foo 2 \"titi\""
    ; "--init-file"
    ; contract "recursion_uncurry.mligo"
    ];
  [%expect {|
    "tititotototo" |}]

(* Test compiling contract with curried recursive function *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "recursion_uncurry.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage string ;
      code { LEFT string ;
             LOOP_LEFT
               { UNPAIR ;
                 PUSH int 0 ;
                 DUP 2 ;
                 COMPARE ;
                 EQ ;
                 IF { DROP ; RIGHT (pair int string) }
                    { PUSH string "toto" ;
                      DIG 2 ;
                      CONCAT ;
                      PUSH int 1 ;
                      DIG 2 ;
                      SUB ;
                      PAIR ;
                      LEFT string } } ;
             NIL operation ;
             PAIR } } |}]

(* Compiling contract with non-tail recursion *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "lambdarec.mligo" ];
  [%expect
    {|
    { parameter (list int) ;
      storage (list int) ;
      code { LAMBDA_REC
               (pair (list int) (list int))
               (list int)
               { UNPAIR ;
                 IF_CONS
                   { DUG 2 ; PAIR ; DIG 2 ; SWAP ; EXEC ; SWAP ; CONS }
                   { SWAP ; DROP } } ;
             SWAP ;
             EXEC ;
             NIL operation ;
             PAIR } } |}]

(* Compiling CameLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "cat [fib 1; fib 2; fib 3] [fib 4; fib 5; fib 6;  fib 7]"
    ; "--init-file"
    ; contract "lambdarec.mligo"
    ];
  [%expect {|
    { 1 ; 2 ; 3 ; 5 ; 8 ; 13 ; 21 } |}]

(* Compiling CameLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "foo"
    ; "--init-file"
    ; contract "lambdarec2.mligo"
    ];
  [%expect
    {|
    (Lambda_rec
       { LEFT int ;
         LOOP_LEFT { DUP ; DUP 3 ; SWAP ; EXEC ; CONS ; LEFT int } ;
         SWAP ;
         DROP }) |}]

(* Compiling CameLIGO Ackermann expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "ackermann (3n, 2n)"
    ; "--init-file"
    ; contract "lambdarec.mligo"
    ];
  [%expect {|
    29 |}]

(* Compiling JsLIGO expression with non-tail recursion using env. *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "fib2()(5)"
    ; "--init-file"
    ; contract "lambdarec.jsligo"
    ];
  [%expect {|
    8 |}]

(* Compiling JsLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "cat(list([1,2,3]), list([4,fib(5)]))"
    ; "--init-file"
    ; contract "lambdarec.jsligo"
    ];
  [%expect {|
    { 1 ; 2 ; 3 ; 4 ; 8 } |}]

(* Compiling JsLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "wrong"
    ; "--init-file"
    ; contract "lambdarec2.jsligo"
    ];
  [%expect
    {|
    (Lambda_rec
       { PUSH int 0 ;
         NIL int ;
         DUP 3 ;
         CONS ;
         DIG 2 ;
         CONS ;
         ITER { DUP 3 ; SWAP ; EXEC ; ADD } ;
         SWAP ;
         DROP }) |}]

(* Compiling expression with non-tail recursion for rose tree map *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "sum t"
    ; "--init-file"
    ; contract "rose_tree.mligo"
    ];
  [%expect {|
    53 |}]

(* voting power *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "voting.mligo" ];
  [%expect
    {|
    { parameter key ;
      storage (pair nat nat) ;
      code { CAR ;
             HASH_KEY ;
             VOTING_POWER ;
             TOTAL_VOTING_POWER ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "michelson_typed_opt.mligo"
    ; "-m"
    ; "A"
    ; "--enable-michelson-typed-opt"
    ];
  [%expect
    {|
    { parameter (or (pair %one (nat %x) (int %y)) (pair %two (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT {} {} ; CAR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "michelson_typed_opt.mligo"
    ; "-m"
    ; "B"
    ; "--enable-michelson-typed-opt"
    ];
  [%expect
    {|
    { parameter (or (pair %onee (nat %x) (int %y)) (pair %three (nat %x) (int %z))) ;
      storage nat ;
      code { CAR ; IF_LEFT {} {} ; CAR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "michelson_typed_opt.mligo"
    ; "-m"
    ; "C"
    ; "--enable-michelson-typed-opt"
    ];
  [%expect
    {|
    { parameter (or (pair %oneee (nat %x) (int %y)) (pair %four (nat %x) (timestamp %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "michelson_typed_opt.mligo"; "-m"; "A" ];
  [%expect
    {|
    { parameter (or (pair %one (nat %x) (int %y)) (pair %two (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "michelson_typed_opt.mligo"; "-m"; "B" ];
  [%expect
    {|
    { parameter (or (pair %onee (nat %x) (int %y)) (pair %three (nat %x) (int %z))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "michelson_typed_opt.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (pair %oneee (nat %x) (int %y)) (pair %four (nat %x) (timestamp %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "y"
    ; "--init-file"
    ; contract "extend_builtin.mligo"
    ];
  [%expect {|
44 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "y"
    ; "--init-file"
    ; contract "extend_builtin.jsligo"
    ];
  [%expect
    {|
File "../../test/contracts/extend_builtin.jsligo", line 2, characters 9-19:
  1 | namespace Tezos {
  2 |   export let x = 42;
               ^^^^^^^^^^
  3 |   export let f = (x  : int) : int => x + 2;

Toplevel let declaration is silently changed to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 3, characters 9-42:
  2 |   export let x = 42;
  3 |   export let f = (x  : int) : int => x + 2;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  4 | }

Toplevel let declaration is silently changed to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 6, characters 0-24:
  5 |
  6 | let y = Tezos.f(Tezos.x);
      ^^^^^^^^^^^^^^^^^^^^^^^^

Toplevel let declaration is silently changed to const declaration.

44 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "thunk.mligo" ];
  [%expect
    {|
    { parameter string ;
      storage string ;
      code { CDR ;
             SENDER ;
             PUSH mutez 1000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage address ;
                 code { DROP ; SENDER ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "--werror"; contract "tzip16-compatible.mligo" ];
  [%expect
    {|
    { parameter string ;
      storage (pair (int %data) (big_map %metadata string bytes)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

(* check compiling many (more than 10) views *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "call_view_impure.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH address "tz1fakefakefakefakefakefakefakcphLA5" ;
             SENDER ;
             VIEW "foo" unit ;
             IF_NONE { UNIT } {} ;
             NIL operation ;
             PAIR } } |}]

(* check compiling view call for a non-literal *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "call_view_not_litstr.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/call_view_not_litstr.mligo", line 4, character 10 to line 8, character 21:
      3 |   let u =
      4 |     match (Tezos.call_view
                    ^^^^^^^^^^^^^^^^
      5 |          s
          ^^^^^^^^^^
      6 |          (Tezos.get_sender ())
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |          ("tz1fakefakefakefakefakefakefakcphLA5" : address)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      8 |        : unit option)
          ^^^^^^^^^^^^^^^^^^^^^
      9 |     with

    Invalid argument.
    View name must be a string literal. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "shadowed_sum_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/shadowed_sum_type.mligo", line 13, characters 8-12:
     12 |
     13 | let x = A 42
                  ^^^^
     14 |

    Constructor "A" not found.
  |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract_return_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_contract_return_type.mligo", line 5, characters 14-23:
      4 |
      5 | let main (_ : parameter) (s : storage) : _return =
                        ^^^^^^^^^
      6 |     [], s, 1tez

    Type "parameter" not found.
  |}]

(* ignore in JsLIGO *)
let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "jsligo"; "test"; "--init-file"; contract "ignore.jsligo" ];
  [%expect {| 1 |}]

(* bytes literals using raw_code *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "tests"
    ; "--init-file"
    ; contract "bytes_literals.mligo"
    ];
  [%expect {| { True ; True ; True } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "tests"
    ; "--init-file"
    ; contract "bytes_literals.jsligo"
    ];
  [%expect {| (Pair True True True) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "realistic"
    ; "--init-file"
    ; contract "bytes_literals.jsligo"
    ];
  [%expect
    {| 0x0a202020207b0a20202020226e616d65223a226e616d65222c0a20202020226465736372697074696f6e223a226465736372697074696f6e222c0a202020202276657273696f6e223a22302e302e30222c0a20202020226c6963656e7365223a7b226e616d65223a226c696e616d65227d2c0a2020202022617574686f7273223a5b22617574686f7273225d2c0a2020202022686f6d6570616765223a22222c0a2020202022736f75726365223a7b22746f6f6c73223a5b22746f6f6c73225d2c20226c6f636174696f6e223a226c6f636174696f6e227d2c0a2020202022696e7465726661636573223a5b22545a4950225d2c0a20202020226572726f7273223a5b5d2c0a20202020227669657773223a5b5d0a202020207d0a2020 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_contract "bytes_literals.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bytes_literals.jsligo", line 2, characters 12-23:
      1 | const shame = () => {
      2 |   const x = bytes `foo` as nat;
                      ^^^^^^^^^^^
      3 |   return x

    Invalid type(s).
    Expected "nat", but got: "bytes". |}]

(* get_entrypoint_opt in uncurried language *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "get_entrypoint.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/get_entrypoint.jsligo", line 3, characters 26-38:
      2 | const main = (_u : unit, _b : address) : [list <operation>, address] => {
      3 |   let c : contract<int> = Option.unopt(Tezos.get_entrypoint_opt ("%foo", Tezos.get_sender()));
                                    ^^^^^^^^^^^^
      4 |   return [list([]) as list <operation>, Tezos.address(c)];
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    { parameter unit ;
      storage address ;
      code { DROP ;
             SENDER ;
             CONTRACT %foo int ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             ADDRESS ;
             NIL operation ;
             PAIR } } |}]

(* make sure that in compile storage/expression we can check ENTRYPOINT/EMIT *)
let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "emit.mligo"; "()" ];
  [%expect {| Unit |}]

(* make sure that in compile storage/expression we can check SELF *)
let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "self_annotations.mligo"; "()" ];
  [%expect {| Unit |}]

(* check tag in Tezos.emit *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "emit_bad_tag.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/emit_bad_tag.mligo", line 3, characters 3-31:
      2 | let main (_ : unit) (_ : string) : operation list * string =
      3 |   [Tezos.emit "%hello world" 12], "bye"
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Invalid entrypoint "%hello world". One of the following patterns is expected:
    * "%bar" is expected for entrypoint "Bar"
    * "%default" when no entrypoint is used.
    Valid characters in annotation: ('a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9'). |}]

(* test compile parameter w.r.t. @entry *)
let%expect_test _ =
  run_ligo_good [ "compile"; "parameter"; contract "single.contract.jsligo"; "Poke()" ];
  [%expect {| Unit |}];
  run_ligo_good
    [ "compile"; "parameter"; contract "single.contract.jsligo"; "[]"; "-e"; "poke" ];
  [%expect {| Unit |}];
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "single.parameter.jsligo"
    ; "[]"
    ; "-e"
    ; "poke"
    ; "-m"
    ; "Contract"
    ];
  [%expect {|
    Unit |}];
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "single.parameter.jsligo"
    ; "Poke()"
    ; "-m"
    ; "Contract"
    ];
  [%expect {|
    Unit |}];
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "single.parameter.jsligo"
    ; "default_parameter"
    ; "-m"
    ; "Contract"
    ];
  [%expect {|
    Unit |}]

(* make sure that in compile storage we annotate the type *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "annotated_storage_and_parameter.mligo"
    ; "Map.empty"
    ];
  [%expect {| {} |}]

(* make sure that in compile parameter we annotate the type *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "annotated_storage_and_parameter.mligo"
    ; "[]"
    ; "-e"
    ; "main"
    ];
  [%expect {| {} |}]

(* make sure that in compile parameter we do not allow polymorphic type *)
let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "parameter"
    ; bad_contract "annotated_storage_and_parameter.mligo"
    ; "([] : int list)"
    ];
  [%expect {|
    File is not a contract |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "disc_union_vbar.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/disc_union_vbar.jsligo", line 14, characters 10-20:
     13 |   let planetType = p.planetType;
     14 |   switch (planetType.kind) {
                    ^^^^^^^^^^
     15 |     case "Tellurian":
    :
    Warning: unused variable "planetType".
    Hint: replace it by "_planetType" to prevent this warning.

    File "../../test/contracts/disc_union_vbar.jsligo", line 14, characters 10-20:
     13 |   let planetType = p.planetType;
     14 |   switch (planetType.kind) {
                    ^^^^^^^^^^
     15 |     case "Tellurian":
    :
    Warning: unused variable "planetType".
    Hint: replace it by "_planetType" to prevent this warning.

    File "../../test/contracts/disc_union_vbar.jsligo", line 14, characters 10-20:
     13 |   let planetType = p.planetType;
     14 |   switch (planetType.kind) {
                    ^^^^^^^^^^
     15 |     case "Tellurian":
    :
    Warning: unused variable "planetType".
    Hint: replace it by "_planetType" to prevent this warning.

    { parameter
        (pair (string %name)
              (or %planetType (unit %tellurian) (or (unit %gaseous) (unit %other)))
              (option %lord address)) ;
      storage int ;
      code { CAR ;
             PUSH int 0 ;
             SWAP ;
             GET 3 ;
             IF_LEFT
               { DROP ; PUSH int 1 ; ADD }
               { IF_LEFT { DROP ; PUSH int 2 ; SWAP ; SUB } { DROP 2 ; PUSH int 0 } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "layout_comb.jsligo" ];
  [%expect
    {|
    { parameter (or (unit %a) (or (int %b) (pair %c int int))) ;
      storage (pair (int %x) (int %y) (int %z)) ;
      code { CAR ;
             IF_LEFT
               { DROP ; PUSH int 10 ; PUSH int 10 ; PUSH int 10 }
               { IF_LEFT
                   { DROP ; PUSH int 20 ; PUSH int 20 ; PUSH int 20 }
                   { DROP ; PUSH int 20 ; PUSH int 20 ; PUSH int 20 } } ;
             PAIR 3 ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "contract_type_vars_let_fun.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (list int) ;
      code { DROP ;
             NIL int ;
             PUSH int 3 ;
             CONS ;
             PUSH int 2 ;
             CONS ;
             PUSH int 1 ;
             CONS ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "used_var_in_local_module.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "entrypoint_in_module.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (int %increment) (or (int %decrement) (unit %reset))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { IF_LEFT { SWAP ; SUB } { DROP 2 ; PUSH int 0 } } ;
             NIL operation ;
             PAIR } ;
      view "foo" int int { UNPAIR ; ADD } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "entrypoint_in_module.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (int %increment) (or (int %decrement) (unit %reset))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { IF_LEFT { SWAP ; SUB } { DROP 2 ; PUSH int 0 } } ;
             NIL operation ;
             PAIR } ;
      view "foo" int int { UNPAIR ; ADD } } |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "entrypoint_in_module.mligo"; "-m"; "Bar" ];
  [%expect {|
    Bar is not a contract |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "entrypoint_in_module.mligo"; "-m"; "Barrau" ];
  [%expect {|
    Barrau is not a contract |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "entrypoint_in_module.mligo"
    ; "32"
    ; "-m"
    ; "C"
    ; "-e"
    ; "increment"
    ];
  [%expect {| (Left 32) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "storage"; contract "entrypoint_in_module.mligo"; "5"; "-m"; "C" ];
  [%expect {| 5 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "parameter"; contract "top_level_entry.mligo"; "42"; "-e"; "increment" ];
  [%expect {|
      (Right (Right 42)) |}];
  run_ligo_good
    [ "compile"; "parameter"; contract "top_level_entry.mligo"; "Increment 42" ];
  [%expect {| (Right (Right 42)) |}];
  run_ligo_good
    [ "compile"; "parameter"; contract "top_level_entry.mligo"; "Decrement 21" ];
  [%expect {| (Right (Left 21)) |}];
  run_ligo_good [ "compile"; "storage"; contract "top_level_entry.mligo"; "42" ];
  [%expect {| 42 |}];
  run_ligo_good [ "compile"; "storage"; contract "top_level_entry.mligo"; "42" ];
  [%expect {| 42 |}];
  run_ligo_good [ "compile"; "storage"; contract "top_level_entry.mligo"; "42" ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "entrypoint_in_module.mligo"
    ; "Increment 5"
    ; "37"
    ; "-m"
    ; "C"
    ];
  [%expect {| ( LIST_EMPTY() , 42 ) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "entrypoint_in_module.jsligo"; "-m"; "M.C" ];
  [%expect
    {|
    { parameter (or (int %increment) (or (int %decrement) (unit %reset))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { IF_LEFT { SWAP ; SUB } { DROP 2 ; PUSH int 0 } } ;
             NIL operation ;
             PAIR } ;
      view "foo" int int { UNPAIR ; ADD } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "entrypoint_no_type.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/entrypoint_no_type.jsligo", line 8, characters 6-12:
      7 | @entry
      8 | const unique = (_p : organization, _s : storage) => {
                ^^^^^^
      9 |     return failwith("You need to be part of Tezos organization to activate an organization");

    Not an entrypoint: [_p]record[admins -> int , name -> string] -> ∀ gen#5 : * . [_s]int -> gen#5 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment_module.jsligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (int %increment) (or (int %decrement) (unit %reset))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { ADD } { IF_LEFT { SWAP ; SUB } { DROP 2 ; PUSH int 0 } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "FA1.2.entries.mligo" ];
  [%expect
    {|
    { parameter
        (or (pair %getTotalSupply (unit %request) (contract %callback nat))
            (or (pair %getBalance (address %owner) (contract %callback nat))
                (or (pair %getAllowance
                       (pair %request (address %owner) (address %spender))
                       (contract %callback nat))
                    (or (pair %approve (address %spender) (nat %value))
                        (pair %transfer (address %from) (address %to) (nat %value)))))) ;
      storage
        (pair (big_map %tokens address nat)
              (big_map %allowances (pair (address %owner) (address %spender)) nat)
              (nat %total_supply)) ;
      code { UNPAIR ;
             IF_LEFT
               { DUP 2 ;
                 NIL operation ;
                 DIG 2 ;
                 CDR ;
                 PUSH mutez 0 ;
                 DIG 4 ;
                 GET 4 ;
                 TRANSFER_TOKENS ;
                 CONS }
               { IF_LEFT
                   { DUP 2 ;
                     NIL operation ;
                     DUP 3 ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 5 ;
                     CAR ;
                     DIG 5 ;
                     CAR ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     TRANSFER_TOKENS ;
                     CONS }
                   { IF_LEFT
                       { DUP 2 ;
                         NIL operation ;
                         DUP 3 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 5 ;
                         GET 3 ;
                         DIG 5 ;
                         CAR ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         TRANSFER_TOKENS ;
                         CONS }
                       { IF_LEFT
                           { DUP 2 ;
                             GET 3 ;
                             DUP 2 ;
                             CAR ;
                             SENDER ;
                             PAIR ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             CDR ;
                             COMPARE ;
                             GT ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             DUP 4 ;
                             GET ;
                             IF_NONE { PUSH nat 0 } {} ;
                             COMPARE ;
                             GT ;
                             AND ;
                             IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                             DIG 2 ;
                             CDR ;
                             DIG 3 ;
                             DIG 3 ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             COMPARE ;
                             EQ ;
                             IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
                             DIG 3 ;
                             UPDATE ;
                             UPDATE 3 }
                           { DUP 2 ;
                             GET 3 ;
                             DUP 3 ;
                             CAR ;
                             DUP 3 ;
                             CAR ;
                             SENDER ;
                             COMPARE ;
                             EQ ;
                             IF { SWAP }
                                { SENDER ;
                                  DUP 4 ;
                                  CAR ;
                                  PAIR ;
                                  DUP 4 ;
                                  GET 4 ;
                                  DUP 4 ;
                                  DUP 3 ;
                                  GET ;
                                  IF_NONE { PUSH nat 0 } {} ;
                                  SUB ;
                                  ISNAT ;
                                  IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                                  DIG 3 ;
                                  PUSH nat 0 ;
                                  DUP 3 ;
                                  COMPARE ;
                                  EQ ;
                                  IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                                  DIG 2 ;
                                  UPDATE } ;
                             DUP 3 ;
                             GET 4 ;
                             DUP 3 ;
                             DUP 5 ;
                             CAR ;
                             GET ;
                             IF_NONE { PUSH nat 0 } {} ;
                             SUB ;
                             ISNAT ;
                             IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                             DIG 2 ;
                             PUSH nat 0 ;
                             DUP 3 ;
                             COMPARE ;
                             EQ ;
                             IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                             DUP 4 ;
                             CAR ;
                             UPDATE ;
                             DUP 3 ;
                             GET 4 ;
                             DUP 2 ;
                             DUP 5 ;
                             GET 3 ;
                             GET ;
                             IF_NONE { PUSH nat 0 } {} ;
                             ADD ;
                             DIG 4 ;
                             DIG 2 ;
                             PUSH nat 0 ;
                             DUP 4 ;
                             COMPARE ;
                             EQ ;
                             IF { DIG 2 ; DROP ; NONE nat } { DIG 2 ; SOME } ;
                             DIG 4 ;
                             GET 3 ;
                             UPDATE ;
                             UPDATE 1 ;
                             SWAP ;
                             UPDATE 3 } ;
                         NIL operation } } } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "FA1.2.entries.mligo"
    ; "Approve { spender = (\"tz1fakefakefakefakefakefakefakcphLA5\" : address) ; value \
       = 3n }"
    ];
  [%expect
    {| (Right (Right (Right (Left (Pair "tz1fakefakefakefakefakefakefakcphLA5" 3))))) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "pokeGame.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/pokeGame.jsligo", line 102, characters 31-43:
    101 |   } else {
    102 |     const t : ticket<string> = Option.unopt(Tezos.create_ticket("can_poke", ticketCount));
                                         ^^^^^^^^^^^^
    103 |     return [
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    { parameter
        (or (pair %init address nat) (or (address %pokeAndGetFeedback) (unit %poke))) ;
      storage
        (pair (map %pokeTraces address (pair (address %receiver) (string %feedback)))
              (string %feedback)
              (map %ticketOwnership address (ticket string))) ;
      code { UNPAIR ;
             IF_LEFT
               { SWAP ;
                 UNPAIR 3 ;
                 PUSH nat 0 ;
                 DUP 5 ;
                 CDR ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 3 ; DROP ; DIG 2 }
                    { DUP 4 ;
                      CDR ;
                      PUSH string "can_poke" ;
                      TICKET ;
                      IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
                      DIG 3 ;
                      SWAP ;
                      SOME ;
                      DIG 4 ;
                      CAR ;
                      UPDATE } ;
                 DUG 2 ;
                 PAIR 3 ;
                 NIL operation ;
                 PAIR }
               { IF_LEFT
                   { SWAP ;
                     UNPAIR 3 ;
                     SWAP ;
                     DROP ;
                     SWAP ;
                     NONE (ticket string) ;
                     SOURCE ;
                     GET_AND_UPDATE ;
                     DUP 4 ;
                     UNIT ;
                     VIEW "feedback" string ;
                     SWAP ;
                     IF_NONE
                       { DROP 4 ;
                         PUSH string "User does not have tickets => not allowed" ;
                         FAILWITH }
                       { DROP ;
                         IF_NONE
                           { DROP 3 ;
                             PUSH string "Cannot find view feedback on given oracle address" ;
                             FAILWITH }
                           { SWAP ;
                             DUP 2 ;
                             DIG 3 ;
                             DIG 3 ;
                             DIG 4 ;
                             PAIR ;
                             SOURCE ;
                             DUG 2 ;
                             SOME ;
                             DIG 2 ;
                             UPDATE ;
                             PAIR 3 ;
                             NIL operation ;
                             PAIR } } }
                   { DROP ;
                     UNPAIR 3 ;
                     DIG 2 ;
                     NONE (ticket string) ;
                     SOURCE ;
                     GET_AND_UPDATE ;
                     IF_NONE
                       { DROP 3 ;
                         PUSH string "User does not have tickets => not allowed" ;
                         FAILWITH }
                       { DROP ;
                         DUG 2 ;
                         PUSH string "" ;
                         SELF_ADDRESS ;
                         PAIR ;
                         SOURCE ;
                         DUG 2 ;
                         SOME ;
                         DIG 2 ;
                         UPDATE ;
                         PAIR 3 ;
                         NIL operation ;
                         PAIR } } } } ;
      view "feedback" unit string { CDR ; GET 3 } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "parameter"; contract "pokeGame.jsligo"; "Poke()" ];
  [%expect
    {|
    File "../../test/contracts/pokeGame.jsligo", line 102, characters 31-43:
    101 |   } else {
    102 |     const t : ticket<string> = Option.unopt(Tezos.create_ticket("can_poke", ticketCount));
                                         ^^^^^^^^^^^^
    103 |     return [
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    (Right (Right Unit)) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "contract_of.jsligo" ];
  [%expect
    {|
    { parameter int ;
      storage int ;
      code { UNPAIR ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "entries_in_module.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (unit %reset) (or (int %sub) (int %add))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { DROP 2 ; PUSH int 0 } { IF_LEFT { SWAP ; SUB } { ADD } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "bytes_bitwise.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH nat 8 ;
             PUSH bytes 0x06 ;
             LSL ;
             PUSH nat 1 ;
             PUSH bytes 0x0006 ;
             LSR ;
             PUSH bytes 0x0003 ;
             SWAP ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0600 ;
             DIG 2 ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0103 ;
             PUSH bytes 0x0106 ;
             PUSH bytes 0x0005 ;
             XOR ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0107 ;
             PUSH bytes 0x0106 ;
             PUSH bytes 0x0005 ;
             OR ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0004 ;
             PUSH bytes 0x0106 ;
             PUSH bytes 0x0005 ;
             AND ;
             COMPARE ;
             EQ ;
             AND ;
             AND ;
             AND ;
             AND ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             UNIT ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "dry-run"; contract "bytes_bitwise.mligo"; "()"; "()" ];
  [%expect {| ( LIST_EMPTY() , unit ) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "bytes_int_nat_conv.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH bytes 0x123456 ;
             DUP ;
             NAT ;
             BYTES ;
             DUP 2 ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DUP ;
             INT ;
             BYTES ;
             SWAP ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             PUSH int 1234 ;
             BYTES ;
             INT ;
             PUSH int 1234 ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             PUSH nat 4567 ;
             BYTES ;
             NAT ;
             PUSH nat 4567 ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             UNIT ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "dry-run"; contract "bytes_int_nat_conv.mligo"; "()"; "()" ];
  [%expect {| ( LIST_EMPTY() , unit ) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "increment_prefix.jsligo"; "-m"; "IncDec" ];
  [%expect
    {|
    { parameter (or (unit %reset) (or (unit %decrement) (unit %increment))) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT
               { DROP 2 ; PUSH int 0 }
               { IF_LEFT { DROP ; PUSH int 1 ; SWAP ; SUB } { DROP ; PUSH int 1 ; ADD } } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "test"; contract "increment_prefix.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/increment_prefix.jsligo", line 24, characters 13-27:
     23 |   let initial_storage = 42;
     24 |   let orig = Test.originate(contract_of(IncDec), initial_storage, 0tez);
                       ^^^^^^^^^^^^^^
     25 |   Test.transfer_exn(orig.addr, Increment(), 1mutez);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/increment_prefix.jsligo", line 25, characters 2-19:
     24 |   let orig = Test.originate(contract_of(IncDec), initial_storage, 0tez);
     25 |   Test.transfer_exn(orig.addr, Increment(), 1mutez);
            ^^^^^^^^^^^^^^^^^
     26 |   return assert(Test.get_storage(orig.addr) == initial_storage + 1);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/increment_prefix.jsligo", line 26, characters 16-32:
     25 |   Test.transfer_exn(orig.addr, Increment(), 1mutez);
     26 |   return assert(Test.get_storage(orig.addr) == initial_storage + 1);
                          ^^^^^^^^^^^^^^^^
     27 | }) ();
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "broken_unzipped_entries"
    ; "--init-file"
    ; bad_contract "loop.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/loop.jsligo", line 4, character 4 to line 7, character 5:
      3 |     let values = list ([]) ;
      4 |     for (const [k, v, z] of x) {
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |       keys = list([k, ...keys]);
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      6 |       values = list([v, ...values]);
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |     };
          ^^^^^
      8 |     return [keys, values];

    Unsupported pattern in loop. Only single variables or pairs of variables (for maps) are allowed. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "broken_unzipped_entries"
    ; "--init-file"
    ; bad_contract "loop2.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/loop2.jsligo", line 4, character 4 to line 7, character 5:
      3 |     let values : list<int> = list([]);
      4 |     for (const [k, v] of x) {
              ^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |       keys = list([k, ...keys]);
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      6 |       values = list([v, ...values]);
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |     };
          ^^^^^
      8 |     return [keys, values];

    Expected collection of type "any", but recieved collection of type "list (
    ( int *
      int ))". |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "reverse_string_for_loop.jsligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { CDR ;
             PUSH string "" ;
             PUSH int 1 ;
             DUP 3 ;
             SIZE ;
             SUB ;
             PUSH bool True ;
             LOOP { PUSH int 0 ;
                    DUP 2 ;
                    COMPARE ;
                    GE ;
                    DUP ;
                    IF { DUP 2 ;
                         ABS ;
                         DUP 5 ;
                         PUSH nat 1 ;
                         DIG 2 ;
                         SLICE ;
                         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} ;
                         DIG 3 ;
                         CONCAT ;
                         DUG 2 ;
                         PUSH int 1 ;
                         DIG 2 ;
                         SUB ;
                         SWAP }
                       {} } ;
             DIG 2 ;
             DROP 2 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "test"; contract "reverse_string_for_loop.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/reverse_string_for_loop.jsligo", line 19, characters 17-31:
     18 |       let initial_storage = "esrever";
     19 |       let orig = Test.originate(contract_of(C), initial_storage, 0 as tez);
                           ^^^^^^^^^^^^^^
     20 |       Test.transfer_exn(orig.addr, Main(unit), 1 as mutez);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/reverse_string_for_loop.jsligo", line 20, characters 6-23:
     19 |       let orig = Test.originate(contract_of(C), initial_storage, 0 as tez);
     20 |       Test.transfer_exn(orig.addr, Main(unit), 1 as mutez);
                ^^^^^^^^^^^^^^^^^
     21 |       Test.log(Test.get_storage(orig.addr));
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/reverse_string_for_loop.jsligo", line 21, characters 6-14:
     20 |       Test.transfer_exn(orig.addr, Main(unit), 1 as mutez);
     21 |       Test.log(Test.get_storage(orig.addr));
                ^^^^^^^^
     22 |       return assert(Test.get_storage(orig.addr) == "reverse")
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/reverse_string_for_loop.jsligo", line 21, characters 15-31:
     20 |       Test.transfer_exn(orig.addr, Main(unit), 1 as mutez);
     21 |       Test.log(Test.get_storage(orig.addr));
                         ^^^^^^^^^^^^^^^^
     22 |       return assert(Test.get_storage(orig.addr) == "reverse")
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/reverse_string_for_loop.jsligo", line 22, characters 20-36:
     21 |       Test.log(Test.get_storage(orig.addr));
     22 |       return assert(Test.get_storage(orig.addr) == "reverse")
                              ^^^^^^^^^^^^^^^^
     23 |     }
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    "reverse"
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test "duplicate entrypoints" =
  run_ligo_bad [ "compile"; "contract"; bad_contract "duplicate_entrypoints.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/duplicate_entrypoints.mligo", line 3, characters 6-7:
      2 |   [@entry]
      3 |   let b () () : operation list * unit = failwith ()
                ^
      4 |   [@entry]

    Duplicate entry-point b |}]

let%expect_test "dry-run module contract" =
  (* the contract must be accessible even if not exported *)
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "simple_contract_in_module.jsligo"
    ; "1n"
    ; "default_storage"
    ; "-m"
    ; "C"
    ];
  [%expect {|
    ( LIST_EMPTY() , unit ) |}]

let%expect_test "infer compilation target" =
  run_ligo_good [ "compile"; "contract"; contract "single_contract_module_compile.mligo" ];
  [%expect
    {|
      { parameter unit ;
        storage unit ;
        code { DROP ; UNIT ; NIL operation ; PAIR } } |}]
