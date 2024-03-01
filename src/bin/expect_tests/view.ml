open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* view + #import : no view expected *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_import.mligo" ];
  [%expect {| { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

(* view inside module : no view expected *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_inside_module.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

(* view + #import + alias : view expected *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_import_and_alias.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

(* view restrictions on primitives *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "view_restrictions1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/view_restrictions1.mligo", line 1, character 0:
    ../../test/contracts/negative/view_restrictions1.mligo: No such file or directory. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "view_restrictions2.mligo" ];
  [%expect
    {|
    View rule violated:
          - Tezos.create_contract ; Tezos.set_delegate and Tezos.transaction cannot be used because they are stateful (expect in lambdas)
          - Tezos.self can't be used because the entry-point does not make sense in a view |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_restrictions.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "ok_view"
           unit
           (lambda int (pair operation address))
           { DROP ;
             LAMBDA
               int
               (pair operation address)
               { PUSH mutez 0 ;
                 NONE key_hash ;
                 CREATE_CONTRACT
                   { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } ;
                 PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_tuple_storage.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage (pair string nat string nat string) ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v" int mutez { DROP ; PUSH mutez 1000000 } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_shadow_ann.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "views_using_view.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { DROP ; PUSH int 0 ; NIL operation ; PAIR } ;
      view "basic" address int { CDR ; PUSH int 0 ; ADD } ;
      view "not_funny" unit int { PUSH int 0 ; SWAP ; CDR ; DUP 2 ; ADD ; ADD } ;
      view "get_storage" unit int { CDR ; PUSH int 0 ; ADD } ;
      view "get_address" unit address { DROP ; SENDER } ;
      view "super_not_funny"
           unit
           int
           { PUSH int 0 ;
             SWAP ;
             CDR ;
             DUP ;
             DUP 3 ;
             ADD ;
             SWAP ;
             DUP 3 ;
             ADD ;
             DIG 2 ;
             ADD ;
             ADD } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "views_using_view.test.mligo" ];
  [%expect
    {|
    File "../../test/contracts/views_using_view.test.mligo", line 14, characters 42-54:
     13 |     = [], (match p with
     14 |             Basic (v,a)       -> Integer (Option.unopt (Tezos.call_view "basic" a v))
                                                    ^^^^^^^^^^^^
     15 |           | Get_storage v     -> Integer (Option.unopt (Tezos.call_view "get_storage" () v))
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/views_using_view.test.mligo", line 15, characters 42-54:
     14 |             Basic (v,a)       -> Integer (Option.unopt (Tezos.call_view "basic" a v))
     15 |           | Get_storage v     -> Integer (Option.unopt (Tezos.call_view "get_storage" () v))
                                                    ^^^^^^^^^^^^
     16 |           | Not_funny v       -> Integer (Option.unopt (Tezos.call_view "not_funny" () v) )
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/views_using_view.test.mligo", line 16, characters 42-54:
     15 |           | Get_storage v     -> Integer (Option.unopt (Tezos.call_view "get_storage" () v))
     16 |           | Not_funny v       -> Integer (Option.unopt (Tezos.call_view "not_funny" () v) )
                                                    ^^^^^^^^^^^^
     17 |           | Get_address v     -> Address (Option.unopt (Tezos.call_view "get_address" () v))
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/views_using_view.test.mligo", line 17, characters 42-54:
     16 |           | Not_funny v       -> Integer (Option.unopt (Tezos.call_view "not_funny" () v) )
     17 |           | Get_address v     -> Address (Option.unopt (Tezos.call_view "get_address" () v))
                                                    ^^^^^^^^^^^^
     18 |           | Super_not_funny v -> Integer (Option.unopt (Tezos.call_view "super_not_funny" () v)))
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/views_using_view.test.mligo", line 18, characters 42-54:
     17 |           | Get_address v     -> Address (Option.unopt (Tezos.call_view "get_address" () v))
     18 |           | Super_not_funny v -> Integer (Option.unopt (Tezos.call_view "super_not_funny" () v)))
                                                    ^^^^^^^^^^^^
     19 | end
    :
    Warning: deprecated value.
    Use `Option.value_with_error` instead.

    File "../../test/contracts/views_using_view.test.mligo", line 24, characters 22-46:
     23 | let test_basic =
     24 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
                                ^^^^^^^^^^^^^^^^^^^^^^^^
     25 |   let addr = Test.to_address orig1.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 25, characters 13-28:
     24 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
     25 |   let addr = Test.to_address orig1.addr in
                       ^^^^^^^^^^^^^^^
     26 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 26, characters 14-28:
     25 |   let addr = Test.to_address orig1.addr in
     26 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
                        ^^^^^^^^^^^^^^
     27 |   let _ = Test.transfer orig2.addr (Main (Basic (addr, addr))) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 27, characters 10-23:
     26 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
     27 |   let _ = Test.transfer orig2.addr (Main (Basic (addr, addr))) 1tez in
                    ^^^^^^^^^^^^^
     28 |   let s = Test.get_storage orig2.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 28, characters 10-26:
     27 |   let _ = Test.transfer orig2.addr (Main (Basic (addr, addr))) 1tez in
     28 |   let s = Test.get_storage orig2.addr in
                    ^^^^^^^^^^^^^^^^
     29 |   s = (Integer 999)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 32, characters 22-46:
     31 | let test_not_funny =
     32 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
                                ^^^^^^^^^^^^^^^^^^^^^^^^
     33 |   let addr = Test.to_address orig1.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 33, characters 13-28:
     32 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
     33 |   let addr = Test.to_address orig1.addr in
                       ^^^^^^^^^^^^^^^
     34 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 34, characters 14-28:
     33 |   let addr = Test.to_address orig1.addr in
     34 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
                        ^^^^^^^^^^^^^^
     35 |   let _ = Test.transfer orig2.addr (Main (Not_funny addr)) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 35, characters 10-23:
     34 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
     35 |   let _ = Test.transfer orig2.addr (Main (Not_funny addr)) 1tez in
                    ^^^^^^^^^^^^^
     36 |   let s = Test.get_storage orig2.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 36, characters 10-26:
     35 |   let _ = Test.transfer orig2.addr (Main (Not_funny addr)) 1tez in
     36 |   let s = Test.get_storage orig2.addr in
                    ^^^^^^^^^^^^^^^^
     37 |   s = (Integer 999)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 40, characters 22-46:
     39 | let test_get_storage =
     40 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
                                ^^^^^^^^^^^^^^^^^^^^^^^^
     41 |   let addr = Test.to_address orig1.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 41, characters 13-28:
     40 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
     41 |   let addr = Test.to_address orig1.addr in
                       ^^^^^^^^^^^^^^^
     42 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 42, characters 14-28:
     41 |   let addr = Test.to_address orig1.addr in
     42 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
                        ^^^^^^^^^^^^^^
     43 |   let _ = Test.transfer orig2.addr (Main (Get_storage addr)) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 43, characters 10-23:
     42 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
     43 |   let _ = Test.transfer orig2.addr (Main (Get_storage addr)) 1tez in
                    ^^^^^^^^^^^^^
     44 |   let s = Test.get_storage orig2.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 44, characters 10-26:
     43 |   let _ = Test.transfer orig2.addr (Main (Get_storage addr)) 1tez in
     44 |   let s = Test.get_storage orig2.addr in
                    ^^^^^^^^^^^^^^^^
     45 |   s = (Integer 999)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 48, characters 22-46:
     47 | let test_get_address =
     48 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
                                ^^^^^^^^^^^^^^^^^^^^^^^^
     49 |   let addr = Test.to_address orig1.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 49, characters 13-28:
     48 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
     49 |   let addr = Test.to_address orig1.addr in
                       ^^^^^^^^^^^^^^^
     50 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 50, characters 14-28:
     49 |   let addr = Test.to_address orig1.addr in
     50 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
                        ^^^^^^^^^^^^^^
     51 |   let _ = Test.transfer orig2.addr (Main (Get_address addr)) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 51, characters 10-23:
     50 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
     51 |   let _ = Test.transfer orig2.addr (Main (Get_address addr)) 1tez in
                    ^^^^^^^^^^^^^
     52 |   let s = Test.get_storage orig2.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 52, characters 10-26:
     51 |   let _ = Test.transfer orig2.addr (Main (Get_address addr)) 1tez in
     52 |   let s = Test.get_storage orig2.addr in
                    ^^^^^^^^^^^^^^^^
     53 |   s = Address (Test.to_address orig2.addr)
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 53, characters 15-30:
     52 |   let s = Test.get_storage orig2.addr in
     53 |   s = Address (Test.to_address orig2.addr)
                         ^^^^^^^^^^^^^^^
     54 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 56, characters 22-46:
     55 | let test_super_not_funny =
     56 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
                                ^^^^^^^^^^^^^^^^^^^^^^^^
     57 |   let addr = Test.to_address orig1.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 57, characters 13-28:
     56 |   let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
     57 |   let addr = Test.to_address orig1.addr in
                       ^^^^^^^^^^^^^^^
     58 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 58, characters 14-28:
     57 |   let addr = Test.to_address orig1.addr in
     58 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
                        ^^^^^^^^^^^^^^
     59 |   let _ = Test.transfer orig2.addr (Main (Super_not_funny addr)) 1tez in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 59, characters 10-23:
     58 |   let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
     59 |   let _ = Test.transfer orig2.addr (Main (Super_not_funny addr)) 1tez in
                    ^^^^^^^^^^^^^
     60 |   let s = Test.get_storage orig2.addr in
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/views_using_view.test.mligo", line 60, characters 10-26:
     59 |   let _ = Test.transfer orig2.addr (Main (Super_not_funny addr)) 1tez in
     60 |   let s = Test.get_storage orig2.addr in
                    ^^^^^^^^^^^^^^^^
     61 |   s = (Integer (999 + 999))
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test_basic exited with value true.
    - test_not_funny exited with value true.
    - test_get_storage exited with value true.
    - test_get_address exited with value true.
    - test_super_not_funny exited with value true. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "view_exported.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/view_exported.jsligo", line 10, characters 13-27:
      9 | const test = do {
     10 |   let orig = Test.originate(contract_of(C), [], 0tez);
                       ^^^^^^^^^^^^^^
     11 |   Test.log(orig.code);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/view_exported.jsligo", line 11, characters 2-10:
     10 |   let orig = Test.originate(contract_of(C), [], 0tez);
     11 |   Test.log(orig.code);
            ^^^^^^^^
     12 |   let c = Test.to_contract(orig.addr);
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/view_exported.jsligo", line 12, characters 10-26:
     11 |   Test.log(orig.code);
     12 |   let c = Test.to_contract(orig.addr);
                    ^^^^^^^^^^^^^^^^
     13 |
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

    File "../../test/contracts/view_exported.jsligo", line 14, characters 2-10:
     13 |
     14 |   Test.log(Tezos.call_view("bar", unit, Tezos.address(c)) as option<C.storage>);
            ^^^^^^^^
     15 | };
    :
    Warning: deprecated value.
    In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

    { parameter unit ;
      storage unit ;
      code { CDR ; NIL operation ; PAIR } ;
      view "bar" unit unit { CDR } }
    Some (())
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "call_view_tuple.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %a) (nat %b) (mutez %c) (address %d)) ;
      code { CDR ;
             PUSH int 1 ;
             SOME ;
             IF_NONE
               {}
               { DROP ;
                 DUP ;
                 GET 6 ;
                 DUP 2 ;
                 CAR ;
                 SENDER ;
                 PAIR ;
                 VIEW "foo" unit ;
                 DROP ;
                 DUP ;
                 GET 6 ;
                 DUP 2 ;
                 GET 3 ;
                 VIEW "bar" unit ;
                 IF_NONE {} { DROP } } ;
             NIL operation ;
             PAIR } } |}]
