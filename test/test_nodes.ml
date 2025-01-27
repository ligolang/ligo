open Core
open Lltz_ir.Ast_builder.With_dummy
module Ast_builder = Lltz_ir.Ast_builder
module LM = Lltz_codegen
module Row = Lltz_ir.Row 
module Tezos_micheline = Tezos_micheline 

let empty_stack : LM.Stack.t = []

let compile_and_collect_instructions ?(optimize = false)  expr =
  let compiled_instruction = LM.compile_to_micheline ~optimize expr in
  [compiled_instruction empty_stack]

let print_instructions instructions =
  List.iter ~f:(fun instruction ->
    Michelson.Ast.pp Format.std_formatter instruction;
    Format.print_newline ()
  ) instructions;
  Format.print_flush ()

let test_expr ?(optimise = true) expr =
  let instructions = compile_and_collect_instructions expr in
  print_instructions instructions;
  if optimise then
    (Printf.printf "\nOptimised: \n";
    let optimised_instructions = compile_and_collect_instructions ~optimize:true expr in
    print_instructions optimised_instructions)
  else ()


let%expect_test "unit" =
  test_expr (unit);
  [%expect {|
    { UNIT }

    Optimised:
    { UNIT } |}]


(* Bool Tests *)
let%expect_test "bool true" =
  test_expr (bool true);
  [%expect {|
    { PUSH bool True }

    Optimised:
    { PUSH bool True } |}]

let%expect_test "bool false" =
  test_expr (bool false);
  [%expect {|
    { PUSH bool False }

    Optimised:
    { PUSH bool False } |}]

(* Nat Tests *)
let%expect_test "nat zero" =
  test_expr (nat 0);
  [%expect {|
    { PUSH nat 0 }

    Optimised:
    { PUSH nat 0 } |}]

let%expect_test "nat 42" =
  test_expr (nat 42);
  [%expect {|
    { PUSH nat 42 }

    Optimised:
    { PUSH nat 42 } |}]

let%expect_test "nat large" =
  test_expr (nat 999999999999999999);
  [%expect {|
    { PUSH nat 999999999999999999 }

    Optimised:
    { PUSH nat 999999999999999999 } |}]

(* Int Tests *)
let%expect_test "int zero" =
  test_expr (int 0);
  [%expect {|
    { PUSH int 0 }

    Optimised:
    { PUSH int 0 } |}]

let%expect_test "int positive" =
  test_expr (int 123456);
  [%expect {|
    { PUSH int 123456 }

    Optimised:
    { PUSH int 123456 } |}]

let%expect_test "int negative" =
  test_expr (int (-999999));
  [%expect {|
    { PUSH int -999999 }

    Optimised:
    { PUSH int -999999 } |}]

(* Mutez Tests *)
let%expect_test "mutez zero" =
  test_expr (mutez 0);
  [%expect {|
    { PUSH mutez 0 }

    Optimised:
    { PUSH mutez 0 } |}]

let%expect_test "mutez small" =
  test_expr (mutez 123);
  [%expect {|
    { PUSH mutez 123 }

    Optimised:
    { PUSH mutez 123 } |}]

let%expect_test "mutez large" = 
  test_expr (mutez 10000000);
  [%expect {|
    { PUSH mutez 10000000 }

    Optimised:
    { PUSH mutez 10000000 } |}]

(* String Tests *)
let%expect_test "string empty" =
  test_expr (string "");
  [%expect {|
    { PUSH string "" }

    Optimised:
    { PUSH string "" } |}]

let%expect_test "string ascii" =
  test_expr (string "hello");
  [%expect {|
    { PUSH string "hello" }

    Optimised:
    { PUSH string "hello" } |}]

let%expect_test "string unicode" =
  test_expr (string "こんにちは");
  [%expect {|
    { PUSH string "こんにちは" }

    Optimised:
    { PUSH string "こんにちは" } |}]

(* Key Tests *)
let%expect_test "key example1" =
  test_expr (key "edpkExampleKeyString");
  [%expect {|
    { PUSH key "edpkExampleKeyString" }

    Optimised:
    { PUSH key "edpkExampleKeyString" } |}]

(* Key_hash Tests *)
let%expect_test "key_hash tz1" =
  test_expr (key_hash "tz1abc123abc123abc123");
  [%expect {|
    { PUSH key_hash "tz1abc123abc123abc123" }

    Optimised:
    { PUSH key_hash "tz1abc123abc123abc123" } |}]

let%expect_test "key_hash tz2" =
  test_expr (key_hash "tz2xyz456xyz456xyz456");
  [%expect {|
    { PUSH key_hash "tz2xyz456xyz456xyz456" }

    Optimised:
    { PUSH key_hash "tz2xyz456xyz456xyz456" } |}]

(* Bytes Tests *)
let%expect_test "bytes empty" =
  test_expr (bytes "0x");
  [%expect {|
    { PUSH bytes 0x3078 }

    Optimised:
    { PUSH bytes 0x3078 } |}]

let%expect_test "bytes small" =
  test_expr (bytes "0xFF");
  [%expect {|
    { PUSH bytes 0x30784646 }

    Optimised:
    { PUSH bytes 0x30784646 } |}]

let%expect_test "bytes bigger" =
  test_expr (bytes "0xDEADBEEF");
  [%expect {|
    { PUSH bytes 0x30784445414442454546 }

    Optimised:
    { PUSH bytes 0x30784445414442454546 } |}]

(* Chain_id Tests *)
let%expect_test "chain_id mainnet" =
  test_expr (chain_id "NetXdQprcVkpaWU");
  [%expect {|
    { PUSH chain_id "NetXdQprcVkpaWU" }

    Optimised:
    { PUSH chain_id "NetXdQprcVkpaWU" } |}]

let%expect_test "chain_id ghostnet" =
  test_expr (chain_id "NetXnHfVqm9iesp");
  [%expect {|
    { PUSH chain_id "NetXnHfVqm9iesp" }

    Optimised:
    { PUSH chain_id "NetXnHfVqm9iesp" } |}]

(* Address Tests *)
let%expect_test "address kt1 contract" =
  test_expr (address_const "KT1ExampleContractAddress");
  [%expect {|
    { PUSH address "KT1ExampleContractAddress" }

    Optimised:
    { PUSH address "KT1ExampleContractAddress" } |}]

let%expect_test "address tz1 implicit" =
  test_expr (address_const "tz1ExampleImplicitAddress");
  [%expect {|
    { PUSH address "tz1ExampleImplicitAddress" }

    Optimised:
    { PUSH address "tz1ExampleImplicitAddress" } |}]

(* Timestamp Tests *)
let%expect_test "timestamp epoch" =
  test_expr (timestamp "1970-01-01T00:00:00Z");
  [%expect {|
    { PUSH timestamp 0 }

    Optimised:
    { PUSH timestamp 0 } |}]

(* Large future date, or something well beyond typical usage. *)
let%expect_test "timestamp far-future" =
  test_expr (timestamp "9999-12-31T23:59:59Z");
  [%expect {|
    { PUSH timestamp 253402300799 }

    Optimised:
    { PUSH timestamp 253402300799 } |}]

(* BLS12-381 G1, G2, FR Tests *)
let%expect_test "bls12_381_g1 short" =
  test_expr (bls12_381_g1 "BLS12_381_G1ShortExample");
  [%expect {|
    { PUSH bls12_381_g1 0x424c5331325f3338315f473153686f72744578616d706c65 }

    Optimised:
    { PUSH bls12_381_g1 0x424c5331325f3338315f473153686f72744578616d706c65 } |}]

let%expect_test "bls12_381_g2 short" =
  test_expr (bls12_381_g2 "BLS12_381_G2ShortExample");
  [%expect {|
    { PUSH bls12_381_g2 0x424c5331325f3338315f473253686f72744578616d706c65 }

    Optimised:
    { PUSH bls12_381_g2 0x424c5331325f3338315f473253686f72744578616d706c65 } |}]

let%expect_test "bls12_381_fr short" =
  test_expr (bls12_381_fr "BLS12_381_FRShortExample");
  [%expect {|
    { PUSH bls12_381_fr 0x424c5331325f3338315f465253686f72744578616d706c65 }

    Optimised:
    { PUSH bls12_381_fr 0x424c5331325f3338315f465253686f72744578616d706c65 } |}]

(* Signature Tests *)
let%expect_test "signature short" =
  test_expr (signature "edsigShortExampleSignature");
  [%expect {|
    { PUSH signature "edsigShortExampleSignature" }

    Optimised:
    { PUSH signature "edsigShortExampleSignature" } |}]

let%expect_test "signature typical" =
  test_expr (signature "edsigthT3ab6iK1ZPw6u9JN7nUeadfZtQ1Qvo8CeCD3ScH7GpdiQA69rVbX7fNCRbs6Sr9aBii1qmuXun5qRmEuA9N2Dg9LGQdN");
  [%expect {|
    { PUSH signature
           "edsigthT3ab6iK1ZPw6u9JN7nUeadfZtQ1Qvo8CeCD3ScH7GpdiQA69rVbX7fNCRbs6Sr9aBii1qmuXun5qRmEuA9N2Dg9LGQdN" }

    Optimised:
    { PUSH signature
           "edsigthT3ab6iK1ZPw6u9JN7nUeadfZtQ1Qvo8CeCD3ScH7GpdiQA69rVbX7fNCRbs6Sr9aBii1qmuXun5qRmEuA9N2Dg9LGQdN" } |}]


let%expect_test "variable x" =
  test_expr (let_in (var "x") ~rhs:(nat 1) ~in_:(variable (var "x") (nat_ty)));
  [%expect {|
    { PUSH nat 1 }

    Optimised:
    { PUSH nat 1 } |}]


let%expect_test "variable usage: let_in (bind + reference)" =
let expr =
  let_in (var "x") ~rhs:(nat 42) ~in_:(variable (var "x") (nat_ty))
in
test_expr expr;
[%expect {|
  { PUSH nat 42 }

  Optimised:
  { PUSH nat 42 } |}]

let%expect_test "nested let_in (two bindings, referencing each other)" =
  (* let x = 42 in
       let y = x + 1 in
         y
  *)
  let expr =
    let_in (var "x") ~rhs:(nat 42)
      ~in_:(
        let_in (var "y")
          ~rhs:(add (variable (var "x") nat_ty) (nat 1))
          ~in_:(variable (var "y") nat_ty)
      )
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 42 ; PUSH nat 1 ; SWAP ; ADD }

    Optimised:
    { PUSH nat 42 ; PUSH nat 1 ; ADD } |}]
  
let%expect_test "let_in variable shadowing" =
  let expr =
    let_in (var "x") ~rhs:(nat 10)
      ~in_:(
        let_in (var "x")
          ~rhs:(add (variable (var "x") nat_ty) (nat 5))
          ~in_:(variable (var "x") nat_ty)
      )
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ; PUSH nat 5 ; DUP 2 ; ADD ; SWAP ; DROP }

    Optimised:
    { PUSH nat 10 ; PUSH nat 5 ; ADD } |}]
  
let%expect_test "let_mut_in usage (assign, reassign, read)" =
  let expr =
    let_mut_in (mut_var "x") ~rhs:(nat 0)
      ~in_:
        (let_in (var "unused_after_assign")
           ~rhs:(assign (mut_var "x") (nat 1))
           ~in_:(deref (mut_var "x") nat_ty))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ; PUSH nat 1 ; DUG 1 ; DIG 0 ; DROP ; UNIT ; DROP }

    Optimised:
    { PUSH nat 1 } |}]

let%expect_test "multiple sequential assign" =
  let mut_x = mut_var "x" in
  let expr =
    let_mut_in mut_x ~rhs:(nat 10)
      ~in_:
        (let_in (var "ignore1")
          ~rhs:(
            assign mut_x (add (deref mut_x nat_ty) (nat 1))
          )
          ~in_:
            (let_in (var "ignore2")
              ~rhs:(
                assign mut_x (add (deref mut_x nat_ty) (nat 2))
              )
              ~in_:
                (deref mut_x nat_ty)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      PUSH nat 1 ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP ;
      PUSH nat 2 ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP }

    Optimised:
    { PUSH nat 10 ; PUSH nat 1 ; ADD ; PUSH nat 2 ; ADD } |}]

let%expect_test "assign inside nested if_bool" =
  let mut_x = mut_var "x" in
  let nested_if =
    if_bool (lt (nat 3) (nat 4))
      ~then_:(assign mut_x (add (deref mut_x nat_ty) (nat 200)))
      ~else_:(assign mut_x (add (deref mut_x nat_ty) (nat 300)))
  in
  let main_if =
    if_bool (eq (nat 1) (nat 2))
      ~then_:(assign mut_x (add (deref mut_x nat_ty) (nat 100)))
      ~else_:nested_if
  in
  let expr =
    let_mut_in mut_x ~rhs:(nat 0)
      ~in_:
        (let_in (var "ignored")
          ~rhs:main_if
          ~in_:(deref mut_x nat_ty))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ;
      PUSH nat 2 ;
      PUSH nat 1 ;
      COMPARE ;
      EQ ;
      IF { PUSH nat 100 ; DUP 2 ; ADD ; DUG 1 ; DIG 0 ; DROP ; UNIT }
         { PUSH nat 4 ;
           PUSH nat 3 ;
           COMPARE ;
           LT ;
           IF { PUSH nat 200 ; DUP 2 ; ADD ; DUG 1 ; DIG 0 ; DROP ; UNIT }
              { PUSH nat 300 ; DUP 2 ; ADD ; DUG 1 ; DIG 0 ; DROP ; UNIT } } ;
      DROP }

    Optimised:
    { PUSH nat 0 ; PUSH nat 200 ; ADD } |}]

let%expect_test "assign same value repeatedly" =
  let mut_x = mut_var "x" in
  let expr =
    let_mut_in mut_x ~rhs:(nat 5)
      ~in_:
        (let_in (var "ignore1")
          ~rhs:(assign mut_x (nat 5))
          ~in_:
            (let_in (var "ignore2")
              ~rhs:(assign mut_x (nat 5))
              ~in_:(deref mut_x nat_ty)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 5 ;
      PUSH nat 5 ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP ;
      PUSH nat 5 ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP }

    Optimised:
    { PUSH nat 5 } |}]

let%expect_test "assign referencing the updated var" =
  let mut_x = mut_var "x" in
  let first_assign =
    assign mut_x (add (deref mut_x nat_ty) (nat 10))
  in
  let second_assign =
    assign mut_x
      (add (deref mut_x nat_ty) (deref mut_x nat_ty))
  in
  let expr =
    let_mut_in mut_x ~rhs:(nat 10)
      ~in_:
        (let_in (var "ignore1") ~rhs:first_assign
          ~in_:
            (let_in (var "ignore2") ~rhs:second_assign
              ~in_:(deref mut_x nat_ty)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      PUSH nat 10 ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP ;
      DUP 1 ;
      DUP 2 ;
      ADD ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP }

    Optimised:
    { PUSH nat 10 ; DUP ; ADD ; DUP ; ADD } |}]

let%expect_test "assign inside while" =
  let mut_x = mut_var "x" in
  let body_expr =
    assign mut_x
      (add (add (deref mut_x nat_ty) (deref mut_x nat_ty)) (nat 1))
  in
  let while_expr =
    while_ (lt (deref mut_x nat_ty) (nat 3)) ~body:body_expr
  in
  let expr =
    let_mut_in mut_x ~rhs:(nat 0)
      ~in_:
        (let_in (var "ignore_while") ~rhs:while_expr
          ~in_:(deref mut_x nat_ty))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ;
      PUSH nat 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH nat 1 ;
             DUP 2 ;
             DUP 3 ;
             ADD ;
             ADD ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH nat 3 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      UNIT ;
      DROP }

    Optimised:
    { PUSH nat 0 ;
      PUSH nat 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH nat 1 ; SWAP ; DUP ; ADD ; ADD ; PUSH nat 3 ; DUP 2 ; COMPARE ; LT } } |}]

let%expect_test "assign same var multiple branches" =
  let mut_x = mut_var "x" in
  let condition = eq (deref mut_x nat_ty) (nat 100) in
  let then_branch =
    assign mut_x (sub (deref mut_x nat_ty) (nat 50))
  in
  let else_branch =
    assign mut_x (sub (deref mut_x nat_ty) (nat 10))
  in
  let expr =
    let_mut_in mut_x ~rhs:(nat 100)
      ~in_:
        (if_bool condition
          ~then_:then_branch
          ~else_:else_branch)
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 100 ;
      PUSH nat 100 ;
      DUP 2 ;
      COMPARE ;
      EQ ;
      IF { DROP ; PUSH nat 50 ; NEVER } { DROP ; PUSH nat 10 ; NEVER } }

    Optimised:
    { PUSH bool True ;
      IF { PUSH nat 50 ; NEVER } { PUSH nat 10 ; NEVER } } |}]

let%expect_test "assign inside for loop" =
  let idx = mut_var "i" in
  let mut_x = mut_var "x" in
  let for_body_expr =
    assign mut_x (add (deref mut_x nat_ty) (deref idx nat_ty))
  in
  let cond_expr = lt (deref idx nat_ty) (nat 4) in
  let update_expr =
    assign (mut_var "i") (add (deref (mut_var "i") nat_ty) (nat 1))
  in
  let for_expr =
    for_ idx
      ~init:(nat 0)
      ~cond:cond_expr
      ~update:update_expr
      ~body:for_body_expr
  in
  let expr =
    let_mut_in mut_x ~rhs:(nat 0)
      ~in_:(
        let_mut_in (mut_var "i") ~rhs:(nat 0)
          ~in_:(
            let_in (var "ignore_for") ~rhs:for_expr
              ~in_:(deref mut_x nat_ty)
          )
      )
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ;
      PUSH nat 0 ;
      PUSH nat 0 ;
      PUSH nat 4 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { DUP 1 ;
             DUP 4 ;
             ADD ;
             DUG 3 ;
             DIG 2 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH nat 1 ;
             DUP 2 ;
             ADD ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH nat 4 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP ;
      UNIT ;
      DROP ;
      SWAP ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH nat 0 ;
      DUP ;
      DUP ;
      PUSH nat 4 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { DUP ;
             DIG 3 ;
             ADD ;
             DUG 2 ;
             PUSH nat 1 ;
             ADD ;
             PUSH nat 4 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP 2 } |}]

let%expect_test "assign same var different type" =
  (* Not valid when type-checked. *)
  let mut_x = mut_var "x" in
  let expr =
    let_mut_in mut_x ~rhs:(int 123)
      ~in_:
        (assign mut_x (string "changed"))
  in
  test_expr expr;
  [%expect {|
    { PUSH int 123 ; DROP ; PUSH string "changed" ; DROP ; UNIT }

    Optimised:
    { UNIT } |}]

let%expect_test "let_in + if_bool" =
  let expr =
    let_in (var "b") ~rhs:(bool true)
      ~in_:(if_bool (variable (var "b") bool_ty)
               ~then_:(nat 10)
               ~else_:(nat 20))
  in
  test_expr expr;
  [%expect {|
    { PUSH bool True ; IF { PUSH nat 10 } { PUSH nat 20 } }

    Optimised:
    { PUSH nat 10 } |}]
  
let%expect_test "let_in referencing previous var inside if_bool" =
  let expr =
    let_in (var "x") ~rhs:(nat 1)
      ~in_:
        (let_in (var "y") ~rhs:(nat 2)
          ~in_:
            (if_bool (bool true)
              ~then_:(variable (var "x") nat_ty)
              ~else_:(variable (var "y") nat_ty)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 1 ;
      PUSH nat 2 ;
      PUSH bool True ;
      IF { DROP } { SWAP ; DROP } }

    Optimised:
    { PUSH nat 1 } |}]

let%expect_test "nested let_in" =
  let expr =
    let_in (var "x") ~rhs:(nat 10)
      ~in_:(
        let_in (var "y") ~rhs:(int (-5))
          ~in_:
            (app
              (lambda (var "z", nat_ty)
                ~body:(mul (variable (var "z") nat_ty) (nat 2)))
              (add (variable (var "y") int_ty) (variable (var "x") nat_ty)))
      )
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      PUSH int -5 ;
      SWAP ;
      SWAP ;
      ADD ;
      LAMBDA nat nat { PUSH nat 2 ; SWAP ; MUL } ;
      SWAP ;
      EXEC }

    Optimised:
    { PUSH nat 10 ; PUSH int -5 ; ADD ; PUSH nat 2 ; SWAP ; MUL } |}]
  
let%expect_test "lambda that uses let_in" =
  let expr =
    lambda (var "x", nat_ty)
      ~body:(
        let_in (var "z") ~rhs:(nat 5)
          ~in_:(add (variable (var "x") nat_ty) (variable (var "z") nat_ty))
      )
  in
  test_expr expr;
  [%expect {|
    { LAMBDA nat nat { PUSH nat 5 ; SWAP ; ADD } }

    Optimised:
    { LAMBDA nat nat { PUSH nat 5 ; ADD } } |}]

let%expect_test "recursive lambda usage" =
let lam_rec_expr =
  lambda_rec (var "f")
    (var "x", nat_ty)
    ~body:(
      if_bool
        (le (variable (var "x") nat_ty) (nat 0))
        ~then_:(nat 1)
        ~else_:(app
                  (variable (var "f") (function_ty nat_ty nat_ty))
                  (sub (variable (var "x") nat_ty) (nat 1)))
    )
in
test_expr lam_rec_expr;
[%expect {|
  { LAMBDA_REC
      nat
      nat
      { SWAP ;
        SWAP ;
        PUSH nat 0 ;
        DUP 2 ;
        COMPARE ;
        LE ;
        IF { SWAP ; DROP ; DROP ; PUSH nat 1 }
           { PUSH nat 1 ; SWAP ; SUB ; SWAP ; SWAP ; EXEC } } }

  Optimised:
  { LAMBDA_REC
      nat
      nat
      { DUP ;
        INT ;
        LE ;
        IF { DROP 2 ; PUSH nat 1 } { PUSH int -1 ; ADD ; EXEC } } } |}]
  
let%expect_test "let_mut_in + assign in if_bool" =
  let expr =
    let_mut_in (mut_var "x") ~rhs:(nat 100)
      ~in_:
        (if_bool (bool false)
          ~then_:(assign (mut_var "x") (nat 200))
          ~else_:(assign (mut_var "x") (nat 300)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 100 ;
      DROP ;
      PUSH bool False ;
      IF { PUSH nat 200 ; DROP ; UNIT } { PUSH nat 300 ; DROP ; UNIT } }

    Optimised:
    { UNIT } |}]

let%expect_test "lambda returning same type: bool -> bool" =
  let lam_expr =
    lambda (var "x", bool_ty)
      ~body:(variable (var "x") bool_ty)
  in
  test_expr lam_expr;
  [%expect {|
    { LAMBDA bool bool {} }

    Optimised:
    { LAMBDA bool bool {} } |}]

let%expect_test "lambda returning different type: string -> bool" =
  let lam_expr =
    lambda (var "x", string_ty)
      ~body:(
        if_bool
          (eq (variable (var "x") string_ty) (string "secret"))
          ~then_:(bool true)
          ~else_:(bool false)
      )
  in
  test_expr lam_expr;
  [%expect {|
    { LAMBDA
        string
        bool
        { PUSH string "secret" ;
          SWAP ;
          COMPARE ;
          EQ ;
          IF { PUSH bool True } { PUSH bool False } } }

    Optimised:
    { LAMBDA string bool { PUSH string "secret" ; COMPARE ; EQ } } |}]

let%expect_test "lambda returning tuple" =
  let lam_expr =
    lambda (var "x", address_ty)
      ~body:(
        tuple
          (mk_row [
             Leaf (None, now);
             Leaf (None, variable (var "x") address_ty)
           ])
      )
  in
  test_expr lam_expr;
  [%expect {|
    { LAMBDA address (pair timestamp address) { NOW ; PAIR } }

    Optimised:
    { LAMBDA address (pair timestamp address) { NOW ; PAIR } } |}]

let%expect_test "lambda_rec factorial" =
  let n_var = var "n" in
  let n_var_ty = int_ty in
  let f_var = var "f" in

  let body_expr =
    if_bool
      (le (variable n_var n_var_ty) (int 1))
      ~then_:(int 1)
      ~else_:(
        mul
          (variable n_var n_var_ty)
          (app (variable f_var (function_ty int_ty int_ty))
               (sub (variable n_var n_var_ty) (int 1)))
      )
  in

  let lam_rec_expr =
    lambda_rec f_var (n_var, n_var_ty) ~body:body_expr
  in
  test_expr lam_rec_expr;
  [%expect {|
    { LAMBDA_REC
        int
        int
        { SWAP ;
          SWAP ;
          PUSH int 1 ;
          DUP 2 ;
          COMPARE ;
          LE ;
          IF { SWAP ; DROP ; DROP ; PUSH int 1 }
             { PUSH int 1 ; DUP 2 ; SUB ; DIG 2 ; SWAP ; EXEC ; SWAP ; MUL } } }

    Optimised:
    { LAMBDA_REC
        int
        int
        { PUSH int 1 ;
          DUP 2 ;
          COMPARE ;
          LE ;
          IF { DROP 2 ; PUSH int 1 }
             { PUSH int 1 ; DUP 2 ; SUB ; DIG 2 ; SWAP ; EXEC ; SWAP ; MUL } } } |}]

let%expect_test "nested lambda" =
  let inner =
    lambda (var "y", nat_ty)
      ~body:(add (variable (var "y") nat_ty) (nat 2))
  in
  let outer =
    lambda (var "x", nat_ty)
      ~body:inner
  in
  test_expr outer;
  [%expect {|
    { LAMBDA
        nat
        (lambda nat nat)
        { LAMBDA nat nat { PUSH nat 2 ; SWAP ; ADD } ; SWAP ; DROP } }

    Optimised:
    { LAMBDA nat (lambda nat nat) { DROP ; LAMBDA nat nat { PUSH nat 2 ; ADD } } } |}]

let%expect_test "lambda in let" =
  let lambda_expr =
    lambda (var "y", nat_ty)
      ~body:(mul (nat 3) (variable (var "y") nat_ty))
  in
  let expr =
    let_in (var "x") ~rhs:lambda_expr
      ~in_:(app (variable (var "x") (function_ty nat_ty nat_ty)) (nat 5))
  in
  test_expr expr;
  [%expect {|
    { LAMBDA nat nat { PUSH nat 3 ; MUL } ; PUSH nat 5 ; SWAP ; SWAP ; EXEC }

    Optimised:
    { LAMBDA nat nat { PUSH nat 3 ; MUL } ; PUSH nat 5 ; EXEC } |}]

let%expect_test "if with lambda" =
  let lambda_then =
    lambda (var "x", nat_ty)
      ~body:(add (variable (var "x") nat_ty) (nat 2))
  in
  let lambda_else =
    lambda (var "y", nat_ty)
      ~body:(sub (variable (var "y") nat_ty) (nat 3))
  in
  let expr =
    if_bool
      (neq (nat 1) (nat 2))
      ~then_:lambda_then
      ~else_:lambda_else
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 2 ;
      PUSH nat 1 ;
      COMPARE ;
      NEQ ;
      IF { LAMBDA nat nat { PUSH nat 2 ; SWAP ; ADD } }
         { LAMBDA nat int { PUSH nat 3 ; SWAP ; SUB } } }

    Optimised:
    { PUSH int -1 ;
      NEQ ;
      IF { LAMBDA nat nat { PUSH nat 2 ; ADD } }
         { LAMBDA nat int { PUSH int -3 ; ADD } } } |}]

let%expect_test "app: lambda (string->bool) applied to a string" =
  let fun_expr =
    lambda (var "x", string_ty)
      ~body:(eq (variable (var "x") string_ty) (string "hello"))
  in
  let app_expr =
    app fun_expr (string "test")
  in
  test_expr app_expr;
  [%expect {|
    { PUSH string "test" ;
      LAMBDA string bool { PUSH string "hello" ; SWAP ; COMPARE ; EQ } ;
      SWAP ;
      EXEC }

    Optimised:
    { PUSH string "test" ; PUSH string "hello" ; COMPARE ; EQ } |}]

let%expect_test "let_mut_in: assign string" =
  let expr =
    let_mut_in (mut_var "s") ~rhs:(string "hello")
      ~in_:
        (let_in (var "ignore")
          ~rhs:(assign (mut_var "s") (string "world"))
          ~in_:(deref (mut_var "s") string_ty))
  in
  test_expr expr;
  [%expect {|
    { PUSH string "hello" ;
      PUSH string "world" ;
      DUG 1 ;
      DIG 0 ;
      DROP ;
      UNIT ;
      DROP }

    Optimised:
    { PUSH string "world" } |}]

let%expect_test "deref inside lambda body" =
  let mut_counter = mut_var "counter" in
  let expr =
    let_mut_in mut_counter ~rhs:(nat 0)
      ~in_:
        (lambda (var "u", unit_ty)
          ~body:(deref mut_counter nat_ty))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ;
      LAMBDA (pair nat unit) nat { UNPAIR ; SWAP ; DROP } ;
      SWAP ;
      APPLY }

    Optimised:
    { LAMBDA (pair nat unit) nat { CAR } ; PUSH nat 0 ; APPLY } |}]

let%expect_test "assign inside if_bool" =
  let mut_x = mut_var "x" in
  let expr =
    let_mut_in mut_x ~rhs:(nat 42)
      ~in_:
        (if_bool (bool true)
          ~then_:(assign mut_x (nat 100))
          ~else_:(assign mut_x (nat 200)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 42 ;
      DROP ;
      PUSH bool True ;
      IF { PUSH nat 100 ; DROP ; UNIT } { PUSH nat 200 ; DROP ; UNIT } }

    Optimised:
    { UNIT } |}]

let%expect_test "app of lambda_rec factorial with let_in" =
  let n_var = var "n" in
  let n_var_ty = int_ty in
  let f_var = var "f" in
  let body_expr =
    if_bool
      (le (variable n_var n_var_ty) (int 1))
      ~then_:(int 1)
      ~else_:
        (mul
          (variable n_var n_var_ty)
          (app (variable f_var (function_ty int_ty int_ty))
               (sub (variable n_var n_var_ty) (int 1))))
  in
  let factorial_rec =
    lambda_rec f_var (n_var, n_var_ty) ~body:body_expr
  in
  let call_factorial_5 =
    app factorial_rec (int 5)
  in
  test_expr call_factorial_5;
  [%expect {|
    { PUSH int 5 ;
      LAMBDA_REC
        int
        int
        { SWAP ;
          SWAP ;
          PUSH int 1 ;
          DUP 2 ;
          COMPARE ;
          LE ;
          IF { SWAP ; DROP ; DROP ; PUSH int 1 }
             { PUSH int 1 ; DUP 2 ; SUB ; DIG 2 ; SWAP ; EXEC ; SWAP ; MUL } } ;
      SWAP ;
      EXEC }

    Optimised:
    { LAMBDA_REC
        int
        int
        { PUSH int 1 ;
          DUP 2 ;
          COMPARE ;
          LE ;
          IF { DROP 2 ; PUSH int 1 }
             { PUSH int 1 ; DUP 2 ; SUB ; DIG 2 ; SWAP ; EXEC ; SWAP ; MUL } } ;
      PUSH int 5 ;
      EXEC } |}]


let%expect_test "if bool 1" =
  let e = if_bool (bool true) ~then_:(string "A") ~else_:(string "B") in
  test_expr e;
  [%expect {|
    { PUSH bool True ; IF { PUSH string "A" } { PUSH string "B" } }

    Optimised:
    { PUSH string "A" } |}]

let%expect_test "if bool 2" =
  let e = if_bool (eq (nat 2) (nat 2)) ~then_:(int 100) ~else_:(int (-1)) in
  test_expr e;
  [%expect {|
    { PUSH nat 2 ;
      PUSH nat 2 ;
      COMPARE ;
      EQ ;
      IF { PUSH int 100 } { PUSH int -1 } }

    Optimised:
    { PUSH int 100 } |}]

let%expect_test "nested if_bool" =
  let expr =
    if_bool
      (eq (nat 1) (nat 2))
      ~then_:
        (if_bool
           (le (nat 1) (nat 10))
           ~then_:(nat 1)
           ~else_:(nat 2))
      ~else_:(nat 3)
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 2 ;
      PUSH nat 1 ;
      COMPARE ;
      EQ ;
      IF { PUSH nat 10 ;
           PUSH nat 1 ;
           COMPARE ;
           LE ;
           IF { PUSH nat 1 } { PUSH nat 2 } }
         { PUSH nat 3 } }

    Optimised:
    { PUSH nat 3 } |}]

let%expect_test "if none 1" =
  let some_val = some (nat 10) in
  let e = if_none some_val ~none:(string "none") ~some:
    {lam_var = (var "val", nat_ty); body = (add (variable (var "val") nat_ty) (nat 5))} in
  test_expr e;
  [%expect {|
    { PUSH nat 10 ;
      SOME ;
      IF_NONE { PUSH string "none" } { PUSH nat 5 ; SWAP ; ADD } }

    Optimised:
    { PUSH nat 10 ;
      SOME ;
      IF_NONE { PUSH string "none" } { PUSH nat 5 ; ADD } } |}]

let%expect_test "if none 2" =
  let none_val = none (bool_ty) in
  let e = if_none none_val 
    ~none:(bool true) 
    ~some:{lam_var =(var "val", bool_ty); body = (not (variable (var "val") bool_ty))} in
  test_expr e;
  [%expect {|
    { NONE bool ; IF_NONE { PUSH bool True } { NOT } }

    Optimised:
    { NONE bool ; IF_NONE { PUSH bool True } { NOT } } |}]

let%expect_test "if cons 1" =
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let e = if_cons lst ~empty:(nat 0)
    ~nonempty:{
      lam_var1 = (var "hd", nat_ty);
      lam_var2 = (var "tl", list_ty nat_ty);
      body = string "nonempty"
    }
  in
  test_expr e;
  [%expect {|
    { NIL nat ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      IF_CONS { DROP ; DROP ; PUSH string "nonempty" } { PUSH nat 0 } }

    Optimised:
    { PUSH (list nat) { 1 ; 2 } ;
      IF_CONS { DROP 2 ; PUSH string "nonempty" } { PUSH nat 0 } } |}]

let%expect_test "if cons 2" =
  let empty_list = nil string_ty in
  let e = if_cons empty_list ~empty:(string "empty")
    ~nonempty:{
      lam_var1 = (var "hd", string_ty);
      lam_var2 = (var "tl", list_ty string_ty);
      body = string "nonempty"
    }
  in
  test_expr e;
  [%expect {|
    { NIL string ;
      IF_CONS { DROP ; DROP ; PUSH string "nonempty" } { PUSH string "empty" } }

    Optimised:
    { NIL string ;
      IF_CONS { DROP 2 ; PUSH string "nonempty" } { PUSH string "empty" } } |}]

let%expect_test "if left" =
  let left_expr = left (None, None, bool_ty) (bool false) in
  let e = if_left left_expr
    ~left:{ lam_var = (var "l", bool_ty); body = string "left" }
    ~right:{ lam_var = (var "r", bool_ty); body = string "right" }
  in
  test_expr e;
  [%expect {|
    { PUSH bool False ;
      LEFT bool ;
      IF_LEFT { DROP ; PUSH string "left" } { DROP ; PUSH string "right" } }

    Optimised:
    { PUSH bool False ;
      LEFT bool ;
      IF_LEFT { DROP ; PUSH string "left" } { DROP ; PUSH string "right" } } |}]

let%expect_test "if left 2" =
  let right_expr = right (None, None, bool_ty) (bool true) in
  let e = if_left right_expr
    ~left:{ lam_var = (var "l", bool_ty); body = int 0 }
    ~right:{ lam_var = (var "r", bool_ty); body = int 1 }
  in
  test_expr e;
  [%expect {|
    { PUSH bool True ;
      RIGHT bool ;
      IF_LEFT { DROP ; PUSH int 0 } { DROP ; PUSH int 1 } }

    Optimised:
    { PUSH bool True ;
      RIGHT bool ;
      IF_LEFT { DROP ; PUSH int 0 } { DROP ; PUSH int 1 } } |}]

let%expect_test "while" =
  let e = while_ (lt (variable (var "i") int_ty) (int 3))
      ~body:(assign (mut_var "i") (add (deref (mut_var "i") int_ty) (int 1))) in
  test_expr (let_mut_in (mut_var "i") ~rhs:(int 0) ~in_:e);
  [%expect {|
    { PUSH int 0 ;
      PUSH int 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH int 1 ;
             DUP 2 ;
             ADD ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH int 3 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      UNIT ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH int 0 ;
      PUSH int 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH int 1 ; ADD ; PUSH int 3 ; DUP 2 ; COMPARE ; LT } ;
      DROP ;
      UNIT } |}]

let%expect_test "while with complex body" =
  let body_expr =
    let_in (var "x") ~rhs:(nat 7)
      ~in_:
        (if_bool
          (ge (variable (var "x") nat_ty) (nat 5))
          ~then_:(assign (mut_var "x") (nat 2))
          ~else_:(assign (mut_var "x") (nat 2)))
  in
  let expr =
    while_ (le (nat 0) (nat 10)) ~body:body_expr
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      PUSH nat 0 ;
      COMPARE ;
      LE ;
      LOOP { PUSH nat 7 ;
             PUSH nat 5 ;
             DUP 2 ;
             COMPARE ;
             GE ;
             IF { DROP ; PUSH nat 2 ; DROP ; UNIT }
                { DROP ; PUSH nat 2 ; DROP ; UNIT } ;
             DROP ;
             PUSH nat 10 ;
             PUSH nat 0 ;
             COMPARE ;
             LE } ;
      UNIT }

    Optimised:
    { PUSH int -1 ; LE ; LOOP { PUSH int -1 ; LE } ; UNIT } |}]

let%expect_test "while left" =
  let start_val = left (None, None, bool_ty) (bool false) in
  let e = while_left start_val ~body:{
    lam_var = (var "v", bool_ty);
    body = if_bool (variable (var "v") bool_ty)
      ~then_:(right (None, None, string_ty) (string "done"))
      ~else_:(left (None, None, bool_ty) (bool true))
  } in
  test_expr e;
  [%expect {|
    { PUSH bool False ;
      LEFT bool ;
      LEFT string ;
      LOOP_LEFT
        { IF { PUSH string "done" ; RIGHT string } { PUSH bool True ; LEFT bool } } }

    Optimised:
    { PUSH bool False ;
      LEFT bool ;
      LEFT string ;
      LOOP_LEFT
        { IF { PUSH string "done" ; RIGHT string } { PUSH bool True ; LEFT bool } } } |}]

let%expect_test "for" =
  let i = mut_var "i" in
  let e = for_ i ~init:(nat 0) ~cond:(lt (deref i nat_ty) (nat 3))
      ~update:(assign (mut_var "i") (add (deref (mut_var "i") nat_ty) (nat 1)))
      ~body:(string "looping") in
  test_expr (let_mut_in (mut_var "i") ~rhs:(nat 0) ~in_:e);
  [%expect {|
    { PUSH nat 0 ;
      PUSH nat 0 ;
      PUSH nat 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH string "looping" ;
             DROP ;
             PUSH nat 1 ;
             DUP 2 ;
             ADD ;
             DUG 1 ;
             DIG 0 ;
             DROP ;
             UNIT ;
             DROP ;
             PUSH nat 3 ;
             DUP 2 ;
             COMPARE ;
             LT } ;
      DROP ;
      UNIT ;
      SWAP ;
      DROP }

    Optimised:
    { PUSH nat 0 ;
      PUSH nat 3 ;
      DUP 2 ;
      COMPARE ;
      LT ;
      LOOP { PUSH nat 1 ; ADD ; PUSH nat 3 ; DUP 2 ; COMPARE ; LT } ;
      DROP ;
      UNIT } |}]

let%expect_test "for each" =
  let items = cons (string "one") (cons (string "two") (nil string_ty)) in
  let e = for_each items ~body:{
    lam_var = (var "s", string_ty);
    body = bool true
  } in
  test_expr e;
  [%expect {|
    { NIL string ;
      PUSH string "two" ;
      CONS ;
      PUSH string "one" ;
      CONS ;
      ITER { DROP ; PUSH bool True ; DROP } ;
      UNIT }

    Optimised:
    { PUSH (list string) { "one" ; "two" } ; ITER { DROP } ; UNIT } |}]

let%expect_test "map1" =
  let lst = cons (int 1) (cons (int 2) (nil int_ty)) in
  let e = map lst ~map:{ lam_var = (var "x", int_ty); body = add (variable (var "x") int_ty) (int 10) } in
  test_expr e;
  [%expect {|
    { NIL int ;
      PUSH int 2 ;
      CONS ;
      PUSH int 1 ;
      CONS ;
      MAP { PUSH int 10 ; SWAP ; ADD } }

    Optimised:
    { PUSH (list int) { 1 ; 2 } ; MAP { PUSH int 10 ; ADD } } |}]

let%expect_test "map2" =
  let opt_val = some (bool true) in
  let e = map opt_val ~map:{ lam_var = (var "b", bool_ty); body = not (variable (var "b") bool_ty) }  in
  test_expr e;
  [%expect {|
    { PUSH bool True ; SOME ; MAP { NOT } }

    Optimised:
    { PUSH bool True ; SOME ; MAP { NOT } } |}]

let%expect_test "map with operations" =
  let coll = cons (nat 1) (cons (nat 2) (cons (nat 3) (nil nat_ty))) in
  let lam_map = annon_function "x" nat_ty ~body:(add (variable (var "x") nat_ty) (nat 10))
  in
  let expr = map coll ~map:lam_map in
  test_expr expr;
  [%expect {|
    { NIL nat ;
      PUSH nat 3 ;
      CONS ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      MAP { PUSH nat 10 ; SWAP ; ADD } }

    Optimised:
    { PUSH (list nat) { 1 ; 2 ; 3 } ; MAP { PUSH nat 10 ; ADD } } |}]

let%expect_test "fold left" =
  let lst = cons (nat 2) (cons (nat 3) (nil nat_ty)) in
  let e = fold_left lst ~init:(nat 1) ~fold:{ lam_var = (var "acc_x", tuple_ty (mk_row [
    Leaf (None, nat_ty);
    Leaf (None, nat_ty)
  ]));
  body = mul
    (car (variable (var "acc_x") (tuple_ty (mk_row [
       Leaf (None, nat_ty);
       Leaf (None, nat_ty)
     ]))))
    (cdr (variable (var "acc_x") (tuple_ty (mk_row [
       Leaf (None, nat_ty);
       Leaf (None, nat_ty)
     ]))))
} in
  test_expr e;
  [%expect {|
    { PUSH nat 1 ;
      NIL nat ;
      PUSH nat 3 ;
      CONS ;
      PUSH nat 2 ;
      CONS ;
      ITER { SWAP ; PAIR ; DUP 1 ; CDR ; SWAP ; CAR ; MUL } }

    Optimised:
    { PUSH nat 1 ; PUSH (list nat) { 2 ; 3 } ; ITER { SWAP ; MUL } } |}]

let%expect_test "fold_left with operations" =
  let coll = cons (nat 1) (cons (nat 2) (cons (nat 3) (nil nat_ty))) in
  let fold_body =
    annon_function "acc_x" (tuple_ty (mk_row [
        Leaf (None, nat_ty); 
        Leaf (None, nat_ty)
      ]))
      ~body:(add
        (car (variable (var "acc_x") (tuple_ty (mk_row [
          Leaf (None, nat_ty);
          Leaf (None, nat_ty)
        ]))))
        (cdr (variable (var "acc_x") (tuple_ty (mk_row [
          Leaf (None, nat_ty);
          Leaf (None, nat_ty)
        ])))))
  in
  let expr = fold_left coll ~init:(nat 0) ~fold:fold_body in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 3 ;
      CONS ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      ITER { SWAP ; PAIR ; DUP 1 ; CDR ; SWAP ; CAR ; ADD } }

    Optimised:
    { PUSH nat 0 ; PUSH (list nat) { 1 ; 2 ; 3 } ; ITER { ADD } } |}]

let%expect_test "fold right" =
  let lst = cons (int 1) (cons (int 2) (nil int_ty)) in
  let e = fold_right lst ~init:(int 0) ~fold:{ lam_var = (var "elem_acc", tuple_ty (mk_row [
    Leaf (None, int_ty);
    Leaf (None, int_ty)
  ]));
  body = add
    (car (variable (var "elem_acc") (tuple_ty (mk_row [
       Leaf (None, int_ty);
       Leaf (None, int_ty)
     ]))))
    (cdr (variable (var "elem_acc") (tuple_ty (mk_row [
       Leaf (None, int_ty);
       Leaf (None, int_ty)
     ]))))
} in
  test_expr e;
  [%expect {|
    { PUSH int 0 ;
      NIL int ;
      PUSH int 2 ;
      CONS ;
      PUSH int 1 ;
      CONS ;
      NIL int ;
      SWAP ;
      ITER { CONS } ;
      ITER { PAIR ; DUP 1 ; CDR ; SWAP ; CAR ; ADD } }

    Optimised:
    { PUSH int 0 ;
      NIL int ;
      PUSH (list int) { 1 ; 2 } ;
      ITER { CONS } ;
      ITER { ADD } } |}]

let%expect_test "fold_right with operations" =
  let coll = cons (nat 1) (cons (nat 2) (cons (nat 3) (nil nat_ty))) in
  let fold_body =
    annon_function "x_acc" (tuple_ty (mk_row [
        Leaf (None, nat_ty);
        Leaf (None, nat_ty)
      ]))
      ~body: (sub (cdr (variable (var "x_acc") (tuple_ty (mk_row [
        Leaf (None, nat_ty);
        Leaf (None, nat_ty)
      ]))))
      (car (variable (var "x_acc") (tuple_ty (mk_row [
        Leaf (None, nat_ty);
        Leaf (None, nat_ty)
      ])))))
  in
  let expr = fold_right coll ~init:(nat 0) ~fold:fold_body in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ;
      NIL nat ;
      PUSH nat 3 ;
      CONS ;
      PUSH nat 2 ;
      CONS ;
      PUSH nat 1 ;
      CONS ;
      NIL nat ;
      SWAP ;
      ITER { CONS } ;
      ITER { PAIR ; DUP 1 ; CAR ; SWAP ; CDR ; SUB } }

    Optimised:
    { PUSH nat 0 ;
      NIL nat ;
      PUSH (list nat) { 1 ; 2 ; 3 } ;
      ITER { CONS } ;
      ITER { SWAP ; SUB } } |}]

let%expect_test "let tuple in basic" =
  let rhs_tuple =
    tuple (mk_row [
      Leaf (None, nat 1);
      Leaf (None, bool true)
    ])
  in
  let expr =
    let_tuple_in
      [var "x"; var "b"]
      ~rhs:rhs_tuple
      ~in_:
        (if_bool
          (variable (var "b") bool_ty)
          ~then_:(add (variable (var "x") nat_ty) (nat 10))
          ~else_:(nat 0))
  in
  test_expr expr;
  [%expect {|
    { PUSH bool True ;
      PUSH nat 1 ;
      PAIR ;
      UNPAIR ;
      SWAP ;
      IF { PUSH nat 10 ; SWAP ; ADD } { DROP ; PUSH nat 0 } }

    Optimised:
    { PUSH nat 1 ; PUSH nat 10 ; ADD } |}]

let%expect_test "let tuple in nested" =
  let inner_tuple = tuple (mk_row [Leaf (None, int 2); Leaf (None, nat 5)]) in
  let outer_tuple =
    tuple (mk_row [
      Leaf (None, inner_tuple);
      Leaf (None, bool false)
    ])
  in
  let expr =
    let_tuple_in
      [var "p"; var "flag"]
      ~rhs:outer_tuple
      ~in_:
        (if_bool
          (variable (var "flag") bool_ty)
          ~then_:(car (variable (var "p") (tuple_ty (mk_row [
             Leaf (None, int_ty);
             Leaf (None, nat_ty)
           ]))))
          ~else_:(cdr (variable (var "p") (tuple_ty (mk_row [
             Leaf (None, int_ty);
             Leaf (None, nat_ty)
           ])))))
  in
  test_expr expr;
  [%expect {|
    { PUSH bool False ;
      PUSH nat 5 ;
      PUSH int 2 ;
      PAIR ;
      PAIR ;
      UNPAIR ;
      SWAP ;
      IF { CAR } { CDR } }

    Optimised:
    { PUSH nat 5 } |}]

let%expect_test "nested tuple and projection" =
  let triple =
    tuple (mk_row [
      Leaf (None, nat 1);
      Leaf (None, nat 2);
      Leaf (None, nat 3)
    ])
  in
  let expr =
    let_tuple_in [(var "x"); (var "y"); (var "z")]
      ~rhs:triple
      ~in_:
        (let_in (var "a")
          ~rhs:(
            proj
              (tuple (mk_row [
                Leaf (None, variable (var "y") nat_ty);
                Leaf (None, variable (var "z") nat_ty)
              ]))
              ~path:(Here [0])
          )
          ~in_:(add (variable (var "a") nat_ty) (variable (var "x") nat_ty)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 3 ;
      PUSH nat 2 ;
      PUSH nat 1 ;
      PAIR 3 ;
      UNPAIR 3 ;
      DIG 2 ;
      DIG 2 ;
      PAIR ;
      GET 1 ;
      SWAP ;
      SWAP ;
      ADD }

    Optimised:
    { PUSH nat 3 ; PUSH nat 2 ; PUSH nat 1 ; DIG 2 ; DROP ; ADD } |}]

let%expect_test "proj second element" =
  let rhs_tuple =
    tuple (mk_row [
      Leaf (None, string "hello");
      Leaf (None, int 42);
      Leaf (None, bool false)
    ])
  in
  let second_elem = proj rhs_tuple ~path:(Here [1]) in
  let expr = second_elem in
  test_expr expr;
  [%expect {|
    { PUSH bool False ; PUSH int 42 ; PUSH string "hello" ; PAIR 3 ; GET 3 }

    Optimised:
    { PUSH bool False ; PUSH int 42 ; PUSH string "hello" ; PAIR 3 ; GET 3 } |}]

let%expect_test "proj nested element" =
  let inner = mk_row [Leaf (None, nat 10); Leaf (None, nat 20)] in
  let outer = tuple (mk_row [Leaf (None, string "s"); inner]) in
  let extract = proj outer ~path:(Here [1;0]) in
  let expr = extract in
  test_expr expr;
  [%expect {|
    { PUSH nat 20 ; PUSH nat 10 ; PAIR ; PUSH string "s" ; PAIR ; GET 2 ; GET 1 }

    Optimised:
    { PUSH nat 10 } |}]

let%expect_test "update tuple index" =
  let original_tuple =
    tuple (mk_row [
      Leaf (None, bool true);
      Leaf (None, nat 123)
    ])
  in
  let updated = update_tuple original_tuple ~component:(Here [1]) ~update:(nat 999) in
  let expr = updated in
  test_expr expr;
  [%expect {|
    { PUSH nat 123 ;
      PUSH bool True ;
      PAIR ;
      DUP 1 ;
      GET 2 ;
      DROP ;
      PUSH nat 999 ;
      UPDATE 2 }

    Optimised:
    { PUSH nat 123 ; PUSH bool True ; PAIR ; PUSH nat 999 ; UPDATE 2 } |}]

(* TODO: INJ/MATCH used in SmartPY
let%expect_test "inj two branch" =
  let left_or_right =
    or_ty (Node [
      Leaf (None, int_ty);
      Leaf (None, string_ty)
    ])
  in
  let c = Row.Context.Node ([], Row.Context.Hole left_or_right, []) in
  let expr = inj c (int 42) in
  test_expr expr;
  [%expect {||}]

let%expect_test "match two branch left" =
  let sum_type =
    mk_type ~range:v (LLTZ.T.Or (R.Node [
      R.Leaf (None, int_ty);
      R.Leaf (None, string_ty)
    ]))
  in
  let c = LLTZ.R.Context.Node ([], sum_type, []) in
  let left_side = inj c (int 7) in
  let cases =
    R.Node [
      R.Leaf (None, { lam_var = (var "l", int_ty); body = nat 999 });
      R.Leaf (None, { lam_var = (var "r", string_ty); body = nat 111 })
    ]
  in
  let expr = match_ left_side ~cases in
  test_expr expr;
  [%expect {||}]

let%expect_test "match two branch right" =
  let sum_type =
    mk_type ~range:v (LLTZ.T.Or (R.Node [
      R.Leaf (None, int_ty);
      R.Leaf (None, string_ty)
    ]))
  in
  let c = LLTZ.R.Context.Node ([], sum_type, []) in
  let right_side = inj c (string "abc") in
  let cases =
    R.Node [
      R.Leaf (None, { lam_var = (var "l", int_ty); body = nat 999 });
      R.Leaf (None, { lam_var = (var "r", string_ty); body = nat 111 })
    ]
  in
  let expr = match_ right_side ~cases in
  test_expr expr;
  [%expect {||}]

let%expect_test "match three branch" =
  let triple_type =
    mk_type ~range:v (LLTZ.T.Or (R.Node [
      R.Leaf (Some (R.Label "A"), bool_ty);
      R.Leaf (Some (R.Label "B"), nat_ty);
      R.Leaf (Some (R.Label "C"), string_ty)
    ]))
  in
  let c = LLTZ.R.Context.Node (
    [],
    triple_type,
    []
  )
  in
  let triple_inj = inj c (string "tagC") in
  let cases =
    R.Node [
      R.Leaf (Some (R.Label "A"), { lam_var = (var "ab", bool_ty); body = int 1 });
      R.Leaf (Some (R.Label "B"), { lam_var = (var "bn", nat_ty); body = int 2 });
      R.Leaf (Some (R.Label "C"), { lam_var = (var "cs", string_ty); body = int 3 })
    ]
  in
  let expr = match_ triple_inj ~cases in
  test_expr expr;
  [%expect {||}]
*)

(* Raw michelson *)
let push_int n = 
  let open Tezos_micheline.Micheline in
  Prim ( Ast_builder.Dummy_range.v, "PUSH", [Int (Ast_builder.Dummy_range.v, Z.of_int n) ], ["int"])
let seq_of_prim prim = Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [prim])
let seq instrs = Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, instrs)


let%expect_test "raw michelson single push int" =
  let michelson_ast =
    seq_of_prim (push_int 42)
  in
  let expr = raw_michelson michelson_ast [] int_ty in
  test_expr ~optimise:false expr;
  [%expect {| { { PUSH int 42 } } |}]

let%expect_test "raw michelson multiple instructions" =
  let open Tezos_micheline.Micheline in
  let michelson_ast =
    Seq (Ast_builder.Dummy_range.v,
      [
        push_int 3;
        push_int 5;
        Prim (Ast_builder.Dummy_range.v, "ADD", [], [])
      ])
  in
  let expr = raw_michelson michelson_ast [] int_ty in
  test_expr ~optimise:false expr;
  [%expect {| { { PUSH int 3 ; PUSH int 5 ; ADD } } |}]

let%expect_test "raw michelson with arguments" =
  let open Tezos_micheline.Micheline in
  let michelson_ast =
    seq_of_prim
      (Prim (Ast_builder.Dummy_range.v, "MUL", [], []))
  in
  let expr = raw_michelson michelson_ast [int 2; int 8] int_ty in
  test_expr ~optimise:false expr;
  [%expect {| { PUSH int 8 ; PUSH int 2 ; { MUL } } |}]

let%expect_test "raw michelson returning string" =
  let open Tezos_micheline.Micheline in
  let michelson_ast =
    Seq (Ast_builder.Dummy_range.v,
      [
        Prim (Ast_builder.Dummy_range.v, "PUSH", [String (Ast_builder.Dummy_range.v, "hello")], ["string"])
      ])
  in
  let expr = raw_michelson michelson_ast [] string_ty in
  test_expr ~optimise:false expr;
  [%expect {| { { PUSH string "hello" } } |}]

let%expect_test "raw michelson complex seq" =
  let open Tezos_micheline.Micheline in
  let michelson_ast =
    Seq (Ast_builder.Dummy_range.v,
      [
        push_int 10;
        push_int 20;
        Prim (Ast_builder.Dummy_range.v, "PAIR", [], []);
        Prim (Ast_builder.Dummy_range.v, "DUP", [], [])
      ])
  in
  let expr = raw_michelson michelson_ast [] (tuple_ty (mk_row [
    Leaf (None, int_ty);
    Leaf (None, int_ty)
  ]))
  in
  test_expr ~optimise:false expr;
  [%expect {| { { PUSH int 10 ; PUSH int 20 ; PAIR ; DUP } } |}]

let operation_list_ty =
  list_ty (operation_ty)

let return_no_ops stg_expr stg_ty =
  tuple (mk_row [
    Leaf (None, nil (operation_ty));
    Leaf (None, stg_expr)
  ]),
  mk_tuple_ty [operation_list_ty; stg_ty]

let%expect_test "create contract unit nat increment" =
  let storage_ty = nat_ty in
  let initial_storage_expr = nat 100 in
  let param_storage_ty =
    mk_tuple_ty [unit_ty; storage_ty]
  in
  let code_lambda =
        let_tuple_in
          [var "p"; var "s"]
          ~rhs:(variable (var "args") param_storage_ty)
          ~in_:
            (let new_storage = add (variable (var "s") nat_ty) (nat 1) in
            let ret_expr, _ret_ty = return_no_ops new_storage nat_ty in
            ret_expr)
  in
  let expr =
    create_contract
      ~storage:storage_ty
      ~code:{ lam_var = (var "args", param_storage_ty); body = code_lambda }
      ~delegate:(none key_hash_ty)
      ~initial_balance:(mutez 1_000_000)
      ~initial_storage:initial_storage_expr
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 100 ;
      PUSH mutez 1000000 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter unit ;
          storage nat ;
          code { UNPAIR ; DROP ; PUSH nat 1 ; SWAP ; ADD ; NIL operation ; PAIR } } ;
      PAIR }

    Optimised:
    { PUSH nat 100 ;
      PUSH mutez 1000000 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter unit ;
          storage nat ;
          code { CDR ; PUSH nat 1 ; ADD ; NIL operation ; PAIR } } ;
      PAIR } |}]

let%expect_test "create contract bool string conditional" =
  let storage_ty = string_ty in
  let initial_storage_expr = string "init" in
  let param_storage_ty =
    mk_tuple_ty [bool_ty; storage_ty]
  in
  let code_lambda =(
        let_tuple_in
          [var "flag"; var "st"]
          ~rhs:(variable (var "args") param_storage_ty)
          ~in_:
            (if_bool (variable (var "flag") bool_ty)
              ~then_:
                (let ret_expr, _ret_ty = return_no_ops (string "param was true") string_ty in
                ret_expr)
              ~else_:
                (let ret_expr, _ret_ty = return_no_ops (string "param was false") string_ty in
                ret_expr))
      )
  in
  let expr =
    create_contract
      ~storage:storage_ty
      ~code:{ lam_var = (var "args", param_storage_ty); body = code_lambda }
      ~delegate:(some (key_hash "tz1DelegateKey"))
      ~initial_balance:(mutez 2_000_000)
      ~initial_storage:initial_storage_expr
  in
  test_expr expr;
  [%expect {|
    { PUSH string "init" ;
      PUSH mutez 2000000 ;
      PUSH key_hash "tz1DelegateKey" ;
      SOME ;
      CREATE_CONTRACT
        { parameter bool ;
          storage string ;
          code { UNPAIR ;
                 SWAP ;
                 DROP ;
                 IF { PUSH string "param was true" ; NIL operation ; PAIR }
                    { PUSH string "param was false" ; NIL operation ; PAIR } } } ;
      PAIR }

    Optimised:
    { PUSH string "init" ;
      PUSH mutez 2000000 ;
      PUSH key_hash "tz1DelegateKey" ;
      SOME ;
      CREATE_CONTRACT
        { parameter bool ;
          storage string ;
          code { CAR ;
                 IF { PUSH string "param was true" } { PUSH string "param was false" } ;
                 NIL operation ;
                 PAIR } } ;
      PAIR } |}]

let%expect_test "create contract int int accumulation" =
  let storage_ty = int_ty in
  let initial_storage_expr = int 5 in
  let param_storage_ty =
    mk_tuple_ty [int_ty; int_ty]
  in
  let code_lambda = (
        let_tuple_in
          [var "p"; var "s"]
          ~rhs:(variable (var "args") param_storage_ty)
          ~in_:
            (if_bool (eq (variable (var "p") int_ty) (int 0))
              ~then_:
                (let ret_expr, _ret_ty = return_no_ops (int 999) int_ty in
                ret_expr)
              ~else_:
                (let new_st = add (variable (var "p") int_ty) (variable (var "s") int_ty) in
                let ret_expr, _ret_ty = return_no_ops new_st int_ty in
                ret_expr))
      )
  in
  let expr =
    create_contract
      ~storage:storage_ty
      ~code:{ lam_var = (var "args", param_storage_ty); body = code_lambda }
      ~delegate:(none key_hash_ty)
      ~initial_balance:(mutez 9999999)
      ~initial_storage:initial_storage_expr
  in
  test_expr expr;
  [%expect {|
    { PUSH int 5 ;
      PUSH mutez 9999999 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter int ;
          storage int ;
          code { UNPAIR ;
                 PUSH int 0 ;
                 DUP 2 ;
                 COMPARE ;
                 EQ ;
                 IF { DROP ; DROP ; PUSH int 999 ; NIL operation ; PAIR }
                    { SWAP ; SWAP ; ADD ; NIL operation ; PAIR } } } ;
      PAIR }

    Optimised:
    { PUSH int 5 ;
      PUSH mutez 9999999 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter int ;
          storage int ;
          code { UNPAIR ;
                 DUP ;
                 EQ ;
                 IF { DROP 2 ; PUSH int 999 } { ADD } ;
                 NIL operation ;
                 PAIR } } ;
      PAIR } |}]

let%expect_test "create contract nat address no change" =
  let storage_ty = address_ty in
  let initial_storage_expr = address_const "KT1InitialAddress" in
  let param_storage_ty =
    mk_tuple_ty [nat_ty; address_ty]
  in
  let code_lambda =(
        let_tuple_in
          [var "p"; var "st"]
          ~rhs:(variable (var "args") param_storage_ty)
          ~in_:
            (let ret_expr, _ret_ty = return_no_ops (variable (var "st") address_ty) address_ty in
            ret_expr)
      )
  in
  let expr =
    create_contract
      ~storage:storage_ty
      ~code:{ lam_var = (var "args", param_storage_ty); body = code_lambda }
      ~delegate:(none key_hash_ty)
      ~initial_balance:(mutez 1234567)
      ~initial_storage:initial_storage_expr
  in
  test_expr expr;
  [%expect {|
    { PUSH address "KT1InitialAddress" ;
      PUSH mutez 1234567 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter nat ;
          storage address ;
          code { UNPAIR ; DROP ; NIL operation ; PAIR } } ;
      PAIR }

    Optimised:
    { PUSH address "KT1InitialAddress" ;
      PUSH mutez 1234567 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter nat ; storage address ; code { CDR ; NIL operation ; PAIR } } ;
      PAIR } |}]

let%expect_test "create contract bool flip storage" =
  let storage_ty = bool_ty in
  let initial_storage_expr = bool false in
  let param_storage_ty =
    mk_tuple_ty [unit_ty; bool_ty]
  in
  let code_lambda = (
        let_tuple_in
          [var "p"; var "s"]
          ~rhs:(variable (var "args") param_storage_ty)
          ~in_:
            (if_bool (variable (var "s") bool_ty)
              ~then_:
                (let ret_expr, _ret_ty = return_no_ops (bool true) bool_ty in
                ret_expr)
              ~else_:
                (let ret_expr, _ret_ty = return_no_ops (bool true) bool_ty in
                ret_expr))
      )
  in
  let expr =
    create_contract
      ~storage:storage_ty
      ~code:{ lam_var = (var "args", param_storage_ty); body = code_lambda }
      ~delegate:(none key_hash_ty)
      ~initial_balance:(mutez 500000)
      ~initial_storage:initial_storage_expr
  in
  test_expr expr;
  [%expect {|
    { PUSH bool False ;
      PUSH mutez 500000 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter unit ;
          storage bool ;
          code { UNPAIR ;
                 DROP ;
                 IF { PUSH bool True ; NIL operation ; PAIR }
                    { PUSH bool True ; NIL operation ; PAIR } } } ;
      PAIR }

    Optimised:
    { PUSH bool False ;
      PUSH mutez 500000 ;
      NONE key_hash ;
      CREATE_CONTRACT
        { parameter unit ;
          storage bool ;
          code { CDR ;
                 IF { PUSH bool True } { PUSH bool True } ;
                 NIL operation ;
                 PAIR } } ;
      PAIR } |}]

let%expect_test "create contract complex" =
let storage_ty = nat_ty in
let param_storage_ty =
  mk_tuple_ty [nat_ty; nat_ty]
in
let code_lambda =
  annon_function "args" param_storage_ty
    ~body:(
      let_tuple_in
        [(var "p"); (var "s")]
        ~rhs:(variable (var "args") param_storage_ty)
        ~in_:
          (let_in (var "x") ~rhs:(nat 1)
            ~in_:
              (let new_s = add (variable (var "x") nat_ty) (nat 2) in
              (* returns (nil operation, new_s) *)
              tuple (mk_row [
                Leaf (None, nil (operation_ty));
                Leaf (None, new_s)
              ])))
    )
in
let expr =
  create_contract
    ~storage:storage_ty
    ~code:code_lambda
    ~delegate:(none key_hash_ty)
    ~initial_balance:(mutez 1000)
    ~initial_storage:(nat 10)
in
test_expr expr;
[%expect {|
  { PUSH nat 10 ;
    PUSH mutez 1000 ;
    NONE key_hash ;
    CREATE_CONTRACT
      { parameter nat ;
        storage nat ;
        code { UNPAIR ;
               DROP ;
               DROP ;
               PUSH nat 1 ;
               PUSH nat 2 ;
               SWAP ;
               ADD ;
               NIL operation ;
               PAIR } } ;
    PAIR }
  
  Optimised:
  { PUSH nat 10 ;
    PUSH mutez 1000 ;
    NONE key_hash ;
    CREATE_CONTRACT
      { parameter nat ;
        storage nat ;
        code { DROP ; PUSH nat 1 ; PUSH nat 2 ; ADD ; NIL operation ; PAIR } } ;
    PAIR } |}]

(* ARITY 0 PRIMITIVES *)

let%expect_test "amount basic" =
  let e = amount in
  test_expr e;
  [%expect {|
    { AMOUNT }

    Optimised:
    { AMOUNT } |}]

let%expect_test "balance basic" =
  let e = balance in
  test_expr e;
  [%expect {|
    { BALANCE }

    Optimised:
    { BALANCE } |}]

let%expect_test "chain_id basic" =
  let e = chain_id_prim in
  test_expr e;
  [%expect {|
    { CHAIN_ID }

    Optimised:
    { CHAIN_ID } |}]

let%expect_test "level basic" =
  let e = level in
  test_expr e;
  [%expect {|
    { LEVEL }

    Optimised:
    { LEVEL } |}]

let%expect_test "now basic" =
  let e = now in
  test_expr e;
  [%expect {|
    { NOW }

    Optimised:
    { NOW } |}]

let%expect_test "self without entrypoint" =
  let contract_type = contract_ty unit_ty in
  let e = self None contract_type in
  test_expr e;
  [%expect {|
    { SELF }

    Optimised:
    { SELF } |}]

let%expect_test "self with entrypoint" =
  let contract_type = contract_ty nat_ty in
  let e = self (Some ("%test_entry")) contract_type in
  test_expr e;
  [%expect {|
    { SELF %test_entry }

    Optimised:
    { SELF %test_entry } |}]

let%expect_test "self_address basic" =
  let e = self_address in
  test_expr e;
  [%expect {|
    { SELF_ADDRESS }

    Optimised:
    { SELF_ADDRESS } |}]

let%expect_test "sender basic" =
  let e = sender in
  test_expr e;
  [%expect {|
    { SENDER }

    Optimised:
    { SENDER } |}]

let%expect_test "source basic" =
  let e = source in
  test_expr e;
  [%expect {|
    { SOURCE }

    Optimised:
    { SOURCE } |}]

let%expect_test "total_voting_power basic" =
  let e = total_voting_power in
  test_expr e;
  [%expect {|
    { TOTAL_VOTING_POWER }

    Optimised:
    { TOTAL_VOTING_POWER } |}]

let%expect_test "empty_bigmap nat->int" =
  let e = empty_bigmap nat_ty int_ty in
  test_expr e;
  [%expect {|
    { EMPTY_BIG_MAP nat int }

    Optimised:
    { EMPTY_BIG_MAP nat int } |}]

let%expect_test "empty_map string->bool" =
  let e = empty_map string_ty bool_ty in
  test_expr e;
  [%expect {|
    { EMPTY_MAP string bool }

    Optimised:
    { EMPTY_MAP string bool } |}]

let%expect_test "empty_set address" =
  let e = empty_set address_ty in
  test_expr e;
  [%expect {|
    { EMPTY_SET address }

    Optimised:
    { EMPTY_SET address } |}]

let%expect_test "nil int" =
  let e = nil int_ty in
  test_expr e;
  [%expect {|
    { NIL int }

    Optimised:
    { NIL int } |}]

let%expect_test "none bool" =
  let e = none bool_ty in
  test_expr e;
  [%expect {|
    { NONE bool }

    Optimised:
    { NONE bool } |}]

let%expect_test "sapling_empty_state memo8" =
  let e = sapling_empty_state 8  in
  test_expr e;
  [%expect {|
    { SAPLING_EMPTY_STATE 8 }

    Optimised:
    { SAPLING_EMPTY_STATE 8 }
    ms: 8
    ms: 8 |}]

let%expect_test "unit prim" =
  let e = unit_prim in
  test_expr e;
  [%expect {|
    { UNIT }

    Optimised:
    { UNIT } |}]

(* ARITY 1/2 PRIMITIVES *)

let%expect_test "car on pair" =
  let pair_expr = pair (None,None) (int 1) (int 2) in
  let e = car pair_expr in
  test_expr e;
  [%expect {|
    { PUSH int 2 ; PUSH int 1 ; PAIR ; CAR }

    Optimised:
    { PUSH int 1 } |}]

let%expect_test "cdr on pair" =
  let pair_expr = pair (None,None) (int 1) (int 2) in
  let e = cdr pair_expr in
  test_expr e;
  [%expect {|
    { PUSH int 2 ; PUSH int 1 ; PAIR ; CDR }

    Optimised:
    { PUSH int 2 } |}]

let%expect_test "left constructor" =
  let e = left (None,None, string_ty) (int 42) in
  test_expr e;
  [%expect {|
    { PUSH int 42 ; LEFT string }

    Optimised:
    { PUSH int 42 ; LEFT string } |}]

let%expect_test "right constructor" =
  let e = right (None,None, bool_ty) (string "test") in
  test_expr e;
  [%expect {|
    { PUSH string "test" ; RIGHT bool }

    Optimised:
    { PUSH string "test" ; RIGHT bool } |}]

let%expect_test "some constructor" =
  let e = some (nat 999) in
  test_expr e;
  [%expect {|
    { PUSH nat 999 ; SOME }

    Optimised:
    { PUSH nat 999 ; SOME } |}]

let%expect_test "abs on negative int" =
  let e = abs (int (-42)) in
  test_expr e;
  [%expect {|
    { PUSH int -42 ; ABS }

    Optimised:
    { PUSH int -42 ; ABS } |}]

let%expect_test "neg on nat (coerced to int)" =
  let e = neg (nat 10) in
  test_expr e;
  [%expect {|
    { PUSH nat 10 ; NEG }

    Optimised:
    { PUSH int -10 } |}]

let%expect_test "nat_prim from int" =
  let e = nat_prim (int 50) in
  test_expr e;
  [%expect {|
    { PUSH int 50 ; NAT }

    Optimised:
    { PUSH int 50 ; NAT } |}]

let%expect_test "int_prim from nat" =
  let e = int_prim (nat 123) in
  test_expr e;
  [%expect {|
    { PUSH nat 123 ; INT }

    Optimised:
    { PUSH nat 123 ; INT } |}]

let%expect_test "bytes_prim from string" =
  let e = bytes_prim (string "raw") in
  test_expr e;
  [%expect {|
    { PUSH string "raw" ; BYTES }

    Optimised:
    { PUSH string "raw" ; BYTES } |}]

let%expect_test "is_nat with negative int" =
  let e = is_nat (int (-100)) in
  test_expr e;
  [%expect {|
    { PUSH int -100 ; ISNAT }

    Optimised:
    { PUSH int -100 ; ISNAT } |}]

let%expect_test "compare eq int" =
  let e = eq (int 2) (int 2) in
  test_expr e;
  [%expect {|
    { PUSH int 2 ; PUSH int 2 ; COMPARE ; EQ }

    Optimised:
    { PUSH bool True } |}]

let%expect_test "compare neq string" =
  let e = neq (string "hello") (string "world") in
  test_expr e;
  [%expect {|
    { PUSH string "world" ; PUSH string "hello" ; COMPARE ; NEQ }

    Optimised:
    { PUSH string "world" ; PUSH string "hello" ; COMPARE ; NEQ } |}]

let%expect_test "compare lt nat" =
  let e = lt (nat 5) (nat 10) in
  test_expr e;
  [%expect {|
    { PUSH nat 10 ; PUSH nat 5 ; COMPARE ; LT }

    Optimised:
    { PUSH bool True } |}]

let%expect_test "compare gt int" =
  let e = gt (int 10) (int 3) in
  test_expr e;
  [%expect {|
    { PUSH int 3 ; PUSH int 10 ; COMPARE ; GT }

    Optimised:
    { PUSH bool True } |}]

let%expect_test "compare le int" =
  let e = le (int 1) (int 1) in
  test_expr e;
  [%expect {|
    { PUSH int 1 ; PUSH int 1 ; COMPARE ; LE }

    Optimised:
    { PUSH int 0 ; LE } |}]

let%expect_test "compare ge string" =
  let e = ge (string "a") (string "b") in
  test_expr e;
  [%expect {|
    { PUSH string "b" ; PUSH string "a" ; COMPARE ; GE }

    Optimised:
    { PUSH string "b" ; PUSH string "a" ; COMPARE ; GE } |}]

let%expect_test "not bool" =
  let e = not (bool false) in
  test_expr e;
  [%expect {|
    { PUSH bool False ; NOT }

    Optimised:
    { PUSH bool True } |}]

let%expect_test "not int" =
  let e = not (int 123) in
  test_expr e;
  [%expect {|
    { PUSH int 123 ; NOT }

    Optimised:
    { PUSH int 123 ; NOT } |}]

let%expect_test "size on bytes" =
  let e = size (bytes "0xABCDEF") in
  test_expr e;
  [%expect {|
    { PUSH bytes 0x3078414243444546 ; SIZE }

    Optimised:
    { PUSH bytes 0x3078414243444546 ; SIZE } |}]

let%expect_test "address of contract" =
  let contract_expr = self None (contract_ty unit_ty) in
  let e = address contract_expr in
  test_expr e;
  [%expect {|
    { SELF ; ADDRESS }

    Optimised:
    { SELF_ADDRESS } |}]

let%expect_test "implicit_account key_hash" =
  let e = implicit_account (key_hash "tz1ABC123") in
  test_expr e;
  [%expect {|
    { PUSH key_hash "tz1ABC123" ; IMPLICIT_ACCOUNT }

    Optimised:
    { PUSH key_hash "tz1ABC123" ; IMPLICIT_ACCOUNT } |}]

let%expect_test "contract opt (bool_ty) address" =
  let e = contract (None, bool_ty) (address_const "KT1XYZ") in
  test_expr e;
  [%expect {|
    { PUSH address "KT1XYZ" ; CONTRACT bool }

    Optimised:
    { PUSH address "KT1XYZ" ; CONTRACT bool } |}]

let%expect_test "pack int" =
  let e = pack (int (-7)) in
  test_expr e;
  [%expect {|
    { PUSH int -7 ; PACK }

    Optimised:
    { PUSH int -7 ; PACK } |}]

let%expect_test "unpack string" =
  let e = unpack string_ty (bytes "0xDEADBEEF") in
  test_expr e;
  [%expect {|
    { PUSH bytes 0x30784445414442454546 ; UNPACK string }

    Optimised:
    { PUSH bytes 0x30784445414442454546 ; UNPACK string } |}]

let%expect_test "hash_key key" =
  let e = hash_key (key "edpkExampleKey") in
  test_expr e;
  [%expect {|
    { PUSH key "edpkExampleKey" ; HASH_KEY }

    Optimised:
    { PUSH key "edpkExampleKey" ; HASH_KEY } |}]

let%expect_test "blake2b bytes" =
  let e = blake2b (bytes "0xAB12") in
  test_expr e;
  [%expect {|
    { PUSH bytes 0x307841423132 ; BLAKE2B }

    Optimised:
    { PUSH bytes 0x307841423132 ; BLAKE2B } |}]

let%expect_test "sha256 bytes" =
  let e = sha256 (bytes "0xAB12") in
  test_expr e;
  [%expect {|
    { PUSH bytes 0x307841423132 ; SHA256 }

    Optimised:
    { PUSH bytes 0x307841423132 ; SHA256 } |}]

let%expect_test "sha512 bytes" =
  let e = sha512 (bytes "0xAB12") in
  test_expr e;
  [%expect {|
    { PUSH bytes 0x307841423132 ; SHA512 }

    Optimised:
    { PUSH bytes 0x307841423132 ; SHA512 } |}]

let%expect_test "keccak bytes" =
  let e = keccak (bytes "0xAB12") in
  test_expr e;
  [%expect {|
    { PUSH bytes 0x307841423132 ; KECCAK }

    Optimised:
    { PUSH bytes 0x307841423132 ; KECCAK } |}]

let%expect_test "sha3 bytes" =
  let e = sha3 (bytes "0xAB12") in
  test_expr e;
  [%expect {|
    { PUSH bytes 0x307841423132 ; SHA3 }

    Optimised:
    { PUSH bytes 0x307841423132 ; SHA3 } |}]

let%expect_test "set_delegate none" =
  let e = set_delegate (none key_hash_ty) in
  test_expr e;
  [%expect {|
    { NONE key_hash ; SET_DELEGATE }

    Optimised:
    { NONE key_hash ; SET_DELEGATE } |}]

let%expect_test "read_ticket ticket" =
  let unwrap_ticket =
    if_none (ticket (string "T") (nat 5))
      ~none:(failwith (string "No ticket"))
      ~some:{
        lam_var = (var "tk", ticket_ty string_ty);
        body = variable (var "tk") (ticket_ty string_ty)
      }
  in
  let e = read_ticket unwrap_ticket in
  test_expr e;
  [%expect {|
    { PUSH nat 5 ;
      PUSH string "T" ;
      TICKET ;
      IF_NONE { PUSH string "No ticket" ; FAILWITH } {} ;
      { READ_TICKET ; PAIR } }

    Optimised:
    { PUSH nat 5 ;
      PUSH string "T" ;
      TICKET ;
      IF_NONE { PUSH string "No ticket" ; FAILWITH } {} ;
      READ_TICKET ;
      PAIR } |}]

let%expect_test "join_tickets same type" =
  let t1_opt = ticket (string "C1") (nat 2) in
  let t2_opt = ticket (string "C1") (nat 3) in
  let unwrap opt_e var_name =
    if_none opt_e
      ~none:(failwith (string "no ticket"))
      ~some:{
        lam_var = (var var_name, ticket_ty string_ty);
        body = variable (var var_name) (ticket_ty string_ty)
      }
  in
  let t1 = unwrap t1_opt "tk1" in
  let t2 = unwrap t2_opt "tk2" in
  let e = join_tickets t1 t2 in
  test_expr e;
  [%expect {|
    { PUSH nat 3 ;
      PUSH string "C1" ;
      TICKET ;
      IF_NONE { PUSH string "no ticket" ; FAILWITH } {} ;
      PUSH nat 2 ;
      PUSH string "C1" ;
      TICKET ;
      IF_NONE { PUSH string "no ticket" ; FAILWITH } {} ;
      JOIN_TICKETS }

    Optimised:
    { PUSH nat 3 ;
      PUSH string "C1" ;
      TICKET ;
      IF_NONE { PUSH string "no ticket" ; FAILWITH } {} ;
      PUSH nat 2 ;
      PUSH string "C1" ;
      TICKET ;
      IF_NONE { PUSH string "no ticket" ; FAILWITH } {} ;
      JOIN_TICKETS } |}]

let%expect_test "pairing_check bls12 list" =
  let g1 = bls12_381_g1 "G1" in
  let g2 = bls12_381_g2 "G2" in
  let pair_expr = pair (None,None) g1 g2 in
  let list_expr = cons pair_expr (nil (mk_tuple_ty [bls12_381_g1_ty; bls12_381_g2_ty])) in
  let e = pairing_check list_expr in
  test_expr e;
  [%expect {|
    { NIL (pair bls12_381_g1 bls12_381_g2) ;
      PUSH bls12_381_g2 0x4732 ;
      PUSH bls12_381_g1 0x4731 ;
      PAIR ;
      CONS ;
      PAIRING_CHECK }

    Optimised:
    { PUSH (list (pair bls12_381_g1 bls12_381_g2)) { Pair 0x4731 0x4732 } ;
      PAIRING_CHECK } |}]

let%expect_test "voting_power key_hash" =
  let e = voting_power (key_hash "tz1Example") in
  test_expr e;
  [%expect {|
    { PUSH key_hash "tz1Example" ; VOTING_POWER }

    Optimised:
    { PUSH key_hash "tz1Example" ; VOTING_POWER } |}]

(*let%expect_test "getn usage" =
  (* Commented out as getn is not implemented *)
  let triple = tuple (mk_row [
    Leaf (None, nat 1);
    Leaf (None, int (-1));
    Leaf (None, bool true)
  ]) in
  let expr = getn 2 triple in
  test_expr expr;
  [%expect {||}]*)

let%expect_test "cast from int to int" =
  let e = cast int_ty (int 42) in
  test_expr e;
  [%expect {|
    { PUSH int 42 ; CAST int }

    Optimised:
    { PUSH int 42 ; CAST int } |}]

let%expect_test "cast from nat to int" =
  let e = cast int_ty (nat 999) in
  test_expr e;
  [%expect {|
    { PUSH nat 999 ; CAST int }

    Optimised:
    { PUSH nat 999 ; CAST int } |}]

let%expect_test "cast from bool to bool" =
  let e = cast bool_ty (bool true) in
  test_expr e;
  [%expect {|
    { PUSH bool True ; CAST bool }

    Optimised:
    { PUSH bool True ; CAST bool } |}]

let%expect_test "emit basic" =
  let e = emit (None, None) (string "Event data") in
  test_expr e;
  [%expect {|
    { PUSH string "Event data" ; EMIT }

    Optimised:
    { PUSH string "Event data" ; EMIT } |}]

let%expect_test "emit with no annotation" =
  let e = emit (None,  Some(string_ty)) (string "Event data") in
  test_expr e;
  [%expect {|
    { PUSH string "Event data" ; EMIT string }

    Optimised:
    { PUSH string "Event data" ; EMIT string } |}]

let%expect_test "emit with no data" =
  let e = emit (Some "LabelX", None) (string "Data2") in
  test_expr e;
  [%expect {|
    { PUSH string "Data2" ; EMIT LabelX }

    Optimised:
    { PUSH string "Data2" ; EMIT LabelX } |}]

let%expect_test "emit with label" =
  let e = emit (Some "LabelX", Some(string_ty)) (string "Data2") in
  test_expr e;
  [%expect {|
    { PUSH string "Data2" ; EMIT LabelX string }

    Optimised:
    { PUSH string "Data2" ; EMIT LabelX string } |}]

let%expect_test "failwith basic" =
  let e = failwith (string "Something went wrong") in
  test_expr e;
  [%expect {|
    { PUSH string "Something went wrong" ; FAILWITH }

    Optimised:
    { PUSH string "Something went wrong" ; FAILWITH } |}]

let%expect_test "never basic" =
  let e = never (int 100) in
  test_expr e;
  [%expect {|
    { PUSH int 100 ; NEVER }

    Optimised:
    { PUSH int 100 ; NEVER } |}]

let%expect_test "pair simple" =
  let e = pair (None, None) (int 1) (int 2) in
  test_expr e;
  [%expect {|
    { PUSH int 2 ; PUSH int 1 ; PAIR }

    Optimised:
    { PUSH int 2 ; PUSH int 1 ; PAIR } |}]

let%expect_test "pair with annotation" =
  let e = pair (Some "fst", Some "snd") (string "hi") (bool false) in
  test_expr e;
  [%expect {|
    { PUSH bool False ; PUSH string "hi" ; PAIR fst snd }

    Optimised:
    { PUSH bool False ; PUSH string "hi" ; PAIR } |}]

let%expect_test "add nat nat" =
  let e = add (nat 3) (nat 5) in
  test_expr e;
  [%expect {|
    { PUSH nat 5 ; PUSH nat 3 ; ADD }

    Optimised:
    { PUSH nat 5 ; PUSH nat 3 ; ADD } |}]

let%expect_test "add int nat" =
  let e = add (int (-2)) (nat 7) in
  test_expr e;
  [%expect {|
    { PUSH nat 7 ; PUSH int -2 ; ADD }

    Optimised:
    { PUSH nat 7 ; PUSH int -2 ; ADD } |}]

let%expect_test "add timestamp int" =
  let e = add (timestamp "2023-01-01T00:00:00Z") (int 60) in
  test_expr e;
  [%expect {|
    { PUSH int 60 ; PUSH timestamp 1672531200 ; ADD }

    Optimised:
    { PUSH int 60 ; PUSH timestamp 1672531200 ; ADD } |}]

let%expect_test "mul nat nat" =
  let e = mul (nat 2) (nat 10) in
  test_expr e;
  [%expect {|
    { PUSH nat 10 ; PUSH nat 2 ; MUL }

    Optimised:
    { PUSH nat 10 ; PUSH nat 2 ; MUL } |}]

let%expect_test "mul int nat" =
  let e = mul (int 3) (nat 10) in
  test_expr e;
  [%expect {|
    { PUSH nat 10 ; PUSH int 3 ; MUL }

    Optimised:
    { PUSH nat 10 ; PUSH int 3 ; MUL } |}]

let%expect_test "mul mutez nat" =
  let e = mul (mutez 1000) (nat 2) in
  test_expr e;
  [%expect {|
    { PUSH nat 2 ; PUSH mutez 1000 ; MUL }

    Optimised:
    { PUSH nat 2 ; PUSH mutez 1000 ; MUL } |}]

let%expect_test "sub int int" =
  let e = sub (int 5) (int 10) in
  test_expr e;
  [%expect {|
    { PUSH int 10 ; PUSH int 5 ; SUB }

    Optimised:
    { PUSH int -5 } |}]

let%expect_test "sub nat nat" =
  let e = sub (nat 10) (nat 3) in
  test_expr e;
  [%expect {|
    { PUSH nat 3 ; PUSH nat 10 ; SUB }

    Optimised:
    { PUSH nat 3 ; PUSH nat 10 ; SUB } |}]

let%expect_test "sub timestamp int" =
  let e = sub (timestamp "2023-01-01T00:00:00Z") (int 3600) in
  test_expr e;
  [%expect {|
    { PUSH int 3600 ; PUSH timestamp 1672531200 ; SUB }

    Optimised:
    { PUSH int 3600 ; PUSH timestamp 1672531200 ; SUB } |}]

let%expect_test "sub mutez" =
  let e = sub (mutez 2000) (mutez 500) in
  test_expr e;
  [%expect {|
    { PUSH mutez 500 ; PUSH mutez 2000 ; SUB_MUTEZ }

    Optimised:
    { PUSH mutez 500 ; PUSH mutez 2000 ; SUB_MUTEZ } |}]

let%expect_test "sub_mutez direct" =
  let e = sub_mutez (mutez 2000) (mutez 1999) in
  test_expr e;
  [%expect {|
    { PUSH mutez 1999 ; PUSH mutez 2000 ; SUB_MUTEZ }

    Optimised:
    { PUSH mutez 1999 ; PUSH mutez 2000 ; SUB_MUTEZ } |}]

let%expect_test "lsr nat" =
  let e = lsr_ (nat 16) (nat 1) in
  test_expr e;
  [%expect {|
    { PUSH nat 1 ; PUSH nat 16 ; LSR }

    Optimised:
    { PUSH nat 1 ; PUSH nat 16 ; LSR } |}]

let%expect_test "lsl nat" =
  let e = lsl_ (nat 4) (nat 2) in
  test_expr e;
  [%expect {|
    { PUSH nat 2 ; PUSH nat 4 ; LSL }

    Optimised:
    { PUSH nat 2 ; PUSH nat 4 ; LSL } |}]

let%expect_test "xor on nat" =
  let e = xor (nat 0b1010) (nat 0b0011) in
  test_expr e;
  [%expect {|
    { PUSH nat 3 ; PUSH nat 10 ; XOR }

    Optimised:
    { PUSH nat 3 ; PUSH nat 10 ; XOR } |}]

let%expect_test "ediv nat nat" =
  let e = ediv (nat 10) (nat 3) in
  test_expr e;
  [%expect {|
    { PUSH nat 3 ; PUSH nat 10 ; EDIV }

    Optimised:
    { PUSH nat 3 ; PUSH nat 10 ; EDIV } |}]

let%expect_test "ediv mutez nat" =
  let e = ediv (mutez 1000) (nat 10) in
  test_expr e;
  [%expect {|
    { PUSH nat 10 ; PUSH mutez 1000 ; EDIV }

    Optimised:
    { PUSH nat 10 ; PUSH mutez 1000 ; EDIV } |}]

let%expect_test "div_ with if_none" =
  let e = div_ (int 10) (int 0) in
  test_expr e;
  [%expect {|
    { PUSH int 0 ;
      PUSH int 10 ;
      EDIV ;
      IF_NONE { PUSH string "DIV by 0" ; FAILWITH } { CAR } }

    Optimised:
    { PUSH int 0 ;
      PUSH int 10 ;
      EDIV ;
      IF_NONE { PUSH string "DIV by 0" ; FAILWITH } { CAR } } |}]

let%expect_test "mod_ with if_none" =
  let e = mod_ (nat 9) (nat 0) in
  test_expr e;
  [%expect {|
    { PUSH nat 0 ;
      PUSH nat 9 ;
      EDIV ;
      IF_NONE { PUSH string "MOD by 0" ; FAILWITH } { CDR } }

    Optimised:
    { PUSH nat 0 ;
      PUSH nat 9 ;
      EDIV ;
      IF_NONE { PUSH string "MOD by 0" ; FAILWITH } { CDR } } |}]

let%expect_test "and_ bool bool" =
  let e = and_ (bool true) (bool false) in
  test_expr e;
  [%expect {|
    { PUSH bool False ; PUSH bool True ; AND }

    Optimised:
    { PUSH bool False } |}]

let%expect_test "and_ int int" =
  let e = and_ (int 0xF0) (int 0xCC) in
  test_expr e;
  [%expect {|
    { PUSH int 204 ; PUSH int 240 ; AND }

    Optimised:
    { PUSH int 204 ; PUSH int 240 ; AND } |}]

let%expect_test "or_ bool bool" =
  let e = or_ (bool false) (bool false) in
  test_expr e;
  [%expect {|
    { PUSH bool False ; PUSH bool False ; OR }

    Optimised:
    { PUSH bool False } |}]

let%expect_test "or_ nat nat" =
  let e = or_ (nat 0b1010) (nat 0b0101) in
  test_expr e;
  [%expect {|
    { PUSH nat 5 ; PUSH nat 10 ; OR }

    Optimised:
    { PUSH nat 5 ; PUSH nat 10 ; OR } |}]

let%expect_test "cons to list" =
  let tail_list = cons (int 2) (cons (int 3) (nil int_ty)) in
  let e = cons (int 1) tail_list in
  test_expr e;
  [%expect {|
    { NIL int ; PUSH int 3 ; CONS ; PUSH int 2 ; CONS ; PUSH int 1 ; CONS }

    Optimised:
    { PUSH (list int) { 1 ; 2 ; 3 } } |}]

let%expect_test "concat1 strings" =
  let e = concat1 (string "Hello, ") (string "World!") in
  test_expr e;
  [%expect {|
    { PUSH string "World!" ; PUSH string "Hello, " ; CONCAT }

    Optimised:
    { PUSH string "World!" ; PUSH string "Hello, " ; CONCAT } |}]

let%expect_test "concat2 bytes" =
  let e = concat2 (bytes "0xABCD") (bytes "0x1234") in
  test_expr e;
  [%expect {|
    { NIL bytes ;
      PUSH bytes 0x307831323334 ;
      CONS ;
      PUSH bytes 0x307841424344 ;
      CONS ;
      CONCAT }

    Optimised:
    { PUSH (list bytes) { 0x307841424344 ; 0x307831323334 } ; CONCAT } |}]

let%expect_test "get from map" =
  let m = empty_map nat_ty bool_ty in
  let expr = get (nat 10) m in
  test_expr expr;
  [%expect {|
    { EMPTY_MAP nat bool ; PUSH nat 10 ; GET }

    Optimised:
    { EMPTY_MAP nat bool ; PUSH nat 10 ; GET } |}]

let%expect_test "get from big_map" =
  let bm = empty_bigmap string_ty int_ty in
  let expr = get (string "hello") bm in
  test_expr expr;
  [%expect {|
    { EMPTY_BIG_MAP string int ; PUSH string "hello" ; GET }

    Optimised:
    { EMPTY_BIG_MAP string int ; PUSH string "hello" ; GET } |}]

let%expect_test "mem in map" =
  let m = empty_map nat_ty bool_ty in
  let expr = mem (nat 999) m in
  test_expr expr;
  [%expect {|
    { EMPTY_MAP nat bool ; PUSH nat 999 ; MEM }

    Optimised:
    { EMPTY_MAP nat bool ; PUSH nat 999 ; MEM } |}]

let%expect_test "mem in big_map" =
  let bm = empty_bigmap nat_ty address_ty in
  let expr = mem (nat 5) bm in
  test_expr expr;
  [%expect {|
    { EMPTY_BIG_MAP nat address ; PUSH nat 5 ; MEM }

    Optimised:
    { EMPTY_BIG_MAP nat address ; PUSH nat 5 ; MEM } |}]

let%expect_test "mem in set" =
  let s = empty_set int_ty in
  let expr = mem (int 42) s in
  test_expr expr;
  [%expect {|
    { EMPTY_SET int ; PUSH int 42 ; MEM }

    Optimised:
    { EMPTY_SET int ; PUSH int 42 ; MEM } |}]

let%expect_test "exec function" =
  let lam = lambda (var "x", nat_ty) ~body:(add (variable (var "x") nat_ty) (nat 1)) in
  let expr = exec (nat 10) lam in
  test_expr expr;
  [%expect {|
    { LAMBDA nat nat { PUSH nat 1 ; SWAP ; ADD } ; PUSH nat 10 ; EXEC }

    Optimised:
    { LAMBDA nat nat { PUSH nat 1 ; ADD } ; PUSH nat 10 ; EXEC } |}]

let%expect_test "apply partial function" =
  let two_arg_fun =
    lambda (var "pair", mk_tuple_ty [int_ty; int_ty])
      ~body:(add
               (car (variable (var "pair") (mk_tuple_ty [int_ty; int_ty])))
               (cdr (variable (var "pair") (mk_tuple_ty [int_ty; int_ty]))))
  in
  let partial_applied = apply (int 5) two_arg_fun in
  (* partial_applied should have type (int -> int) *)
  let final_expr = exec (int 7) partial_applied in
  test_expr final_expr;
  [%expect {|
    { LAMBDA (pair int int) int { DUP 1 ; CDR ; SWAP ; CAR ; ADD } ;
      PUSH int 5 ;
      APPLY ;
      PUSH int 7 ;
      EXEC }

    Optimised:
    { LAMBDA (pair int int) int { UNPAIR ; ADD } ;
      PUSH int 5 ;
      APPLY ;
      PUSH int 7 ;
      EXEC } |}]

let%expect_test "sapling_verify_update correct types" =
  let transaction =
    (* For demonstration, we create a dummy expr with type Sapling_transaction 8. *)
    cast (sapling_transaction_ty 8)
      (bytes "0xDEADBEEF") 
    (* This is simplistic; real code would produce an actual sapling tx. *)
  in
  let state =
    sapling_empty_state 8
  in
  let expr = sapling_verify_update transaction state in
  test_expr expr;
  [%expect {|
    { SAPLING_EMPTY_STATE 8 ;
      PUSH bytes 0x30784445414442454546 ;
      CAST (sapling_transaction 8) ;
      SAPLING_VERIFY_UPDATE }

    Optimised:
    { SAPLING_EMPTY_STATE 8 ;
      PUSH bytes 0x30784445414442454546 ;
      CAST (sapling_transaction 8) ;
      SAPLING_VERIFY_UPDATE }
    ms: 8
    ms: 8 |}]

let%expect_test "ticket creation" =
  let expr = ticket (string "TicketContent") (nat 3) in
  test_expr expr;
  [%expect {|
    { PUSH nat 3 ; PUSH string "TicketContent" ; TICKET }

    Optimised:
    { PUSH nat 3 ; PUSH string "TicketContent" ; TICKET } |}]

let%expect_test "ticket_deprecated usage" =
  let expr = ticket_deprecated (int 42) (nat 2) in
  test_expr expr;
  [%expect {|
    { PUSH nat 2 ; PUSH int 42 ; TICKET_DEPRECATED }

    Optimised:
    { PUSH nat 2 ; PUSH int 42 ; TICKET_DEPRECATED } |}]

let%expect_test "ticket then read_ticket" =
  let maybe_ticket = ticket (string "content") (nat 10) in
  let unwrapped =
    if_none maybe_ticket
      ~none:(failwith (string "No ticket"))
      ~some:{
        lam_var = (var "tk", ticket_ty string_ty);
        body = variable (var "tk") (ticket_ty string_ty)
      }
  in
  let expr = read_ticket unwrapped in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      PUSH string "content" ;
      TICKET ;
      IF_NONE { PUSH string "No ticket" ; FAILWITH } {} ;
      { READ_TICKET ; PAIR } }

    Optimised:
    { PUSH nat 10 ;
      PUSH string "content" ;
      TICKET ;
      IF_NONE { PUSH string "No ticket" ; FAILWITH } {} ;
      READ_TICKET ;
      PAIR } |}]

let%expect_test "split_ticket usage" =
  let maybe_ticket = ticket (nat 123) (nat 10) in
  let unwrapped =
    if_none maybe_ticket
      ~none:(failwith (string "No ticket"))
      ~some:{
        lam_var = (var "tk", ticket_ty nat_ty);
        body = variable (var "tk") (ticket_ty nat_ty)
      }
  in
  let amounts = pair (None, None) (nat 3) (nat 7) in
  let expr = split_ticket unwrapped amounts in
  test_expr expr;
  [%expect {|
    { PUSH nat 7 ;
      PUSH nat 3 ;
      PAIR ;
      PUSH nat 10 ;
      PUSH nat 123 ;
      TICKET ;
      IF_NONE { PUSH string "No ticket" ; FAILWITH } {} ;
      SPLIT_TICKET }

    Optimised:
    { PUSH nat 7 ;
      PUSH nat 3 ;
      PAIR ;
      PUSH nat 10 ;
      PUSH nat 123 ;
      TICKET ;
      IF_NONE { PUSH string "No ticket" ; FAILWITH } {} ;
      SPLIT_TICKET } |}]

(*
  Commented out as updaten is not implemented.
  let%expect_test "updaten usage" =
  let pair_expr = pair (None,None) (int 5) (bool true) in
  let expr = updaten 1 (string "replacement") pair_expr in
  test_expr expr;
  [%expect {||}]*)

let%expect_test "view usage" =
  let expr =
    view
      "myView"
      ~return_type:nat_ty
      ~d:(int 7)
      ~address:(address_const "KT1SampleAddress")
  in
  test_expr expr;
  [%expect {|
    { PUSH address "KT1SampleAddress" ; PUSH int 7 ; VIEW "myView" nat }

    Optimised:
    { PUSH address "KT1SampleAddress" ; PUSH int 7 ; VIEW "myView" nat } |}]

let%expect_test "slice on string" =
  let off = nat 1 in
  let len = nat 3 in
  let s = string "Hello" in
  let expr = slice off ~length:len ~seq:s in
  test_expr expr;
  [%expect {|
    { PUSH string "Hello" ; PUSH nat 3 ; PUSH nat 1 ; SLICE }

    Optimised:
    { PUSH string "Hello" ; PUSH nat 3 ; PUSH nat 1 ; SLICE } |}]

let%expect_test "slice on bytes" =
  let off = nat 2 in
  let len = nat 4 in
  let b = bytes "0xDEADBEEF" in
  let expr = slice off ~length:len ~seq:b in
  test_expr expr;
  [%expect {|
    { PUSH bytes 0x30784445414442454546 ; PUSH nat 4 ; PUSH nat 2 ; SLICE }

    Optimised:
    { PUSH bytes 0x30784445414442454546 ; PUSH nat 4 ; PUSH nat 2 ; SLICE } |}]

let%expect_test "update in a map" =
  let m = empty_map nat_ty int_ty in
  let expr = update (nat 7) (some (int 100)) ~of_:m in
  test_expr expr;
  [%expect {|
    { EMPTY_MAP nat int ; PUSH int 100 ; SOME ; PUSH nat 7 ; UPDATE }

    Optimised:
    { PUSH (map nat int) { Elt 7 100 } } |}]

let%expect_test "update remove key in a map" =
  let m = empty_map string_ty bool_ty in
  let expr = update (string "test") (none bool_ty) ~of_:m in
  test_expr expr;
  [%expect {|
    { EMPTY_MAP string bool ; NONE bool ; PUSH string "test" ; UPDATE }

    Optimised:
    { EMPTY_MAP string bool ; NONE bool ; PUSH string "test" ; UPDATE } |}]

let%expect_test "update in a set" =
  let s = empty_set int_ty in
  (* For sets, value is bool. True => add, False => remove. *)
  let expr = update (int 3) (bool true) ~of_:s in
  test_expr expr;
  [%expect {|
    { EMPTY_SET int ; PUSH bool True ; PUSH int 3 ; UPDATE }

    Optimised:
    { PUSH (set int) { 3 } } |}]

let%expect_test "get_and_update in map" =
  let m = empty_map bool_ty nat_ty in
  let expr = get_and_update (bool false) (some (nat 42)) ~of_:m in
  test_expr expr;
  [%expect {|
    { EMPTY_MAP bool nat ;
      PUSH nat 42 ;
      SOME ;
      PUSH bool False ;
      { GET_AND_UPDATE ; PAIR } }

    Optimised:
    { EMPTY_MAP bool nat ;
      PUSH nat 42 ;
      SOME ;
      PUSH bool False ;
      GET_AND_UPDATE ;
      PAIR } |}]

let%expect_test "get_and_update removing from big_map" =
  let bm = empty_bigmap nat_ty string_ty in
  let expr = get_and_update (nat 99) (none string_ty) ~of_:bm in
  test_expr expr;
  [%expect {|
    { EMPTY_BIG_MAP nat string ;
      NONE string ;
      PUSH nat 99 ;
      { GET_AND_UPDATE ; PAIR } }

    Optimised:
    { EMPTY_BIG_MAP nat string ;
      NONE string ;
      PUSH nat 99 ;
      GET_AND_UPDATE ;
      PAIR } |}]

let%expect_test "transfer_tokens simple" =
  let param = unit in
  let amt = mutez 1_000_000 in
  let c = contract (None, unit_ty) (address_const "KT1Example") in
  let expr = transfer_tokens param ~amount:amt ~contract:c in
  test_expr expr;
  [%expect {|
    { PUSH address "KT1Example" ;
      CONTRACT unit ;
      PUSH mutez 1000000 ;
      UNIT ;
      TRANSFER_TOKENS }

    Optimised:
    { PUSH address "KT1Example" ;
      CONTRACT unit ;
      PUSH mutez 1000000 ;
      UNIT ;
      TRANSFER_TOKENS } |}]

let%expect_test "check_signature usage" =
  let k = key "edpkExample" in
  let sig_ = signature "edsigExampleSig" in
  let msg = bytes "0xDEADBEEF" in
  let expr = check_signature k ~signature:sig_ ~message:msg in
  test_expr expr;
  [%expect {|
    { PUSH bytes 0x30784445414442454546 ;
      PUSH signature "edsigExampleSig" ;
      PUSH key "edpkExample" ;
      CHECK_SIGNATURE }

    Optimised:
    { PUSH bytes 0x30784445414442454546 ;
      PUSH signature "edsigExampleSig" ;
      PUSH key "edpkExample" ;
      CHECK_SIGNATURE } |}]

let%expect_test "open_chest usage" =
  let chest_key_expr = bytes "0xAB" in
  let chest_expr = bytes "0xCD" in
  let time_expr = nat 100 in
  let expr = open_chest chest_key_expr ~chest:chest_expr ~time:time_expr in
  test_expr expr;
  [%expect {|
    { PUSH nat 100 ;
      PUSH bytes 0x30784344 ;
      PUSH bytes 0x30784142 ;
      OPEN_CHEST }

    Optimised:
    { PUSH nat 100 ;
      PUSH bytes 0x30784344 ;
      PUSH bytes 0x30784142 ;
      OPEN_CHEST } |}]

let%expect_test "convert_list usage" =
  let lst_exprs = [int 1; int 2; int 3] in
  let row = convert_list lst_exprs in
  let tuple_expr =
    tuple (row)
  in
  test_expr tuple_expr;
  [%expect {|
    { PUSH int 3 ; PUSH int 2 ; PUSH int 1 ; PAIR 3 }

    Optimised:
    { PUSH int 3 ; PUSH int 2 ; PUSH int 1 ; PAIR 3 } |}]

let%expect_test "gen_name usage" =
  let x = gen_name in
  let expr =
    let_in (Var x) ~rhs:(nat 10)
      ~in_:(variable (Var x) nat_ty)
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 }

    Optimised:
    { PUSH nat 10 } |}]

let%expect_test "global_constant usage" =
  let hash_val = "expruExampleHash" in
  let args = [nat 42; bool false] in
  let val_ty = int_ty in
  let expr = global_constant hash_val args val_ty in
  test_expr expr;
  [%expect {|
    { PUSH bool False ; PUSH nat 42 ; constant "expruExampleHash" }

    Optimised:
    { PUSH bool False ; PUSH nat 42 ; constant "expruExampleHash" } |}]

let%expect_test "cast int->nat with negative int" =
  let negative_int = int (-10) in
  let expr = cast nat_ty negative_int in
  test_expr expr;
  [%expect {|
    { PUSH int -10 ; CAST nat }

    Optimised:
    { PUSH int -10 ; CAST nat } |}]

let%expect_test "lambda -> lambda -> lambda" =
  let inner =
    lambda (var "z", bool_ty)
      ~body:(
        if_bool (variable (var "z") bool_ty)
          ~then_:(add (variable (var "x") int_ty) (variable (var "y") nat_ty))
          ~else_:(sub (variable (var "x") int_ty) (variable (var "y") nat_ty))
      )
  in
  let mid =
    lambda (var "y", nat_ty)
      ~body:inner
  in
  let outer =
    lambda (var "x", int_ty)
      ~body:mid
  in
  test_expr outer;
  [%expect {|
    { LAMBDA
        int
        (lambda nat (lambda bool int))
        { LAMBDA
            (pair int nat)
            (lambda bool int)
            { UNPAIR ;
              LAMBDA
                (pair int (pair nat bool))
                int
                { UNPAIR 3 ; DIG 2 ; IF { SWAP ; SWAP ; ADD } { SWAP ; SWAP ; SUB } } ;
              SWAP ;
              APPLY ;
              SWAP ;
              APPLY } ;
          SWAP ;
          APPLY } }

    Optimised:
    { LAMBDA
        int
        (lambda nat (lambda bool int))
        { LAMBDA
            (pair int nat)
            (lambda bool int)
            { UNPAIR ;
              LAMBDA
                (pair int (pair nat bool))
                int
                { UNPAIR 3 ; DIG 2 ; IF { ADD } { SUB } } ;
              SWAP ;
              APPLY ;
              SWAP ;
              APPLY } ;
          SWAP ;
          APPLY } } |}]

let%expect_test "apply function with 3-tuple param" =
  let three_tuple_ty =
    mk_tuple_ty [int_ty; mk_tuple_ty [int_ty; int_ty]]
  in
  let three_tuple_fun =
    lambda (var "p", three_tuple_ty)
      ~body:(
        add
          (car (variable (var "p") three_tuple_ty))
          (add
             (car (cdr (variable (var "p") three_tuple_ty)))
             (cdr (cdr (variable (var "p") three_tuple_ty))))
      )
  in
  let partial_applied =
    apply (int 2) three_tuple_fun
  in
  let final_expr =
    exec
      (tuple (mk_row [Leaf(None, int 3); Leaf(None, int 4)]))
      partial_applied
  in
  test_expr final_expr;
  [%expect{|
    { LAMBDA
        (pair int (pair int int))
        int
        { DUP 1 ; CDR ; CDR ; DUP 2 ; CDR ; CAR ; ADD ; SWAP ; CAR ; ADD } ;
      PUSH int 2 ;
      APPLY ;
      PUSH int 4 ;
      PUSH int 3 ;
      PAIR ;
      EXEC }

    Optimised:
    { LAMBDA
        (pair int (pair int int))
        int
        { DUP ; GET 4 ; DUP 2 ; GET 3 ; ADD ; SWAP ; CAR ; ADD } ;
      PUSH int 2 ;
      APPLY ;
      PUSH int 4 ;
      PUSH int 3 ;
      PAIR ;
      EXEC } |}]

let%expect_test "apply leftover function unused" =
  let two_tuple_ty = mk_tuple_ty [nat_ty; nat_ty] in
  let two_tuple_fun =
    lambda (var "p", two_tuple_ty)
      ~body:(
        add
          (car (variable (var "p") two_tuple_ty))
          (cdr (variable (var "p") two_tuple_ty))
      )
  in
  let partial_applied = apply (nat 8) two_tuple_fun in
  test_expr partial_applied;
  [%expect {|
    { LAMBDA (pair nat nat) nat { DUP 1 ; CDR ; SWAP ; CAR ; ADD } ;
      PUSH nat 8 ;
      APPLY }

    Optimised:
    { LAMBDA (pair nat nat) nat { UNPAIR ; ADD } ; PUSH nat 8 ; APPLY } |}]

let%expect_test "failwith usage in else branch" =
  let condition = eq (string "ok") (string "fail") in
  let expr =
    if_bool condition
      ~then_:(nat 1)
      ~else_:(failwith (concat1 (string "Error: ") (string "String mismatch")))
  in
  test_expr expr;
  [%expect {|
    { PUSH string "fail" ;
      PUSH string "ok" ;
      COMPARE ;
      EQ ;
      IF { PUSH nat 1 }
         { PUSH string "String mismatch" ; PUSH string "Error: " ; CONCAT ; FAILWITH } }

    Optimised:
    { PUSH string "fail" ;
      PUSH string "ok" ;
      COMPARE ;
      EQ ;
      IF {}
         { PUSH string "String mismatch" ; PUSH string "Error: " ; CONCAT ; FAILWITH } ;
      PUSH nat 1 } |}]

let%expect_test "update big_map of nat->contract(bool)" =
  let bigm =
    empty_bigmap nat_ty (contract_ty bool_ty)
  in
  let contr = some (contract (None, bool_ty) (address_const "KT1Test")) in
  let expr = update (nat 10) contr ~of_:bigm in
  test_expr expr;
  [%expect {|
    { EMPTY_BIG_MAP nat (contract bool) ;
      PUSH address "KT1Test" ;
      CONTRACT bool ;
      SOME ;
      PUSH nat 10 ;
      UPDATE }

    Optimised:
    { EMPTY_BIG_MAP nat (contract bool) ;
      PUSH address "KT1Test" ;
      CONTRACT bool ;
      SOME ;
      PUSH nat 10 ;
      UPDATE } |}]

let%expect_test "chained cast" =
  let e =
    cast int_ty (
      cast nat_ty (
        cast int_ty (nat 99)
      )
    )
  in
  test_expr e;
  [%expect {|
    { PUSH nat 99 ; CAST int ; CAST nat ; CAST int }

    Optimised:
    { PUSH nat 99 ; CAST int ; CAST nat ; CAST int } |}]

let%expect_test "global_constant returning function" =
  let hash_val = "expruCustomHash" in
  let args = [nat 2; int (-3)] in
  let val_ty = function_ty nat_ty int_ty in
  let expr = global_constant hash_val args val_ty in
  test_expr expr;
  [%expect {|
    { PUSH int -3 ; PUSH nat 2 ; constant "expruCustomHash" }

    Optimised:
    { PUSH int -3 ; PUSH nat 2 ; constant "expruCustomHash" } |}]

let%expect_test "apply global constant function" =
  let val_ty = function_ty nat_ty int_ty in
  let gc_expr = global_constant "expruFuncHash" [] val_ty in
  let expr = app gc_expr (nat 100) in
  test_expr expr;
  [%expect {|
    { PUSH nat 100 ;
      LAMBDA nat int { constant "expruFuncHash" } ;
      SWAP ;
      EXEC }

    Optimised:
    { PUSH nat 100 ; constant "expruFuncHash" } |}]

let%expect_test "lambda capturing external let var" =
  let expr =
    let_in (var "x") ~rhs:(nat 10)
      ~in_:(
        app
          (lambda (var "y", nat_ty)
             ~body:(add (variable (var "x") nat_ty) (variable (var "y") nat_ty)))
          (nat 5)
      )
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      PUSH nat 5 ;
      LAMBDA (pair nat nat) nat { UNPAIR ; SWAP ; SWAP ; ADD } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      EXEC }

    Optimised:
    { PUSH nat 10 ;
      PUSH nat 5 ;
      LAMBDA (pair nat nat) nat { UNPAIR ; ADD } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      EXEC } |}]

let%expect_test "nested lambdas referencing outer variable" =
  let expr =
    let_in (var "a") ~rhs:(nat 2)
      ~in_:
        (let_in (var "b") ~rhs:(nat 3)
          ~in_:
            (lambda (var "x", nat_ty)
              ~body:(
                lambda (var "y", nat_ty)
                  ~body:(
                    add
                      (add (variable (var "x") nat_ty) (variable (var "y") nat_ty))
                      (add (variable (var "a") nat_ty) (variable (var "b") nat_ty))
                  ))
              ))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 2 ;
      PUSH nat 3 ;
      LAMBDA
        (pair nat (pair nat nat))
        (lambda nat nat)
        { UNPAIR 3 ;
          LAMBDA
            (pair nat (pair nat (pair nat nat)))
            nat
            { UNPAIR 4 ; SWAP ; SWAP ; ADD ; DIG 2 ; DIG 2 ; ADD ; ADD } ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      APPLY }

    Optimised:
    { PUSH nat 2 ;
      PUSH nat 3 ;
      LAMBDA
        (pair nat (pair nat nat))
        (lambda nat nat)
        { UNPAIR 3 ;
          LAMBDA
            (pair nat (pair nat (pair nat nat)))
            nat
            { UNPAIR 4 ; ADD ; DUG 2 ; ADD ; ADD } ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY } ;
      DIG 2 ;
      APPLY ;
      SWAP ;
      APPLY } |}]

let%expect_test "nested lambdas returning another referencing an external var" =
  let expr =
    let_in (var "factor") ~rhs:(nat 10)
      ~in_:
        (lambda (var "x", nat_ty)
          ~body:(
            lambda (var "y", nat_ty)
              ~body:(
                mul
                  (mul (variable (var "x") nat_ty) (variable (var "y") nat_ty))
                  (variable (var "factor") nat_ty)
              )
          ))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      LAMBDA
        (pair nat nat)
        (lambda nat nat)
        { UNPAIR ;
          LAMBDA (pair nat (pair nat nat)) nat { UNPAIR 3 ; DIG 2 ; DIG 2 ; MUL ; MUL } ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY } ;
      SWAP ;
      APPLY }

    Optimised:
    { LAMBDA
        (pair nat nat)
        (lambda nat nat)
        { UNPAIR ;
          LAMBDA (pair nat (pair nat nat)) nat { UNPAIR 3 ; DUG 2 ; MUL ; MUL } ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY } ;
      PUSH nat 10 ;
      APPLY } |}]

let%expect_test "partial application capturing environment" =
  let pair_ty = mk_tuple_ty [nat_ty; nat_ty] in
  let lam_f =
    lambda (var "p", pair_ty)
      ~body:(
        add
          (variable (var "a") nat_ty)
          (add
             (car (variable (var "p") pair_ty))
             (cdr (variable (var "p") pair_ty)))
      )
  in
  let expr =
    let_in (var "a") ~rhs:(nat 42)
      ~in_:
        (let_in (var "f") ~rhs:lam_f
          ~in_:
            (let_in (var "applied")
              ~rhs:(apply (nat 5) (variable (var "f") (function_ty pair_ty int_ty)))
              ~in_:
                (exec (nat 7) (variable (var "applied") (function_ty nat_ty int_ty)))))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 42 ;
      LAMBDA
        (pair nat (pair nat nat))
        nat
        { UNPAIR ; DUP 2 ; CDR ; DIG 2 ; CAR ; ADD ; SWAP ; ADD } ;
      SWAP ;
      APPLY ;
      PUSH nat 5 ;
      APPLY ;
      PUSH nat 7 ;
      EXEC }

    Optimised:
    { LAMBDA (pair nat (pair nat nat)) nat { UNPAIR ; SWAP ; UNPAIR ; ADD ; ADD } ;
      PUSH nat 42 ;
      APPLY ;
      PUSH nat 5 ;
      APPLY ;
      PUSH nat 7 ;
      EXEC } |}]

let%expect_test "inner param overshadowing outer var name" =
  let expr =
    let_in (var "z") ~rhs:(nat 9)
      ~in_:
        (lambda (var "z", nat_ty)
          ~body:(add (variable (var "z") nat_ty) (nat 1)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 9 ; LAMBDA nat nat { PUSH nat 1 ; SWAP ; ADD } ; SWAP ; DROP }

    Optimised:
    { LAMBDA nat nat { PUSH nat 1 ; ADD } } |}]

let%expect_test "lambda capturing a mut var read" =
  let mut_m = mut_var "m" in
  let expr =
    let_mut_in mut_m ~rhs:(nat 10)
      ~in_:
        (lambda (var "x", nat_ty)
          ~body:(add (variable (var "x") nat_ty) (deref mut_m nat_ty)))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 10 ;
      LAMBDA (pair nat nat) nat { UNPAIR ; SWAP ; ADD } ;
      SWAP ;
      APPLY }

    Optimised:
    { LAMBDA (pair nat nat) nat { UNPAIR ; ADD } ; PUSH nat 10 ; APPLY } |}]

let%expect_test "lambda capturing mut var and assigning" =
  let mut_c = mut_var "c" in
  let expr =
    let_mut_in mut_c ~rhs:(nat 0)
      ~in_:
        (lambda (var "inc", nat_ty)
          ~body:(
            let_in (var "ignore_assign")
              ~rhs:(assign mut_c (add (deref mut_c nat_ty) (variable (var "inc") nat_ty)))
              ~in_:(deref mut_c nat_ty)
          ))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 0 ;
      LAMBDA
        (pair nat nat)
        nat
        { UNPAIR ; SWAP ; DUP 2 ; ADD ; DUG 1 ; DIG 0 ; DROP ; UNIT ; DROP } ;
      SWAP ;
      APPLY }

    Optimised:
    { LAMBDA (pair nat nat) nat { UNPAIR ; ADD } ; PUSH nat 0 ; APPLY } |}]

let%expect_test "partial apply nested lambda env capture" =
  let param_ty = mk_tuple_ty [int_ty; int_ty] in
  let outer_lam =
    lambda (var "p", param_ty)
      ~body:(
        let_in (var "sum") ~rhs:(add
                                   (add (car (variable (var "p") param_ty))
                                        (cdr (variable (var "p") param_ty)))
                                   (variable (var "a") nat_ty))
          ~in_:
            (lambda (var "leftover", int_ty)
              ~body:(add (variable (var "sum") int_ty) (variable (var "leftover") int_ty)))
      )
  in
  let expr =
    let_in (var "a") ~rhs:(nat 3)
      ~in_:
        (let_in (var "outer") ~rhs:outer_lam
          ~in_:
            (let_in (var "applied_outer")
              ~rhs:(app (variable (var "outer") (function_ty param_ty (function_ty int_ty int_ty)))
                        (tuple (mk_row [
                          Leaf(None, int 7);
                          Leaf(None, int 5)
                        ])))
              ~in_:
                ((app (variable (var "applied_outer") (function_ty int_ty int_ty)) (int 10)))))
  in
  test_expr expr;
  [%expect {|
    { PUSH nat 3 ;
      LAMBDA
        (pair nat (pair int int))
        (lambda int int)
        { UNPAIR ;
          DUP 2 ;
          CDR ;
          DIG 2 ;
          CAR ;
          ADD ;
          ADD ;
          LAMBDA (pair int int) int { UNPAIR ; SWAP ; SWAP ; ADD } ;
          SWAP ;
          APPLY } ;
      SWAP ;
      APPLY ;
      PUSH int 5 ;
      PUSH int 7 ;
      PAIR ;
      SWAP ;
      SWAP ;
      EXEC ;
      PUSH int 10 ;
      SWAP ;
      SWAP ;
      EXEC }

    Optimised:
    { LAMBDA
        (pair nat (pair int int))
        (lambda int int)
        { UNPAIR ;
          SWAP ;
          UNPAIR ;
          ADD ;
          ADD ;
          LAMBDA (pair int int) int { UNPAIR ; ADD } ;
          SWAP ;
          APPLY } ;
      PUSH nat 3 ;
      APPLY ;
      PUSH int 5 ;
      PUSH int 7 ;
      PAIR ;
      EXEC ;
      PUSH int 10 ;
      EXEC } |}]

let%expect_test "return environment capturing function unused" =
  let expr =
    let_in (var "q") ~rhs:(int 123)
      ~in_:
        (lambda (var "x", int_ty)
          ~body:(
            lambda (var "y", int_ty)
              ~body:(add (add (variable (var "x") int_ty) (variable (var "y") int_ty))
                           (variable (var "q") int_ty))
          ))
  in
  test_expr expr;
  [%expect {|
    { PUSH int 123 ;
      LAMBDA
        (pair int int)
        (lambda int int)
        { UNPAIR ;
          LAMBDA (pair int (pair int int)) int { UNPAIR 3 ; DIG 2 ; DIG 2 ; ADD ; ADD } ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY } ;
      SWAP ;
      APPLY }

    Optimised:
    { LAMBDA
        (pair int int)
        (lambda int int)
        { UNPAIR ;
          LAMBDA (pair int (pair int int)) int { UNPAIR 3 ; DUG 2 ; ADD ; ADD } ;
          SWAP ;
          APPLY ;
          SWAP ;
          APPLY } ;
      PUSH int 123 ;
      APPLY } |}]

let%expect_test "deeper let_in overshadowing environment var" =
  let expr =
    let_in (var "x") ~rhs:(int 1)
      ~in_:
        (lambda (var "y", int_ty)
          ~body:(
            let_in (var "x")
              ~rhs:(add (variable (var "y") int_ty) (variable (var "x") int_ty))
              ~in_:(add (variable (var "x") int_ty) (int 100))
          ))
  in
  test_expr expr;
  [%expect {|
    { PUSH int 1 ;
      LAMBDA
        (pair int int)
        int
        { UNPAIR ; DUP 1 ; DIG 2 ; ADD ; PUSH int 100 ; SWAP ; ADD ; SWAP ; DROP } ;
      SWAP ;
      APPLY }

    Optimised:
    { LAMBDA (pair int int) int { UNPAIR ; ADD ; PUSH int 100 ; ADD } ;
      PUSH int 1 ;
      APPLY } |}]