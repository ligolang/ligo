open Core
open Lltz_ir.Ast_builder.With_dummy
module FV = Lltz_ir.Free_vars
module E = Lltz_ir.Ast_builder.With_dummy
module LM = Lltz_codegen
module Tezos_micheline = Tezos_micheline 
module Ast_builder = Lltz_ir.Ast_builder

let print_map_str_ty map =
  Map.to_alist map
  |> List.map ~f:(fun (k,v) ->
       Printf.sprintf "%s : %s"
         k
         (Sexplib.Sexp.to_string_hum (Lltz_ir.Type.sexp_of_t v)))
  |> String.concat ~sep:", "

let print_free_vars expr =
  let fv_map = FV.free_vars_with_types expr in
  print_string (print_map_str_ty fv_map);
  Format.print_newline ()

let%expect_test "fv simple var + constant (no free vars)" =
  let expr =
    let_in (var "x") ~rhs:(nat 10)
      ~in_:(add (variable (var "x") nat_ty) (int (-5)))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv simple var + constant (one free var)" =
  let expr =
    add (variable (var "x") nat_ty) (int 5)
  in
  print_free_vars expr;
  [%expect {|
    x : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv overshadowed variable (no free var)" =
  let expr =
    let_in (var "z") ~rhs:(int 5)
      ~in_:(let_in (var "z") ~rhs:(nat 99)
              ~in_:(add (variable (var "z") nat_ty) (variable (var "z") nat_ty)))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv overshadowed variable (one free var)" =
  let expr =
    let_in (var "z") ~rhs:(int 5)
      ~in_:(
        add (variable (var "z") nat_ty) (variable (var "unboundZ") int_ty)
      )
  in
  print_free_vars expr;
  [%expect {|
    unboundZ : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv lambda capturing outer var (no free var)" =
  let expr =
    let_in (var "a") ~rhs:(nat 10)
      ~in_:
        (lambda (var "x", nat_ty)
           ~body:(add (variable (var "x") nat_ty)
                       (variable (var "a") nat_ty)))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv lambda referencing free var (one free var)" =
  let expr =
    lambda (var "x", nat_ty)
      ~body:(add (variable (var "x") nat_ty) (variable (var "b") nat_ty))
  in
  print_free_vars expr;
  [%expect {|
    b : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv lambda_rec no free var" =
  let expr =
    lambda_rec (var "fact")
      (var "n", nat_ty)
      ~body:(
        if_bool (le (variable (var "n") nat_ty) (nat 1))
          ~then_:(nat 1)
          ~else_:(mul (variable (var "n") nat_ty)
                      (app (variable (var "fact") (function_ty nat_ty nat_ty))
                           (sub (variable (var "n") nat_ty) (nat 1)))))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv lambda_rec referencing a free var" =
  let expr =
    lambda_rec (var "f") (var "n", int_ty)
      ~body:(
        if_bool
          (le (variable (var "n") int_ty) (int 0))
          ~then_:(variable (var "outer_val") int_ty)
          ~else_:(app (variable (var "f") (function_ty int_ty int_ty))
                     (sub (variable (var "n") int_ty) (int 1)))
      )
  in
  print_free_vars expr;
  [%expect {|
    outer_val : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

  let%expect_test "fv app no free var" =
  let lam_expr =
    lambda (var "x", nat_ty)
      ~body:(add (variable (var "x") nat_ty) (nat 1))
  in
  let expr =
    let_in (var "f") ~rhs:lam_expr
      ~in_:(app (variable (var "f") (function_ty nat_ty nat_ty)) (nat 42))
  in
  print_free_vars expr;
  [%expect {||}]

let%expect_test "fv app with free var in abs" =
  let lam_expr =
    lambda (var "x", nat_ty)
      ~body:(add (variable (var "x") nat_ty) (variable (var "glo") nat_ty))
  in
  let expr = app lam_expr (nat 100) in
  print_free_vars expr;
  [%expect {|
    glo : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv app with free var in arg" =
  let lam_expr =
    lambda (var "x", int_ty)
      ~body:(mul (variable (var "x") int_ty) (int 2))
  in
  let expr = app lam_expr (variable (var "argFree") int_ty) in
  print_free_vars expr;
  [%expect {|
    argFree : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv const no free var" =
  let expr = int 42 in
  print_free_vars expr;
  [%expect {||}]

let%expect_test "fv prim no free var (e.g. Add of 2 constants)" =
  let expr = add (int 1) (int 2) in
  print_free_vars expr;
  [%expect {||}]

let%expect_test "fv prim with one free var in arg" =
  let expr = eq (variable (var "x") nat_ty) (nat 3) in
  print_free_vars expr;
  [%expect {|
    x : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv prim with multiple free vars in args" =
  let expr = add (variable (var "x") int_ty) (variable (var "y") int_ty) in
  print_free_vars expr;
  [%expect {|
    x : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content ""))))))), y : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv let_mut_in no free var" =
  let expr =
    let_mut_in (mut_var "m") ~rhs:(int 10)
      ~in_:(assign (mut_var "m") (add (deref (mut_var "m") int_ty) (int 1)))
  in
  print_free_vars expr;
  [%expect {||}]

let%expect_test "fv let_mut_in free var in body" =
  let expr =
    let_mut_in (mut_var "mm") ~rhs:(nat 1)
      ~in_:(assign (mut_var "mm")
              (add (deref (mut_var "mm") nat_ty)
                   (variable (var "z") nat_ty)))
  in
  print_free_vars expr;
  [%expect {|
    z : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv let_mut_in free var in rhs" =
  let expr =
    let_mut_in (mut_var "mm") ~rhs:(variable (var "x") int_ty)
      ~in_:(deref (mut_var "mm") int_ty)
  in
  print_free_vars expr;
  [%expect {|
    x : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv deref no free var" =
  let expr =
    let_mut_in (mut_var "mm") ~rhs:(nat 3)
      ~in_:(deref (mut_var "mm") nat_ty)
  in
  print_free_vars expr;
  [%expect {||}]

let%expect_test "fv deref is free" =
  let expr = deref (mut_var "mm") int_ty in
  print_free_vars expr;
  [%expect {|
    mm : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv mutable var assignment (one free var)" =
  let expr =
    let_mut_in (mut_var "m") ~rhs:(nat 99)
      ~in_:(assign (mut_var "m")
              (add (deref (mut_var "m") nat_ty)
                   (variable (var "x") nat_ty)))
  in
  print_free_vars expr;
  [%expect {|
    x : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv mutable var assignment no free var" =
  let expr =
    let_in (var "outer") ~rhs:(nat 2)
      ~in_:
        (let_mut_in (mut_var "m") ~rhs:(nat 99)
          ~in_:(assign (mut_var "m")
                  (add (deref (mut_var "m") nat_ty)
                       (variable (var "outer") nat_ty))))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv if_none capturing var in some branch (no free var)" =
  let expr =
    let_in (var "c") ~rhs:(nat 999)
      ~in_:
        (if_none (some (int 5))
          ~none:(nat 0)
          ~some:{
            lam_var = (var "k", int_ty);
            body = add (variable (var "k") int_ty) (variable (var "c") nat_ty)
          })
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv if_none referencing free var in none branch" =
  let expr =
    if_none (some (int 5))
      ~none:(add (int 0) (variable (var "unb") nat_ty))
      ~some:{
        lam_var=(var "k",int_ty);
        body=(int 999)
      }
  in
  print_free_vars expr;
  [%expect {|
    unb : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv if_cons referencing an unbound var" =
  let lst = cons (nat 2) (cons (nat 4) (nil nat_ty)) in
  let expr =
    if_cons lst
      ~empty:(int 0)
      ~nonempty:{
        lam_var1 = (var "hd", nat_ty);
        lam_var2 = (var "tl", list_ty nat_ty);
        body = add (variable (var "hd") nat_ty)
                   (variable (var "some_free_var") nat_ty)
      }
  in
  print_free_vars expr;
  [%expect {|
    some_free_var : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv if_cons no free var" =
  let lst = nil nat_ty in
  let expr =
    if_cons lst
      ~empty:(nat 999)
      ~nonempty:{
        lam_var1=(var "hd", nat_ty);
        lam_var2=(var "tl", list_ty nat_ty);
        body=add (variable (var "hd") nat_ty) (nat 1)
      }
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv if_left overshadow var (no free var)" =
  let left_expr = left (None, None, bool_ty) (bool true) in
  let expr =
    if_left left_expr
      ~left:{
        lam_var = (var "flag", bool_ty);
        body = let_in (var "flag") ~rhs:(bool false)
                ~in_:(variable (var "flag") bool_ty)
      }
      ~right:{
        lam_var = (var "x", bool_ty);
        body = bool false
      }
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv if_left right branch free var" =
  let left_expr = left (None, None, bool_ty) (bool true) in
  let expr =
    if_left left_expr
      ~left:{
        lam_var = (var "flag", bool_ty);
        body = bool true
      }
      ~right:{
        lam_var = (var "x", bool_ty);
        body = add (nat 0) (variable (var "unbound_ifleft") int_ty)
      }
  in
  print_free_vars expr;
  [%expect {|
    unbound_ifleft : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv if_bool same free var in both branches" =
  let condition = gt (variable (var "x") int_ty) (int 0) in
  let expr =
    if_bool condition
      ~then_:(add (variable (var "x") int_ty) (int 1))
      ~else_:(sub (variable (var "x") int_ty) (int 1))
  in
  print_free_vars expr;
  [%expect {|
    x : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv if_bool with distinct free vars in each branch" =
  let condition = bool true in
  let expr =
    if_bool condition
      ~then_:(add (variable (var "varA") nat_ty) (nat 10))
      ~else_:(mul (variable (var "varB") nat_ty) (nat 2))
  in
  print_free_vars expr;
  [%expect {|
    varA : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content ""))))))), varB : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv nested if_bool overshadow" =
  let expr =
    let_in (var "x") ~rhs:(int 1)
      ~in_:
        (if_bool (eq (variable (var "x") int_ty) (int 1))
          ~then_:
            (let_in (var "x") ~rhs:(int 2)
              ~in_:(add (variable (var "x") int_ty)
                         (variable (var "outer") int_ty)))
          ~else_:
            (add (variable (var "x") int_ty)
                (variable (var "outer2") int_ty)))
  in
  print_free_vars expr;
  [%expect {|
    outer : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content ""))))))), outer2 : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv for with multiple free vars" =
  let mut_x = mut_var "x" in
  let expr =
    let_mut_in mut_x ~rhs:(int 10)
      ~in_:
        (for_ (mut_var "i")
          ~init:(variable (var "freeVarInit") int_ty)
          ~cond:(lt (deref (mut_var "i") int_ty)
                    (variable (var "freeCond") int_ty))
          ~update:(assign (mut_var "i") (add (deref (mut_var "i") int_ty) (int 1)))
          ~body:(
            assign mut_x
              (add (deref mut_x int_ty)
                   (add (deref (mut_var "i") int_ty)
                        (variable (var "freeBody") int_ty)))
          ))
  in
  print_free_vars expr;
  [%expect {|
    freeBody : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content ""))))))), freeCond : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content ""))))))), freeVarInit : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv for no free vars" =
  let mut_x = mut_var "x" in
  let expr =
    let_mut_in mut_x ~rhs:(int 0)
      ~in_:
        (for_ (mut_var "i")
          ~init:(int 1)
          ~cond:(lt (deref (mut_var "i") int_ty) (int 3))
          ~update:(assign (mut_var "i") (add (deref (mut_var "i") int_ty) (int 1)))
          ~body:(assign mut_x (add (deref mut_x int_ty) (deref (mut_var "i") int_ty))))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv for_each referencing outer var" =
  let list_expr = cons (int 1) (cons (int 2) (nil int_ty)) in
  let expr =
    let_in (var "outer") ~rhs:(int 10)
      ~in_:
        (for_each list_expr ~body:{
          lam_var = (var "elem", int_ty);
          body = add (variable (var "elem") int_ty)
                     (variable (var "outer") int_ty)
        })
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv for_each body free var" =
  let list_expr = cons (int 1) (nil int_ty) in
  let expr =
    for_each list_expr ~body:{
      lam_var=(var "el", int_ty);
      body=add (variable (var "el") int_ty) (variable (var "unboundFe") int_ty)
    }
  in
  print_free_vars expr;
  [%expect {|
    unboundFe : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv map overshadowing var (no free var)" =
  let list_expr = cons (string "a") (cons (string "b") (nil string_ty)) in
  let expr =
    let_in (var "s") ~rhs:(string "outerS")
      ~in_:
        (map list_expr ~map:{
          lam_var = (var "s", string_ty);
          body = concat1 (variable (var "s") string_ty)
                         (variable (var "s") string_ty)
        })
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv map body free var" =
  let list_expr = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let expr =
    map list_expr ~map:{
      lam_var=(var "n", nat_ty);
      body=add (variable (var "n") nat_ty) (variable (var "unboundMap") nat_ty)
    }
  in
  print_free_vars expr;
  [%expect {|
    unboundMap : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv fold_right referencing an unbound var in fold body" =
  let lst = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let expr =
    fold_right lst ~init:(nat 0)
      ~fold:{
        lam_var = (var "elem_acc", tuple_ty (mk_row [
          Leaf(None, nat_ty); Leaf(None, nat_ty)
        ]));
        body = add
          (car (variable (var "elem_acc") (tuple_ty (mk_row [
            Leaf(None, nat_ty); Leaf(None, nat_ty)
          ]))))
          (add
             (cdr (variable (var "elem_acc") (tuple_ty (mk_row [
               Leaf(None, nat_ty); Leaf(None, nat_ty)
             ]))))
             (variable (var "unbound") nat_ty))
      }
  in
  print_free_vars expr;
  [%expect {|
    unbound : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv fold_right no free var" =
  let lst = cons (nat 1) (nil nat_ty) in
  let expr =
    fold_right lst ~init:(nat 0)
      ~fold:{
        lam_var=(var "pair", tuple_ty (mk_row [
          Leaf(None,nat_ty); Leaf(None,nat_ty)
        ]));
        body=add
          (car (variable (var "pair") (mk_tuple_ty [nat_ty; nat_ty])))
               (cdr (variable (var "pair") (mk_tuple_ty [nat_ty; nat_ty])))
      }
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv fold_left no free var" =
let coll = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
let expr =
  fold_left coll ~init:(nat 0)
    ~fold:{
      lam_var = (var "acc_x", tuple_ty (mk_row [
        Leaf(None, nat_ty);
        Leaf(None, nat_ty)
      ]));
      body = add
        (car (variable (var "acc_x") (tuple_ty (mk_row [
          Leaf(None, nat_ty); Leaf(None, nat_ty)
        ]))))
        (cdr (variable (var "acc_x") (tuple_ty (mk_row [
          Leaf(None, nat_ty); Leaf(None, nat_ty)
        ]))))
    }
in
print_free_vars expr;
[%expect {|
  |}]
  
let%expect_test "fv fold_left free var in body" =
  let coll = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let expr =
    fold_left coll ~init:(nat 0)
      ~fold:{
        lam_var = (var "acc_x", tuple_ty (mk_row [
          Leaf(None, nat_ty);
          Leaf(None, nat_ty)
        ]));
        body = add
          (add
             (car (variable (var "acc_x") (mk_tuple_ty [nat_ty; nat_ty])))
             (cdr (variable (var "acc_x") (mk_tuple_ty [nat_ty; nat_ty]))))
          (variable (var "freeFold") nat_ty)
      }
  in
  print_free_vars expr;
  [%expect {|
    freeFold : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv let_tuple_in overshadow (no free var)" =
  let triple =
    tuple (mk_row [
      Leaf(None, nat 1);
      Leaf(None, nat 2);
      Leaf(None, nat 3)
    ])
  in
  let expr =
    let_in (var "x") ~rhs:(nat 999)
      ~in_:
        (let_tuple_in [var "x"; var "y"; var "z"]
          ~rhs:triple
          ~in_:(add (variable (var "x") nat_ty) (variable (var "x") nat_ty)))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv let_tuple_in body free var" =
  let triple =
    tuple (mk_row [
      Leaf(None, int 10);
      Leaf(None, int 20);
      Leaf(None, int 30)
    ])
  in
  let expr =
    let_tuple_in [var "a"; var "b"; var "c"]
      ~rhs:triple
      ~in_:(add (variable (var "c") int_ty)
                (variable (var "notBound") int_ty))
  in
  print_free_vars expr;
  [%expect {|
    notBound : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv proj referencing free var" =
  let triple =
    tuple (mk_row [
      Leaf(None, nat 1);
      Leaf(None, nat 2);
      Leaf(None, nat 3)
    ])
  in
  let second_elem = proj triple ~path:(Here [1]) in
  let expr = add second_elem (variable (var "outer") nat_ty) in
  print_free_vars expr;
  [%expect {|
    outer : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv proj fully bound, no free var" =
  let triple =
    let_in (var "a") ~rhs:(nat 10)
      ~in_:
        (tuple (mk_row [
          Leaf(None, variable (var "a") nat_ty);
          Leaf(None, nat 11);
          Leaf(None, nat 12)
        ]))
  in
  let second_elem = proj triple ~path:(Here [1]) in
  let expr = add second_elem (nat 100) in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv create_contract referencing outer var (no free var leftover)" =
  let storage_ty = nat_ty in
  let param_storage_ty = mk_tuple_ty [nat_ty; storage_ty] in
  let code_lam =
    let_tuple_in [var "p"; var "s"]
      ~rhs:(variable (var "args") param_storage_ty)
      ~in_:(tuple (mk_row [
        Leaf(None, nil (operation_ty));
        Leaf(None, add (variable (var "p") nat_ty)
                       (variable (var "big_val") nat_ty))
      ]))
  in
  let expr =
    let_in (var "big_val") ~rhs:(nat 9999)
      ~in_:
        (create_contract
          ~storage:storage_ty
          ~code:{ lam_var=(var "args", param_storage_ty); body=code_lam }
          ~delegate:(none key_hash_ty)
          ~initial_balance:(mutez 1000)
          ~initial_storage:(nat 0))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv create_contract referencing unbound var in code" =
  let storage_ty = nat_ty in
  let param_storage_ty = mk_tuple_ty [int_ty; storage_ty] in
  let code_lam =
    let_tuple_in [var "p"; var "s"]
      ~rhs:(variable (var "args") param_storage_ty)
      ~in_:(tuple (mk_row [
        Leaf(None, nil (operation_ty));
        Leaf(None, add (variable (var "p") int_ty)
                       (variable (var "freeU") nat_ty))
      ]))
  in
  let expr =
    create_contract
      ~storage:storage_ty
      ~code:{ lam_var=(var "args", param_storage_ty); body=code_lam }
      ~delegate:(none key_hash_ty)
      ~initial_balance:(mutez 1000)
      ~initial_storage:(nat 0)
  in
  print_free_vars expr;
  [%expect {|
    freeU : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let michelson_push_int n = Tezos_micheline.Micheline.Prim (Ast_builder.Dummy_range.v, "PUSH", [Tezos_micheline.Micheline.Int (Ast_builder.Dummy_range.v, Z.of_int n)], [])
let%expect_test "fv raw_michelson no arguments" =
  let michelson_ast =
    Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [michelson_push_int 42])
  in
  let expr =
    raw_michelson michelson_ast [] (int_ty)
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv raw_michelson single free var in argument" =
  let michelson_ast =
    Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [michelson_push_int 100])
  in
  let expr =
    raw_michelson michelson_ast
      [variable (var "freeArg") int_ty]
      int_ty
  in
  print_free_vars expr;
  [%expect {|
    freeArg : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv raw_michelson mixed arguments" =
  let michelson_ast =
    Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [michelson_push_int 999])
  in
  let expr =
    let_in (var "x") ~rhs:(int 123)
      ~in_:
        (raw_michelson michelson_ast
          [
            variable (var "x") int_ty;
            variable (var "unboundVar") int_ty
          ]
          int_ty)
  in
  print_free_vars expr;
  [%expect {|
    unboundVar : ((desc Int)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]

let%expect_test "fv raw_michelson all bound arguments" =
  let michelson_ast =
    Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [michelson_push_int 1])
  in
  let expr =
    let_in (var "a") ~rhs:(int 10)
      ~in_:
        (let_in (var "b") ~rhs:(int 20)
          ~in_:
            (raw_michelson michelson_ast
              [
                variable (var "a") int_ty;
                variable (var "b") int_ty
              ]
              int_ty))
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv global_constant no free var" =
  let expr =
    global_constant "someHash" [int 10] int_ty
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv global_constant referencing outer var" =
  let expr =
    let_in (var "outer") ~rhs:(int 42)
      ~in_:
        (global_constant "expruHash" [variable (var "outer") int_ty] int_ty)
  in
  print_free_vars expr;
  [%expect {|
    |}]

let%expect_test "fv global_constant unbound var" =
  let expr =
    global_constant "hashX" [variable (var "unG") nat_ty; nat 9] nat_ty
  in
  print_free_vars expr;
  [%expect {|
    unG : ((desc Nat)
     (range ((start 0) (stop 0) (source (String ((name ("")) (content "")))))))
  |}]
