open Core
module Last_vars = Lltz_codegen.Last_vars
module Ast_builder = Lltz_ir.Ast_builder
open Lltz_ir.Ast_builder.With_dummy


let print_vars expr =
  let vars =Last_vars.collect_all_vars expr in
  vars
  |> Set.to_list
  |> String.concat ~sep:", "
  |> print_endline

let v_x_nat = var "x"
let v_y_nat = var "y"
let mut_m_nat = mut_var "m"

let%expect_test "vars simple overshadowing" =
  let expr =
    let_in (var "z") ~rhs:(int 5)
      ~in_:
        (let_in (var "z") ~rhs:(nat 10)
          ~in_:(add (variable (var "z") nat_ty) (variable (var "z") nat_ty)))
  in
  print_vars expr;
  [%expect {|
    z
  |}]

let%expect_test "vars lambda overshadow outer" =
  let expr =
    let_in (var "a") ~rhs:(nat 99)
      ~in_:
        (lambda (var "a", nat_ty)
          ~body:(
            add
              (variable (var "a") nat_ty)
              (variable (var "a") nat_ty)
          ))
  in
  print_vars expr;
  [%expect {|
    a
  |}]

let%expect_test "vars lambda_rec" =
  let expr =
    lambda_rec (var "fact") (var "n", nat_ty)
      ~body:(
        if_bool
          (le (variable (var "n") nat_ty) (nat 1))
          ~then_:(nat 1)
          ~else_:(mul
                    (variable (var "n") nat_ty)
                    (app (variable (var "fact") (function_ty nat_ty nat_ty))
                         (sub (variable (var "n") nat_ty) (nat 1)))))
  in
  print_vars expr;
  [%expect {|
    fact, n
  |}]

let%expect_test "vars lambda_rec + outer" =
  let expr =
    let_in (var "outer") ~rhs:(int 10)
      ~in_:
        (lambda_rec (var "f") (var "x", int_ty)
          ~body:(
            if_bool
              (lt (variable (var "x") int_ty) (variable (var "outer") int_ty))
              ~then_:(app (variable (var "f") (function_ty int_ty int_ty))
                          (add (variable (var "x") int_ty) (int 1)))
              ~else_:(variable (var "outer") int_ty)
          ))
  in
  print_vars expr;
  [%expect {|
    f, outer, x
  |}]

let%expect_test "vars app overshadow in function" =
  let lam_expr =
    lambda (var "y", nat_ty)
      ~body:(
        let_in (var "y") ~rhs:(nat 99)
          ~in_:(variable (var "y") nat_ty)
      )
  in
  let expr = app lam_expr (variable (var "x") nat_ty) in
  print_vars expr;
  [%expect {|
    x, y
  |}]

let%expect_test "vars app overshadow in argument" =
  let f_expr =
    lambda (var "t", int_ty)
      ~body:(add (variable (var "t") int_ty) (int 1))
  in
  let overshadowed_arg =
    let_in (var "t") ~rhs:(int 100)
      ~in_:(variable (var "t") int_ty)
  in
  let expr = app f_expr overshadowed_arg in
  print_vars expr;
  [%expect {|
    t
  |}]

let%expect_test "vars const int 42" =
  let expr = int 42 in
  print_vars expr;
  [%expect {| |}]

let%expect_test "vars const overshadow in let_in" =
  let expr =
    let_in (var "x") ~rhs:(int 5)
      ~in_:(let_in (var "x") ~rhs:(int 10)
              ~in_:(variable (var "x") int_ty))
  in
  print_vars expr;
  [%expect {|
    x
  |}]

let%expect_test "vars prim eq overshadow" =
  let shadow_z =
    let_in (var "z") ~rhs:(int 5)
      ~in_:(variable (var "z") int_ty)
  in
  let expr = eq shadow_z (variable (var "z") int_ty) in
  print_vars expr;
  [%expect {|
    z
  |}]

let%expect_test "vars prim add overshadow in both arguments" =
  let lhs =
    let_in (var "a") ~rhs:(nat 1)
      ~in_:(variable (var "a") nat_ty)
  in
  let rhs =
    let_in (var "a") ~rhs:(nat 2)
      ~in_:(variable (var "a") nat_ty)
  in
  let expr = add lhs rhs in
  print_vars expr;
  [%expect {|
    a
  |}]

let%expect_test "vars let_mut_in deref assign" =
  let expr =
    let_mut_in (mut_var "m") ~rhs:(nat 0)
      ~in_:
        (let_in (var "dummy") ~rhs:(
          assign (mut_var "m")
            (add (deref (mut_var "m") nat_ty) (variable (var "z") nat_ty))
        )
          ~in_:(deref (mut_var "m") nat_ty))
  in
  print_vars expr;
  [%expect {|
    dummy, m, z
  |}]

let%expect_test "vars deref alone free" =
  let expr =
    deref (mut_var "m") nat_ty
  in
  print_vars expr;
  [%expect {|
    m
  |}]

let%expect_test "vars deref overshadow" =
  let expr =
    let_mut_in (mut_var "m") ~rhs:(nat 10)
      ~in_:(deref (mut_var "m") nat_ty)
  in
  print_vars expr;
  [%expect {|
    m
  |}]

let%expect_test "vars if_bool overshadow in condition" =
  let cond_expr =
    let_in (var "c") ~rhs:(int 5)
      ~in_:(variable (var "c") int_ty)
  in
  let then_expr =
    let_in (var "c") ~rhs:(int 10)
      ~in_:(variable (var "c") int_ty)
  in
  let else_expr =
    let_in (var "c") ~rhs:(int 20)
      ~in_:(variable (var "c") int_ty)
  in
  let expr = if_bool cond_expr ~then_:then_expr ~else_:else_expr in
  print_vars expr;
  [%expect {|
    c
  |}]

let%expect_test "vars if_bool referencing one free var in cond, different in then, else" =
  let cond_expr = eq (variable (var "condX") int_ty) (int 1) in
  let then_expr = add (variable (var "thY") int_ty) (int 2) in
  let else_expr = sub (variable (var "elZ") int_ty) (int 3) in
  let expr = if_bool cond_expr ~then_:then_expr ~else_:else_expr in
  print_vars expr;
  [%expect {|
    condX, elZ, thY
  |}]

let%expect_test "vars if_none" =
  let expr =
    if_none (some (int 5))
      ~none:(nat 999)
      ~some:{
        lam_var = (var "k", int_ty);
        body = add (variable (var "k") int_ty) (variable (var "c") nat_ty)
      }
  in
  print_vars expr;
  [%expect {|
    c, k
  |}]

let%expect_test "vars if_cons overshadow in nonempty" =
  let expr =
    if_cons (nil nat_ty)
      ~empty:(int 0)
      ~nonempty:{
        lam_var1 = (var "hd", nat_ty);
        lam_var2 = (var "hd", list_ty nat_ty);
        body = add
          (variable (var "hd") nat_ty)
          (nat 1)
      }
  in
  print_vars expr;
  [%expect {|
    hd
  |}]

let%expect_test "vars if_left overshadow in right" =
  let left_expr = left (None, None, bool_ty) (bool false) in
  let expr =
    if_left left_expr
      ~left:{
        lam_var = (var "lf", bool_ty);
        body = bool true
      }
      ~right:{
        lam_var = (var "lf", bool_ty); 
        body = variable (var "lf") bool_ty
      }
  in
  print_vars expr;
  [%expect {|
    lf
  |}]

let%expect_test "vars while referencing multiple var" =
  let expr =
    while_ (eq (variable (var "x") nat_ty) (nat 0))
      ~body:(
        assign (mut_var "m")
          (add (deref (mut_var "m") nat_ty) (variable (var "y") nat_ty))
      )
  in
  print_vars expr;
  [%expect {|
    m, x, y
  |}]

let%expect_test "vars while_left overshadow var in body" =
  let expr =
    while_left (left (None,None,int_ty) (int 10)) ~body:{
      lam_var = (var "lv", int_ty);
      body =
        let_in (var "lv") ~rhs:(int 999)
          ~in_:(right (None,None,int_ty) (int 42))
    }
  in
  print_vars expr;
  [%expect{| lv |}]

let%expect_test "vars while_left referencing outer var" =
  let left_cond =
    left (None,None,bool_ty) (variable (var "outerW") bool_ty)
  in
  let expr = while_left left_cond ~body:{
      lam_var=(var "v", bool_ty);
      body=let_in (var "v") ~rhs:(bool false)
             ~in_:(right (None,None,bool_ty) (variable (var "v") bool_ty))
    }
  in
  print_vars expr;
  [%expect{| outerW, v |}]

let%expect_test "vars for" =
  let expr =
    for_ (mut_var "i")
      ~init:(nat 0)
      ~cond:(lt (deref (mut_var "i") nat_ty) (nat 5))
      ~update:(assign (mut_var "i") (add (deref (mut_var "i") nat_ty) (nat 1)))
      ~body:(assign (mut_var "x") (deref (mut_var "i") nat_ty))
  in
  print_vars expr;
  [%expect {|
    i, x
  |}]

let%expect_test "vars for_each overshadow lam_var" =
  let items = cons (int 1) (nil int_ty) in
  let expr =
    for_each items ~body:{
      lam_var=(var "elem", int_ty);
      body=let_in (var "elem") ~rhs:(int 999) ~in_:(variable (var "elem") int_ty)
    }
  in
  print_vars expr;
  [%expect {|
    elem
  |}]

let%expect_test "vars map referencing outer var inside lam" =
  let list_expr = cons (string "a") (nil string_ty) in
  let expr =
    let_in (var "outer") ~rhs:(string "S")
      ~in_:
        (map list_expr ~map:{
          lam_var=(var "v",string_ty);
          body=concat1 (variable (var "v") string_ty)
                       (variable (var "outer") string_ty)
        })
  in
  print_vars expr;
  [%expect {|
    outer, v
  |}]

let%expect_test "vars fold_left overshadow lam var" =
  let list_expr = cons (nat 1) (cons (nat 2) (nil nat_ty)) in
  let expr =
    fold_left list_expr ~init:(nat 0)
      ~fold:{
        lam_var=(var "pairAcc", tuple_ty (mk_row [
          Leaf(None, nat_ty); Leaf(None, nat_ty)
        ]));
        body=let_in (var "pairAcc") ~rhs:(nat 999)
               ~in_:(nat 100)
      }
  in
  print_vars expr;
  [%expect {|
    pairAcc
  |}]

let%expect_test "vars fold_right with outside ref" =
  let lst = cons (int 1) (nil int_ty) in
  let expr =
    let_in (var "outer_f") ~rhs:(int 10)
      ~in_:
        (fold_right lst ~init:(int 0)
          ~fold:{
            lam_var = (var "ea", tuple_ty (mk_row [
              Leaf(None,int_ty); Leaf(None,int_ty)
            ]));
            body = add
              (car (variable (var "ea") (mk_tuple_ty [int_ty; int_ty])))
              (add
                 (cdr (variable (var "ea") (mk_tuple_ty [int_ty; int_ty])))
                 (variable (var "outer_f") int_ty))
          })
  in
  print_vars expr;
  [%expect {|
    ea, outer_f
  |}]

let%expect_test "vars tuple overshadow" =
  let expr =
    tuple (mk_row [
      Leaf(None, variable (var "t") int_ty);
      Leaf(None, let_in (var "t") ~rhs:(int 22) ~in_:(int 33))
    ])
  in
  print_vars expr;
  [%expect {|
    t
  |}]

let%expect_test "vars proj referencing var" =
  let triple = tuple (mk_row [
    Leaf(None, variable (var "alpha") nat_ty);
    Leaf(None, nat 2);
    Leaf(None, nat 3)
  ]) in
  let expr = proj triple ~path:(Here [0]) in
  print_vars expr;
  [%expect {|
    alpha
  |}]

let%expect_test "vars update overshadow" =
  let original =
    tuple (mk_row [
      Leaf(None, int 1);
      Leaf(None, int 2)
    ])
  in
  let expr =
    update_tuple original ~component:(Here [1])
      ~update:(let_in (var "u") ~rhs:(int 10) ~in_:(variable (var "u") int_ty))
  in
  print_vars expr;
  [%expect {|
    u
  |}]

module MA = Michelson.Ast

let push_int n = Tezos_micheline.Micheline.Prim (Ast_builder.Dummy_range.v, "PUSH", [Tezos_micheline.Micheline.Int(Ast_builder.Dummy_range.v, Z.of_int n)], [])

let%expect_test "vars raw_michelson overshadow" =
  let code_ast = Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [ push_int 42 ]) in
  let expr =
    raw_michelson code_ast
      [ variable (var "x") int_ty
      ; let_in (var "x") ~rhs:(int 10) ~in_:(int 20)
      ]
      int_ty
  in
  print_vars expr;
  [%expect {|
    x
  |}]

let%expect_test "vars create_contract overshadow binder_var" =
  let param_storage_ty = mk_tuple_ty [nat_ty; nat_ty] in
  let code_body =
    let_tuple_in [var "p"; var "s"]
      ~rhs:(variable (var "args") param_storage_ty)
      ~in_:(add (variable (var "p") nat_ty) (variable (var "s") nat_ty))
  in
  let expr =
    create_contract
      ~storage:nat_ty
      ~code:{ lam_var=(var "args", param_storage_ty); body=code_body }
      ~delegate:(none key_hash_ty)
      ~initial_balance:(mutez 1000)
      ~initial_storage:(let_in (var "args") ~rhs:(nat 5) ~in_:(nat 6))
  in
  print_vars expr;
  [%expect {|
    args, p, s
  |}]

let%expect_test "vars global_constant overshadow args" =
  let expr =
    global_constant "hashQ"
      [
        let_in (var "x") ~rhs:(int 1) ~in_:(int 2);
        variable (var "x") nat_ty
      ]
      (nat_ty)
  in
  print_vars expr;
  [%expect {|
    x
  |}]

  let%expect_test "vars combined scenario spot-check" =
  let cond_expr =
    left (None,None,bool_ty)
      (gt (deref (mut_var "x") int_ty) (int 0))
  in
  let body_expr =
    let_in (var "flg") ~rhs:(not (variable (var "flg") bool_ty))
      ~in_:
        (right (None,None,bool_ty) (app 
          (variable (var "overshadowZ") (function_ty bool_ty bool_ty))
          (eq (deref (mut_var "x") int_ty)
              (let_in (var "overshadowZ") ~rhs:(int 5)
                 ~in_:(variable (var "overshadowZ") int_ty)))
          ))
  in
  let while_left_expr =
    while_left cond_expr
      ~body:{ lam_var=(var "flg", bool_ty); body=body_expr }
  in
  let expr =
    let_mut_in (mut_var "x") ~rhs:(int 0)
      ~in_:while_left_expr
  in
  print_vars expr;
  [%expect{| flg, overshadowZ, x |}]
