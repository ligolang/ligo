open Core
open Lltz_ir.Ast_builder.With_dummy
module Last_vars = Lltz_codegen.Last_vars
module Ast_builder = Lltz_ir.Ast_builder

let print_used expr =
  let vars = Last_vars.collect_used_vars expr in
  vars
  |> Set.to_list
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:", "
  |> print_endline

let%expect_test "used var simple" =
  let expr = variable (var "x") nat_ty in
  print_used expr;
  [%expect {|
    x
  |}]

let%expect_test "used deref free" =
  let expr = deref (mut_var "m") nat_ty in
  print_used expr;
  [%expect {|
    m
  |}]

let%expect_test "used const" =
  let expr = int 42 in
  print_used expr;
  [%expect {| |}]

let%expect_test "used let_in overshadow" =
  let expr =
    let_in (var "x") ~rhs:(nat 1)
      ~in_:(add (variable (var "x") nat_ty) (variable (var "x") nat_ty))
  in
  print_used expr;
  [%expect {|
    x
  |}]

let%expect_test "used let_mut_in referencing var in body" =
  let expr =
    let_mut_in (mut_var "m") ~rhs:(int 10)
      ~in_:(assign (mut_var "m") (add (deref (mut_var "m") int_ty) (variable (var "x") int_ty)))
  in
  print_used expr;
  [%expect {|
    m, x
  |}]

let%expect_test "used lambda referencing outer var not bound" =
  let lam_expr =
    lambda (var "p", nat_ty)
      ~body:(add (variable (var "p") nat_ty) (variable (var "freeVar") nat_ty))
  in
  print_used lam_expr;
  [%expect {|
    freeVar, p
  |}]

let%expect_test "used lambda_rec referencing outer var" =
  let expr =
    lambda_rec (var "f") (var "n", nat_ty)
      ~body:(
        if_bool
          (le (variable (var "n") nat_ty) (variable (var "outerN") nat_ty))
          ~then_:(variable (var "f") (function_ty nat_ty nat_ty)) 
          ~else_:(app (variable (var "f") (function_ty nat_ty nat_ty)) (variable (var "n") nat_ty))
      )
  in
  print_used expr;
  [%expect {|
    f, n, outerN
  |}]

let%expect_test "used app overshadow in abs + free var in arg" =
  let abs_expr =
    lambda (var "z", nat_ty)
      ~body:(variable (var "z") nat_ty)
  in
  let arg_expr =
    add (int 0) (variable (var "unboundArg") nat_ty)
  in
  let expr = app abs_expr arg_expr in
  print_used expr;
  [%expect {|
    unboundArg, z
  |}]

let%expect_test "used partial application overshadow outer var" =
  let pair_ty = mk_tuple_ty [int_ty; int_ty] in
  let two_arg_fun =
    lambda (var "p", pair_ty)
      ~body:(
        add (variable (var "outer") int_ty)
          (add
             (car (variable (var "p") pair_ty))
             (cdr (variable (var "p") pair_ty)))
      )
  in
  let expr =
    let_in (var "outer") ~rhs:(int 100)
      ~in_:
        (let_in (var "two_arg_fun") ~rhs:two_arg_fun
          ~in_:
            (let_in (var "applied1")
              ~rhs:(apply (int 10) (variable (var "two_arg_fun") (function_ty pair_ty int_ty)))
              ~in_:
                (exec (int 20) (variable (var "applied1") (function_ty int_ty int_ty)))))
  in
  print_used expr;
  [%expect {|
    applied1, outer, p, two_arg_fun
  |}]

let%expect_test "used partial apply overshadow leftover function" =
  let pair_ty = mk_tuple_ty [int_ty; int_ty] in
  let lambda_f =
    lambda (var "p", pair_ty)
      ~body:(
        add
          (variable (var "a") int_ty)
          (add
             (car (variable (var "p") pair_ty))
             (cdr (variable (var "p") pair_ty)))
      )
  in
  let expr =
    let_in (var "a") ~rhs:(int 8)
      ~in_:
        (let_in (var "f") ~rhs:lambda_f
          ~in_:
            (let_in (var "partial")
              ~rhs:(apply (int 2) (variable (var "f") (function_ty pair_ty int_ty)))
              ~in_:
                (let_in (var "leftover")
                  ~rhs:(
                    let_in (var "a") ~rhs:(int 999)
                      ~in_:(variable (var "partial") (function_ty int_ty int_ty))
                  )
                  ~in_:
                    (app (variable (var "leftover") (function_ty int_ty int_ty)) (int 3)))))
  in
  print_used expr;
  [%expect {| a, f, leftover, p, partial |}]


let%expect_test "used prim add" =
  let expr =
    add (variable (var "x") int_ty) (variable (var "y") int_ty)
  in
  print_used expr;
  [%expect {|
    x, y
  |}]

let%expect_test "used prim eq overshadow usage" =
  let lhs = let_in (var "a") ~rhs:(nat 9) ~in_:(variable (var "a") nat_ty) in
  let rhs = variable (var "a") nat_ty in
  let expr = eq lhs rhs in
  print_used expr;
  [%expect {|
    a
  |}]

let%expect_test "used div_ overshadow local var" =
  let expr =
    let_in (var "x") ~rhs:(int 50)
      ~in_:
        (div_
          (variable (var "x") int_ty)
          (let_in (var "x") ~rhs:(int 5) ~in_:(variable (var "x") int_ty)))
  in
  print_used expr;
  [%expect {|
    name_var_4sfa9wjas81, x
  |}]

  let%expect_test "used mod_ overshadow in lhs" =
  let lhs_expr =
    let_in (var "n") ~rhs:(int 7)
      ~in_:(add (variable (var "n") int_ty) (int 2))
  in
  let expr = mod_ lhs_expr (variable (var "y") int_ty) in
  print_used expr;
  [%expect {|
    n, name_var_4sfa9wjas82, y
  |}]

let%expect_test "used div_ with mutable var + overshadow in rhs" =
  let lhs_expr =
    let_mut_in (mut_var "m") ~rhs:(add (variable (var "x") int_ty) (int 1))
      ~in_:(deref (mut_var "m") int_ty)
  in
  let rhs_expr =
    let_in (var "v") ~rhs:(int 2)
      ~in_:(variable (var "v") int_ty)
  in
  let expr = div_ lhs_expr rhs_expr in
  print_used expr;
  [%expect {|
    m, name_var_4sfa9wjas83, v, x
  |}]

let%expect_test "used both div_ and mod_ combined" =
  let a_expr = let_in (var "a") ~rhs:(int 10) ~in_:(variable (var "a") int_ty) in
  let b_expr = let_in (var "b") ~rhs:(int 2)  ~in_:(variable (var "b") int_ty) in
  let div_expr = div_ a_expr b_expr in
  let expr = mod_ div_expr (variable (var "b") int_ty) in
  print_used expr;
  [%expect {|
    a, b, name_var_4sfa9wjas84, name_var_4sfa9wjas85
  |}]

let%expect_test "used if_bool distinct free vars" =
  let condition = eq (variable (var "condX") int_ty) (int 0) in
  let then_expr = variable (var "thenVar") int_ty in
  let else_expr = variable (var "elseVar") int_ty in
  let expr = if_bool condition ~then_:then_expr ~else_:else_expr in
  print_used expr;
  [%expect {|
    condX, elseVar, thenVar
  |}]

let%expect_test "used if_bool overshadow in condition + overshadow in then branch" =
  let condition_expr =
    let_in (var "a") ~rhs:(int 20)
      ~in_:(eq (variable (var "a") int_ty) (variable (var "x") int_ty))
  in
  let then_expr =
    let_in (var "x") ~rhs:(int 99)
      ~in_:(variable (var "x") int_ty)
  in
  let else_expr = variable (var "x") int_ty in
  let full_expr =
    let_in (var "x") ~rhs:(int 10)
      ~in_:(if_bool condition_expr ~then_:then_expr ~else_:else_expr)
  in
  print_used full_expr;
  [%expect {| a, x |}]

let%expect_test "used if_none referencing subject + some" =
  let expr =
    if_none (variable (var "optVal") (option_ty int_ty))
      ~none:(int 100)
      ~some:{
        lam_var=(var "v", int_ty);
        body=add (variable (var "v") int_ty) (variable (var "x") int_ty)
      }
  in
  print_used expr;
  [%expect {|
    optVal, v, x
  |}]

let%expect_test "used if_cons overshadow lam_var1/lam_var2" =
  let expr =
    if_cons (variable (var "lst") (list_ty nat_ty))
      ~empty:(int 0)
      ~nonempty:{
        lam_var1=(var "hd", nat_ty);
        lam_var2=(var "hd", list_ty nat_ty);
        body=add (variable (var "hd") nat_ty) (nat 2)
      }
  in
  print_used expr;
  [%expect {|
    hd, lst
  |}]

let%expect_test "used if_left overshadow lam_var" =
  let subject = variable (var "s") (or_ty (mk_row [
    Leaf(None,int_ty); Leaf(None,int_ty)
  ])) in
  let expr =
    if_left subject
      ~left:{
        lam_var=(var "l",int_ty);
        body=let_in (var "l") ~rhs:(int 999)
               ~in_:(int 1)
      }
      ~right:{
        lam_var=(var "l",int_ty);
        body=add (variable (var "l") int_ty) (int 2)
      }
  in
  print_used expr;
  [%expect {|
    l, s
  |}]

let%expect_test "used while referencing cond, body" =
  let expr =
    while_
      (lt (variable (var "x") nat_ty) (nat 5))
      ~body:(assign (mut_var "m") (variable (var "x") nat_ty))
  in
  print_used expr;
  [%expect {|
    x
  |}]

let%expect_test "used while_left overshadow lam_var" =
  let cond_expr = left (None,None, int_ty) (int 10) in
  let expr = while_left cond_expr ~body:{ lam_var=(var "flag",int_ty); body=(right (None,None, int_ty) (variable (var "flag") int_ty)) } in
  print_used expr;
  [%expect {|
    flag
  |}]

let%expect_test "used for usage" =
  let expr =
    for_ (mut_var "i")
      ~init:(nat 0)
      ~cond:(lt (deref (mut_var "i") nat_ty) (nat 5))
      ~update:(assign (mut_var "i") (add (deref (mut_var "i") nat_ty) (nat 1)))
      ~body:(assign (mut_var "m") (deref (mut_var "i") nat_ty))
  in
  print_used expr;
  [%expect {|
    i
  |}]

let%expect_test "used for overshadow in body referencing outer var" =
  let mut_i = mut_var "i" in
  let init_expr = nat 0 in
  let cond_expr = lt (deref mut_i nat_ty) (nat 3) in
  let update_expr = assign mut_i (add (deref mut_i nat_ty) (nat 1)) in
  let body_expr =
    let_in (var "i") ~rhs:(variable (var "outer") nat_ty)
      ~in_:(add (variable (var "i") nat_ty) (nat 1))
  in
  let for_expr =
    for_ mut_i ~init:init_expr ~cond:cond_expr ~update:update_expr ~body:body_expr
  in
  let full_expr =
    let_in (var "outer") ~rhs:(nat 50)
      ~in_:for_expr
  in
  print_used full_expr;
  [%expect {| i, outer |}]


let%expect_test "used for_each overshadow lam_var" =
  let list_expr = nil int_ty in
  let expr =
    for_each list_expr
      ~body:{
        lam_var=(var "elem",int_ty);
        body=let_in (var "elem") ~rhs:(int 999)
               ~in_:(variable (var "elem") int_ty)
      }
  in
  print_used expr;
  [%expect {|
    elem
  |}]

let%expect_test "used map referencing outer var" =
  let list_expr = cons (nat 1) (nil nat_ty) in
  let lam_body =
    add (variable (var "x") nat_ty) (variable (var "outer") nat_ty)
  in
  let expr =
    let_in (var "outer") ~rhs:(nat 2)
      ~in_:(map list_expr ~map:{ lam_var=(var "x",nat_ty); body=lam_body })
  in
  print_used expr;
  [%expect {|
    outer, x
  |}]

let%expect_test "used fold_left overshadow lam_var" =
  let coll = cons (nat 1) (nil nat_ty) in
  let fold_body =
    let_in (var "acc_x") ~rhs:(int 999)
      ~in_:(nat 0)
  in
  let expr =
    fold_left coll ~init:(nat 10)
      ~fold:{ lam_var=(var "acc_x", tuple_ty (mk_row [
        Leaf(None,nat_ty); Leaf(None,nat_ty)
      ])); body=fold_body }
  in
  print_used expr;
  [%expect {| |}]

let%expect_test "used fold_right referencing outer var" =
  let coll = cons (int 1) (nil int_ty) in
  let expr =
    let_in (var "outer") ~rhs:(int 100)
      ~in_:
        (fold_right coll ~init:(int 0)
          ~fold:{
            lam_var=(var "p", tuple_ty (mk_row [
              Leaf(None,int_ty); Leaf(None,int_ty)
            ]));
            body=add
              (car (variable (var "p") (mk_tuple_ty [int_ty; int_ty])))
              (add
                 (cdr (variable (var "p") (mk_tuple_ty [int_ty; int_ty])))
                 (variable (var "outer") int_ty))
          })
  in
  print_used expr;
  [%expect {|
    outer, p
  |}]

let%expect_test "used let_tuple_in overshadow" =
  let triple =
    tuple (mk_row [
      Leaf(None, int 1);
      Leaf(None, int 2);
      Leaf(None, int 3)
    ])
  in
  let expr =
    let_tuple_in [var "a"; var "b"; var "a"]
      ~rhs:triple
      ~in_:(add (variable (var "a") int_ty) (variable (var "b") int_ty))
  in
  print_used expr;
  [%expect {|
    a, b
  |}]

let%expect_test "used tuple referencing x,y inside row" =
  let expr =
    tuple (mk_row [
      Leaf(None, variable (var "x") nat_ty);
      Leaf(None, variable (var "y") nat_ty)
    ])
  in
  print_used expr;
  [%expect {|
    x, y
  |}]

let%expect_test "used proj referencing some var in the tuple" =
  let tuple_expr =
    tuple (mk_row [
      Leaf(None, variable (var "alpha") int_ty);
      Leaf(None, int 20)
    ])
  in
  let expr =
    proj tuple_expr ~path:(Here [0])
  in
  print_used expr;
  [%expect {|
    alpha
  |}]

let%expect_test "used update overshadow" =
  let pair_expr =
    tuple (mk_row [
      Leaf(None, nat 10);
      Leaf(None, nat 20)
    ])
  in
  let upd_expr =
    let_in (var "u") ~rhs:(nat 999)
      ~in_:(variable (var "u") nat_ty)
  in
  let expr =
    update_tuple pair_expr ~component:(Here [1]) ~update:upd_expr
  in
  print_used expr;
  [%expect {|
    u
  |}]

module MA = Michelson.Ast

let push_int n = Tezos_micheline.Micheline.Prim(Ast_builder.Dummy_range.v, "PUSH", [Tezos_micheline.Micheline.Int(Ast_builder.Dummy_range.v, Z.of_int n)], [])

let%expect_test "used raw_michelson overshadow" =
  let code_ast = Tezos_micheline.Micheline.Seq (Ast_builder.Dummy_range.v, [ push_int 42 ]) in
  let arg1 = variable (var "a1") int_ty in
  let arg2 =
    let_in (var "a1") ~rhs:(int 100)
      ~in_:(variable (var "a1") int_ty)
  in
  let expr =
    raw_michelson code_ast [arg1; arg2] int_ty
  in
  print_used expr;
  [%expect {|
    a1
  |}]

let%expect_test "used create_contract overshadow binder_var" =
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
      ~delegate:(variable (var "del") (option_ty key_hash_ty))
      ~initial_balance:(variable (var "bal") mutez_ty)
      ~initial_storage:(let_in (var "args") ~rhs:(nat 5) ~in_:(nat 6))
  in
  print_used expr;
  [%expect {|
    args, bal, del, p, s
  |}]

let%expect_test "used global_constant overshadow" =
  let expr =
    global_constant "myHash"
      [
        let_in (var "g") ~rhs:(int 10)
          ~in_:(variable (var "g") int_ty);
        variable (var "g") int_ty
      ]
      (int_ty)
  in
  print_used expr;
  [%expect {|
    g
  |}]

(* TODO: Inj/Match *)