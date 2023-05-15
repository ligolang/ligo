open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location
module Variable = Ligo_prim.Value_var
include Flag.No_arg ()

let interal_postfix_old_value = "interal_postfix_old_value"

let get_operator =
  let open Prefix_postfix in
  function
  | Increment -> Operators.PLUS
  | Decrement -> MINUS


let compile ~raise:_ =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let open Prefix_postfix in
    match Location.unwrap e with
    | E_prefix { pre_op; variable = v } ->
      (* Prefix operators (++p) are turned into
         ```      
         let () = p := p + 1 in p
         ``` *)
      let pre_op = Location.unwrap pre_op in
      e_simple_let_in
        ~loc
        { binder = p_unit ~loc
        ; rhs =
            e_assign_unitary
              ~loc
              { binder = Ligo_prim.Binder.make v None
              ; expression =
                  e_binary_op
                    ~loc
                    { operator = Location.wrap ~loc (get_operator pre_op)
                    ; left = e_variable ~loc v
                    ; right = e_literal ~loc (Literal_int Z.one)
                    }
              }
        ; let_result = e_variable ~loc v
        }
    | E_postfix { post_op; variable = v } ->
      (* Postfix operators (p++) are turned into
         ```
         let interal_postfix_old_value#1 = p in
         let () = p := p + 1 in
         interal_postfix_old_value#1
         ``` *)
      let post_op = Location.unwrap post_op in
      let old_value = Variable.fresh ~loc ~name:interal_postfix_old_value () in
      e_simple_let_in
        ~loc
        { binder = p_var ~loc old_value
        ; rhs = e_variable ~loc v
        ; let_result =
            e_simple_let_in
              ~loc
              { binder = p_unit ~loc
              ; rhs =
                  e_assign_unitary
                    ~loc
                    { binder = Ligo_prim.Binder.make v None
                    ; expression =
                        e_binary_op
                          ~loc
                          { operator = Location.wrap ~loc (get_operator post_op)
                          ; left = e_variable ~loc v
                          ; right = e_literal ~loc (Literal_int Z.one)
                          }
                    }
              ; let_result = e_variable ~loc old_value
              }
        }
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_prefix _ | E_postfix _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__

let decompile ~raise:_ =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let postfix Simple_let_in.{ binder; rhs; let_result } =
      let open Simple_utils.Option in
      let* old = get_p_var binder in
      let* var = get_e_variable rhs in
      let* Simple_let_in.{ binder; rhs; let_result } = get_e_simple_let_in let_result in
      let* var''' = get_e_variable let_result in
      let* () = get_p_unit binder in
      let* Assign.{ binder = { var = var'; _ }; expression = assign_body } =
        get_e_assign_unitary rhs
      in
      let* Operators.{ left; operator; right } = get_e_binary_op assign_body in
      let* var'' = get_e_variable left in
      let* n = get_e_literal_int right in
      let* () =
        some_if
          Variable.(
            equal var var'
            && equal var var''
            && equal old var'''
            && is_name old interal_postfix_old_value
            && Z.equal n Z.one)
          ()
      in
      match Location.unwrap operator with
      | PLUS ->
        return
        @@ e_postfix
             ~loc
             { post_op = Location.wrap ~loc Prefix_postfix.Increment; variable = var }
      | MINUS ->
        return
        @@ e_postfix
             ~loc
             { post_op = Location.wrap ~loc Prefix_postfix.Decrement; variable = var }
      | _ -> None
    in
    let prefix Simple_let_in.{ binder; rhs; let_result } =
      let open Simple_utils.Option in
      let* () = get_p_unit binder in
      let* { binder; expression } = get_e_assign_unitary rhs in
      let var = binder.var in
      let* { left; operator; right } = get_e_binary_op expression in
      let* var' = get_e_variable left in
      let* n = get_e_literal_int right in
      let* var'' = get_e_variable let_result in
      let* () =
        some_if Variable.(equal var var' && equal var var'' && Z.equal n Z.one) ()
      in
      match Location.unwrap operator with
      | PLUS ->
        return
        @@ e_prefix
             ~loc
             { pre_op = Location.wrap ~loc Prefix_postfix.Increment; variable = var }
      | MINUS ->
        return
        @@ e_prefix
             ~loc
             { pre_op = Location.wrap ~loc Prefix_postfix.Decrement; variable = var }
      | _ -> None
    in
    match Location.unwrap e with
    | E_simple_let_in x ->
      let opt = Option.merge (postfix x) (prefix x) ~f:(fun x _ -> x) in
      Option.value_or_thunk opt ~default:(fun () -> make_e ~loc e.wrap_content)
    | _ -> make_e ~loc e.wrap_content
  in
  Fold { idle_fold with expr }


open Unit_test_helpers.Expr

let%expect_test "compile & decompile x++" =
  {|
  (E_postfix ((post_op Increment) (variable x)))
  |} |-> compile;
  [%expect
    {|
    (E_simple_let_in
     ((binder (P_var interal_postfix_old_value)) (rhs (E_variable x))
      (let_result
       (E_simple_let_in
        ((binder P_unit)
         (rhs
          (E_assign_unitary
           ((binder ((var x) (ascr ())))
            (expression
             (E_binary_op
              ((operator PLUS) (left (E_variable x))
               (right (E_literal (Literal_int 1)))))))))
         (let_result (E_variable interal_postfix_old_value)))))))
    |}];
  {|
  (E_simple_let_in
     ((binder (P_var interal_postfix_old_value)) (rhs (E_variable x))
      (let_result
       (E_simple_let_in
        ((binder P_unit)
         (rhs
          (E_assign_unitary
           ((binder ((var x) (ascr ())))
            (expression
             (E_binary_op
              ((operator PLUS) (left (E_variable x))
               (right (E_literal (Literal_int 1)))))))))
         (let_result (E_variable interal_postfix_old_value)))))))
  |}
  |-> decompile;
  [%expect {|
    (E_postfix ((post_op Increment) (variable x)))
    |}]

let%expect_test "compile & decompile x--" =
  {|
  (E_postfix ((post_op Decrement) (variable x)))
    |} |-> compile;
  [%expect
    {|
        (E_simple_let_in
         ((binder (P_var interal_postfix_old_value)) (rhs (E_variable x))
          (let_result
           (E_simple_let_in
            ((binder P_unit)
             (rhs
              (E_assign_unitary
               ((binder ((var x) (ascr ())))
                (expression
                 (E_binary_op
                  ((operator MINUS) (left (E_variable x))
                   (right (E_literal (Literal_int 1)))))))))
             (let_result (E_variable interal_postfix_old_value)))))))
      |}];
  {|
       (E_simple_let_in
         ((binder (P_var interal_postfix_old_value)) (rhs (E_variable x))
          (let_result
           (E_simple_let_in
            ((binder P_unit)
             (rhs
              (E_assign_unitary
               ((binder ((var x) (ascr ())))
                (expression
                 (E_binary_op
                  ((operator MINUS) (left (E_variable x))
                   (right (E_literal (Literal_int 1)))))))))
             (let_result (E_variable interal_postfix_old_value)))))))

    |}
  |-> decompile;
  [%expect {| 
    (E_postfix ((post_op Decrement) (variable x)))
      |}]

let%expect_test "compile & decompile ++x" =
  {|
  (E_prefix ((pre_op Increment) (variable x)))
      |} |-> compile;
  [%expect
    {|
          (E_simple_let_in
           ((binder P_unit)
            (rhs
             (E_assign_unitary
              ((binder ((var x) (ascr ())))
               (expression
                (E_binary_op
                 ((operator PLUS) (left (E_variable x))
                  (right (E_literal (Literal_int 1)))))))))
            (let_result (E_variable x))))
        |}];
  {|
        (E_simple_let_in
          ((binder P_unit)
          (rhs
            (E_assign_unitary
            ((binder ((var x) (ascr ())))
              (expression
              (E_binary_op
                ((operator PLUS) (left (E_variable x))
                (right (E_literal (Literal_int 1)))))))))
          (let_result (E_variable x))))
            |}
  |-> decompile;
  [%expect {|
    (E_prefix ((pre_op Increment) (variable x)))
            |}]

let%expect_test "compile & decompile --x" =
  {|
  (E_prefix ((pre_op Decrement) (variable x)))
        |} |-> compile;
  [%expect
    {|
            (E_simple_let_in
             ((binder P_unit)
              (rhs
               (E_assign_unitary
                ((binder ((var x) (ascr ())))
                 (expression
                  (E_binary_op
                   ((operator MINUS) (left (E_variable x))
                    (right (E_literal (Literal_int 1)))))))))
              (let_result (E_variable x))))
          |}];
  {|
  (E_simple_let_in
             ((binder P_unit)
              (rhs
               (E_assign_unitary
                ((binder ((var x) (ascr ())))
                 (expression
                  (E_binary_op
                   ((operator MINUS) (left (E_variable x))
                    (right (E_literal (Literal_int 1)))))))))
              (let_result (E_variable x))))
        |}
  |-> decompile;
  [%expect {|
    (E_prefix ((pre_op Decrement) (variable x)))
          |}]
