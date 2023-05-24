open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location
module Variable = Ligo_prim.Value_var

include Flag.With_arg (struct
  type flag = bool
end)

let internal_for_loop_identifier = "internal_for_loop_identifier"

(* creates a dummy declaration (const nternal_for_loop_identifier#123 = unit;)
   used to delimit different parts of for-loop  *)
let internal_delimiter ~loc () =
  let loop_id = Ligo_prim.Value_var.fresh ~loc ~name:internal_for_loop_identifier () in
  s_decl
    ~loc
    (d_const
       ~loc
       { type_params = None
       ; pattern = p_var ~loc loop_id
       ; rhs_type = Some (tv_unit ~loc ())
       ; let_rhs = e_unit ~loc
       })


let while_loop
    ~raise
    ~loc
    ~condition
    ~(afterthought : expr List.Ne.t option)
    ~(statement : statement option)
  =
  let afterthought =
    Option.map afterthought ~f:(fun (afterthought : expr List.Ne.t) ->
        let exprs = List.Ne.to_list afterthought in
        let loc, stmts =
          List.fold_map exprs ~init:Location.generated ~f:(fun prev_loc expr ->
              let expr = expr.fp in
              let loc = Location.get_location expr in
              let expr : expr = { fp = expr } in
              Location.cover prev_loc loc, s_instr ~loc (i_expr ~loc expr))
        in
        loc, stmts)
  in
  let statement =
    Option.map statement ~f:(fun statement ->
        let loc = Location.get_location statement.fp in
        loc, [ statement ])
  in
  let block =
    match afterthought, statement with
    | None, None ->
      (* If both afterthought & statement are absent, we do
         {
            let internal_for_loop_identifier#125 = unit ;
         }
      *)
      let block = Location.wrap ~loc (List.Ne.of_list [ internal_delimiter ~loc () ]) in
      ({ fp = block } : block)
    | Some (loc, afterthought), None ->
      (* If only afterthought is present, we do
         {
            let internal_for_loop_identifier#125 = unit ;
            afterthought
         }
      *)
      let block =
        Location.wrap
          ~loc
          (List.Ne.of_list ([ internal_delimiter ~loc () ] @ afterthought))
      in
      ({ fp = block } : block)
    | None, Some (loc, statement) ->
      (* If only statement is present, we do
         {
            statement
            let internal_for_loop_identifier#125 = unit ;
         }
      *)
      let block =
        Location.wrap ~loc (List.Ne.of_list (statement @ [ internal_delimiter ~loc () ]))
      in
      ({ fp = block } : block)
    | Some (_, afterthought), Some (loc, statement) ->
      (* If both afterthought & statement are present, we do
         {
            statement ;
            let internal_for_loop_identifier#125 = unit ;
            afterthought ;
         }
      *)
      let block =
        Location.wrap
          ~loc
          (List.Ne.of_list (statement @ [ internal_delimiter ~loc () ] @ afterthought))
      in
      ({ fp = block } : block)
  in
  let cond =
    match condition with
    | None ->
      let warn_infinite_loop = get_flag () in
      if warn_infinite_loop then raise.warning (`Nanopasses_infinite_for_loop loc);
      e_true ~loc
    | Some cond -> cond
  in
  s_instr ~loc (i_while ~loc { cond; block })


let compile ~raise =
  let instruction
      : (instruction, expr, pattern, statement, block) instruction_ -> instruction
    =
   fun instr ->
    let loc = Location.get_location instr in
    let open For_stmt in
    match Location.unwrap instr with
    | I_for_stmt { initialiser; condition; afterthought; statement } ->
      (* 'for' loops are desugared into 'while' loops like,

        for (opt(initialiser) ; opt(condition) ; opt(afterthought)) opt(body) 
          
        ==>
        
        { 
          let internal_for_loop_identifier#123 = unit ;

          opt(initialiser) ;
          
          let internal_for_loop_identifier#124 = unit ; // acts as a separator
          
          while (condition) // If condition is None we raise infinite_for_loop error
          { 
            opt(body) ; 
          
            let internal_for_loop_identifier#125 = unit ; // acts as a separator
          
            opt(afterthought) ;
          } 
        }
      *)
      let statements =
        [ internal_delimiter ~loc () ]
        @ Option.value_map initialiser ~default:[] ~f:List.return
        @ [ internal_delimiter ~loc () ]
        @ [ while_loop ~raise ~loc ~condition ~afterthought ~statement ]
      in
      i_block ~loc { fp = Location.wrap ~loc (List.Ne.of_list statements) }
    | instr -> make_i ~loc instr
  in
  Fold { idle_fold with instruction }


let reduction ~raise =
  { Iter.defaults with
    instruction =
      (function
      | { wrap_content = I_for_stmt _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing

open Unit_test_helpers.Instruction

let%expect_test "compile [for (DECLARATION ; EXPR1 ; EXPR2) BLOCK]" =
  {|
  (I_for_stmt
    ((initialiser((S_decl (DECLARATION))))
    (condition ((EXPR1)))
    (afterthought (((EXPR2))))
    (statement ((S_instr (I_block (BLOCK)))))))
  |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR1))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR2))))))))))
    |}]

let%expect_test "compile [for ( ; EXPR ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR)))
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let%expect_test "compile [for ( ; EXPR ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR)))
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let%expect_test "compile [for ( ; EXPR1 ; EXPR2) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR1)))
      (afterthought (((EXPR2))))
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR1))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR2)))))))))) |}]

let%expect_test "compile [for (DECLARATION ; EXPR ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ((EXPR)))
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let%expect_test "compile [for ( ; EXPR1 ; EXPR2) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR1)))
      (afterthought (((EXPR2))))
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR1))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR2)))))))))) |}]

let%expect_test "compile [for (DECLARATION ; EXPR ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ((EXPR)))
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let%expect_test "compile [for (DECLARATION ; EXPR1 ; EXPR2) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ((EXPR1)))
      (afterthought (((EXPR2))))
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (EXPR1))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR2)))))))))) |}]

(* Infinite for loops *)

let flag_bef = !flag
let () = flag := Some (true, true)

let%expect_test "compile [for (DECLARATION ; ; EXPR) BLOCK]" =
  {|
  (I_for_stmt
    ((initialiser((S_decl (DECLARATION))))
    (condition ())
    (afterthought (((EXPR))))
    (statement ((S_instr (I_block (BLOCK)))))))
  |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR)))))))))) |}]

let%expect_test "compile [for ( ; ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let%expect_test "compile [for ( ; ; EXPR) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought (((EXPR))))
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR)))))))))) |}]

let%expect_test "compile [for ( ; ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let%expect_test "compile [for ( ; ; EXPR) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought (((EXPR))))
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR)))))))))) |}]

let%expect_test "compile [for (DECLARATION; ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ())
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let%expect_test "compile [for (DECLARATION; ; EXPR) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ())
      (afterthought (((EXPR))))
      (statement ())))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))
           (S_instr (I_expr (EXPR)))))))))) |}]

let%expect_test "compile [for (DECLARATION ; ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ())
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  [%expect
    {|
    (I_block
     ((S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_decl (DECLARATION))
      (S_decl
       (D_const
        ((pattern (P_var internal_for_loop_identifier)) (rhs_type (T_var unit))
         (let_rhs (E_literal Literal_unit)))))
      (S_instr
       (I_while
        ((cond (E_constant ((cons_name C_TRUE))))
         (block
          ((S_instr (I_block (BLOCK)))
           (S_decl
            (D_const
             ((pattern (P_var internal_for_loop_identifier))
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}]

let () = flag := flag_bef
