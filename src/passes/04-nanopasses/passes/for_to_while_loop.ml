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


let is_internal_delimiter : statement -> unit option =
 fun stmt ->
  let open Simple_utils.Option in
  let* decl = get_s_decl_opt stmt in
  let* d_const = get_d_const_opt decl in
  let Simple_decl.{ pattern; let_rhs; _ } = d_const in
  let* v = get_p_var_opt pattern in
  if is_e_unit let_rhs && Variable.is_name v internal_for_loop_identifier
  then Some ()
  else None


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
          
          while (condition) // If condition is None we consider it as e_true
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

let decompile ~raise:_ =
  let instruction
      : (instruction, expr, pattern, statement, block) instruction_ -> instruction
    =
   fun i ->
    let loc = Location.get_location i in
    let open Simple_utils.Option in
    let decompile_initialiser stmts =
      match stmts with
      | initialiser :: statements when Option.is_some (is_internal_delimiter initialiser)
        -> Some (None, statements)
      | initialiser :: internal_delimiter :: statements
        when Option.is_some (is_internal_delimiter internal_delimiter) ->
        Some (Some initialiser, statements)
      | _ -> None
    in
    let decompile_condition stmts =
      match stmts with
      | [ while_loop ] ->
        let* instr = get_s_instr_opt while_loop in
        let* { cond; block } = get_i_while_opt instr in
        Some (Some cond, block)
      | _ -> None
    in
    let decompile_afterthought afterthought =
      match afterthought with
      | [] -> None
      | expr_stmts ->
        let ne_expr_stmts = List.Ne.of_list expr_stmts in
        ne_expr_stmts
        |> List.Ne.map (fun expr_stmt ->
               let* s = get_s_instr expr_stmt in
               let* e = get_i_expr s in
               Some e)
        |> List.Ne.deoptionalize
    in
    let decompile_body_and_afterthought (b : block) =
      let statements = List.Ne.to_list b.fp.wrap_content in
      match statements with
      | [ internal_delimiter ]
        when Option.is_some (is_internal_delimiter internal_delimiter) -> Some (None, None)
      | internal_delimiter :: afterthought
        when Option.is_some (is_internal_delimiter internal_delimiter) ->
        Some (None, decompile_afterthought afterthought)
      | body :: internal_delimiter :: afterthought
        when Option.is_some (is_internal_delimiter internal_delimiter) ->
        Some (Some body, decompile_afterthought afterthought)
      | _ -> None
    in
    let decompile_for_loop ~loc (statement, statements) =
      let* () = is_internal_delimiter statement in
      let* initialiser, statements = decompile_initialiser statements in
      let* condition, block = decompile_condition statements in
      let* statement, afterthought = decompile_body_and_afterthought block in
      Some (i_for_stmt ~loc { initialiser; condition; statement; afterthought })
    in
    match Location.unwrap i with
    | I_block b ->
      let statements = b.fp.wrap_content in
      (match decompile_for_loop ~loc statements with
      | None -> make_i ~loc i.wrap_content
      | Some for_loop -> for_loop)
    | _ -> make_i ~loc i.wrap_content
  in
  Fold { idle_fold with instruction }


open Unit_test_helpers.Instruction

let%expect_test "compile & decompile [for (DECLARATION ; EXPR1 ; EXPR2) BLOCK]" =
  {|
  (I_for_stmt
    ((initialiser((S_decl (DECLARATION))))
    (condition ((EXPR1)))
    (afterthought (((EXPR2))))
    (statement ((S_instr (I_block (BLOCK)))))))
  |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
    |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION)))) (condition ((EXPR1)))
      (afterthought (((EXPR2)))) (statement ((S_instr (I_block (BLOCK))))))) |}]

let%expect_test "compile & decompile [for ( ; EXPR ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR)))
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((EXPR))) (afterthought ())
      (statement ((S_instr (I_block (BLOCK))))))) |}]

let%expect_test "compile & decompile [for ( ; EXPR ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR)))
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((EXPR))) (afterthought ()) (statement ())))
     |}]

let%expect_test "compile & decompile [for ( ; EXPR1 ; EXPR2) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR1)))
      (afterthought (((EXPR2))))
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
           (S_instr (I_expr (EXPR2)))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((EXPR1))) (afterthought (((EXPR2))))
      (statement ((S_instr (I_block (BLOCK))))))) |}]

let%expect_test "compile & decompile [for (DECLARATION ; EXPR ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ((EXPR)))
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION)))) (condition ((EXPR)))
      (afterthought ()) (statement ((S_instr (I_block (BLOCK)))))))
     |}]

let%expect_test "compile & decompile [for ( ; EXPR1 ; EXPR2) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ((EXPR1)))
      (afterthought (((EXPR2))))
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
           (S_instr (I_expr (EXPR2)))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((EXPR1))) (afterthought (((EXPR2))))
      (statement ())))
    |}]

let%expect_test "compile & decompile [for (DECLARATION ; EXPR ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ((EXPR)))
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION)))) (condition ((EXPR)))
      (afterthought ()) (statement ()))) |}]

let%expect_test "compile & decompile [for (DECLARATION ; EXPR1 ; EXPR2) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ((EXPR1)))
      (afterthought (((EXPR2))))
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
           (S_instr (I_expr (EXPR2)))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION)))) (condition ((EXPR1)))
      (afterthought (((EXPR2)))) (statement ()))) |}]

(* Infinite for loops *)

let flag_bef = !flag
let () = flag := Some (true, true)

let%expect_test "compile & decompile [for (DECLARATION ; ; EXPR) BLOCK]" =
  {|
  (I_for_stmt
    ((initialiser((S_decl (DECLARATION))))
    (condition ())
    (afterthought (((EXPR))))
    (statement ((S_instr (I_block (BLOCK)))))))
  |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
           (S_instr (I_expr (EXPR)))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION))))
      (condition ((E_constant ((cons_name C_TRUE))))) (afterthought (((EXPR))))
      (statement ((S_instr (I_block (BLOCK))))))) |}]

let%expect_test "compile & decompile [for ( ; ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((E_constant ((cons_name C_TRUE)))))
      (afterthought ()) (statement ((S_instr (I_block (BLOCK))))))) |}]

let%expect_test "compile & decompile [for ( ; ; EXPR) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought (((EXPR))))
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
           (S_instr (I_expr (EXPR)))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((E_constant ((cons_name C_TRUE)))))
      (afterthought (((EXPR)))) (statement ((S_instr (I_block (BLOCK))))))) |}]

let%expect_test "compile & decompile [for ( ; ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((E_constant ((cons_name C_TRUE)))))
      (afterthought ()) (statement ()))) |}]

let%expect_test "compile & decompile [for ( ; ; EXPR) ;]" =
  {|
    (I_for_stmt
      ((initialiser())
      (condition ())
      (afterthought (((EXPR))))
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
           (S_instr (I_expr (EXPR)))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ()) (condition ((E_constant ((cons_name C_TRUE)))))
      (afterthought (((EXPR)))) (statement ()))) |}]

let%expect_test "compile & decompile [for (DECLARATION; ; ) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ())
      (afterthought ())
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION))))
      (condition ((E_constant ((cons_name C_TRUE))))) (afterthought ())
      (statement ()))) |}]

let%expect_test "compile & decompile [for (DECLARATION; ; EXPR) ;]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ())
      (afterthought (((EXPR))))
      (statement ())))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
           (S_instr (I_expr (EXPR)))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION))))
      (condition ((E_constant ((cons_name C_TRUE))))) (afterthought (((EXPR))))
      (statement ()))) |}]

let%expect_test "compile & decompile [for (DECLARATION ; ; ) BLOCK]" =
  {|
    (I_for_stmt
      ((initialiser((S_decl (DECLARATION))))
      (condition ())
      (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))
    |}
  |-> compile;
  let output = [%expect.output] in
  print_endline output;
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
              (rhs_type (T_var unit)) (let_rhs (E_literal Literal_unit)))))))))))) |}];
  output |-> decompile;
  [%expect
    {|
    (I_for_stmt
     ((initialiser ((S_decl (DECLARATION))))
      (condition ((E_constant ((cons_name C_TRUE))))) (afterthought ())
      (statement ((S_instr (I_block (BLOCK)))))))|}]

let () = flag := flag_bef
