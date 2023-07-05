open Simple_utils.Trace
open Ast_unified
open Pass_type
open Errors

(* morph binary and unary operators/keywords to ligo internal constants 
  each syntax has its own set of keywords *)
include Flag.With_arg (struct
  type flag = Syntax_types.t
end)

let todo_error operator =
  failwith
    (Format.asprintf
       "error operator: %s"
       (Sexp.to_string @@ Operators.sexp_of_operator operator))


(*
  notes:
    - in the future (), we could imagine those mapping to be defined in the stdlib
*)
let mapping_binop ~syntax : (Operators.op * Ligo_prim.Constant.constant') list =
  let open Syntax_types in
  match syntax with
  | CameLIGO ->
    [ PLUS, C_ADD
    ; MINUS, C_POLYMORPHIC_SUB
    ; STAR, C_MUL
    ; SLASH, C_DIV
    ; PRCENT, C_MOD
    ; DPIPE, C_OR
    ; DAMPERSAND, C_AND
    ; LT, C_LT
    ; GT, C_GT
    ; GE, C_GE
    ; LE, C_LE
    ; SEQ, C_EQ
    ; LTGT, C_NEQ
    ; WORD_LSL, C_LSL
    ; WORD_LSR, C_LSR
    ; WORD_LXOR, C_LXOR
    ; WORD_LAND, C_LAND
    ; WORD_LOR, C_LOR
    ; DCOLON, C_CONS
    ; CARET, C_CONCAT
    ]
  | JsLIGO ->
    [ PLUS, C_POLYMORPHIC_ADD
    ; PLUS, C_ADD
    ; MINUS, C_POLYMORPHIC_SUB
    ; STAR, C_MUL
    ; SLASH, C_DIV
    ; PRCENT, C_MOD
    ; DPIPE, C_OR
    ; DAMPERSAND, C_AND
    ; LT, C_LT
    ; GT, C_GT
    ; GE, C_GE
    ; LE, C_LE
    ; DEQ, C_EQ
    ; EQ_SLASH_EQ, C_NEQ
    ]
  | PascaLIGO ->
    [ SHARP, C_CONS
    ; CONTAINS, C_SET_MEM
    ; CARET, C_CONCAT
    ; PLUS, C_ADD
    ; MINUS, C_POLYMORPHIC_SUB
    ; STAR, C_MUL
    ; SLASH, C_DIV
    ; WORD_MOD, C_MOD
    ; WORD_OR, C_OR
    ; WORD_AND, C_AND
    ; LT, C_LT
    ; GT, C_GT
    ; GE, C_GE
    ; LE, C_LE
    ; SEQ, C_EQ
    ; EQ_SLASH_EQ, C_NEQ
    ]


let mapping_unop ~syntax : (Operators.op * Ligo_prim.Constant.constant') list =
  let open Syntax_types in
  match syntax with
  | CameLIGO -> [ MINUS, C_NEG; WORD_NOT, C_NOT ]
  | JsLIGO -> [ MINUS, C_NEG; EX_MARK, C_NOT ]
  | PascaLIGO -> [ MINUS, C_NEG; WORD_NOT, C_NOT ]


let get_constant_of_operator mapping ~syntax k : Ligo_prim.Constant.constant' option =
  let m = mapping ~syntax in
  List.Assoc.find m ~equal:Operators.equal_op k


let get_operator_of_constant mapping ~syntax k : Operators.op option =
  let m = List.map ~f:(fun (x, y) -> y, x) (mapping ~syntax) in
  List.Assoc.find m ~equal:Ligo_prim.Constant.equal_constant' k


let compile ~raise:_ =
  let syntax = get_flag () in
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    (* we are unfortunately throwing operator locations away *)
    match Location.unwrap e with
    | E_binary_op { operator; left; right } ->
      (match
         get_constant_of_operator mapping_binop ~syntax (Location.unwrap operator)
       with
      | Some cons_name -> e_constant ~loc { cons_name; arguments = [ left; right ] }
      | None -> todo_error operator)
    | E_unary_op { operator; arg } ->
      (match get_constant_of_operator mapping_unop ~syntax (Location.unwrap operator) with
      | Some cons_name -> e_constant ~loc { cons_name; arguments = [ arg ] }
      | None -> todo_error operator)
    | x -> make_e ~loc x
  in
  Fold { idle_fold with expr = pass_expr }


let reduction ~raise =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_binary_op _ | E_unary_op _; _ } -> fail ()
      | _ -> ())
  }


let decompile ~raise:_ =
  let syntax = get_flag () in
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_constant { cons_name = C_SUB; arguments = [ left; right ] } ->
      (* temporary:
          until after protocol lima (emit C_POLYMORPHIC_SUB in fuzz_ast_aggregated.ml)
        *)
      (match get_operator_of_constant mapping_binop ~syntax C_POLYMORPHIC_SUB with
      | Some c -> e_binary_op ~loc { operator = Location.wrap ~loc c; left; right }
      | None -> make_e ~loc e.wrap_content)
    | E_constant { cons_name; arguments = [ left; right ] } ->
      (match get_operator_of_constant mapping_binop ~syntax cons_name with
      | Some c -> e_binary_op ~loc { operator = Location.wrap ~loc c; left; right }
      | None -> make_e ~loc e.wrap_content)
    | E_constant { cons_name; arguments = [ arg ] } ->
      (match get_operator_of_constant mapping_unop ~syntax cons_name with
      | Some c -> e_unary_op ~loc { operator = Location.wrap ~loc c; arg }
      | None -> make_e ~loc e.wrap_content)
    | x -> make_e ~loc x
  in
  Fold { idle_fold with expr = pass_expr }


let name = __MODULE__

open Unit_test_helpers.Expr

let flag_bef = !flag
let () = flag := Some (true, PascaLIGO)

let%expect_test "compile" =
  {| (E_binary_op ((operator SLASH) (left (EXPR1)) (right (EXPR2)))) |} |-> compile;
  [%expect {| (E_constant ((cons_name C_DIV) (arguments ((EXPR1) (EXPR2))))) |}]

let%expect_test "decompile" =
  {| (E_constant ((cons_name C_DIV) (arguments ((EXPR1) (EXPR2))))) |} |-> decompile;
  [%expect {| (E_binary_op ((operator SLASH) (left (EXPR1)) (right (EXPR2)))) |}]

let () = flag := flag_bef
