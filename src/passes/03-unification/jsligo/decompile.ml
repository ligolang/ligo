module CST = Cst.Jsligo
module AST = Ast_unified
open Simple_utils
open Lexing_jsligo.Token

let rec folder =
  let todo _ = failwith ("TODO" ^ __LOC__) in
  AST.Catamorphism.
    { expr
    ; ty_expr
    ; pattern = todo
    ; statement = todo
    ; block = todo
    ; mod_expr = todo
    ; instruction = todo
    ; declaration = todo
    ; program_entry = todo
    ; program = todo
    }


and decompile_program p = AST.Catamorphism.cata_program ~f:folder p
and decompile_expression e = AST.Catamorphism.cata_expr ~f:folder e
and decompile_pattern p = AST.Catamorphism.cata_pattern ~f:folder p

and expr : (CST.expr, unit, CST.pattern, unit, unit) AST.expression_ -> CST.expr =
 fun e ->
  let w = Region.wrap_ghost in
  match Location.unwrap e with
  | E_variable v -> EVar (w (Format.asprintf "%a" AST.Variable.pp v))
  | E_binary_op { operator; left; right } ->
    let binop op : 'a CST.wrap CST.bin_op CST.reg =
      w @@ CST.{ op; arg1 = left; arg2 = right }
    in
    (match Location.unwrap operator with
    | PLUS -> EArith (Add (binop ghost_plus))
    | MINUS -> EArith (Sub (binop ghost_minus))
    | STAR -> EArith (Mult (binop ghost_times))
    | SLASH -> EArith (Div (binop ghost_slash))
    | PRCENT -> EArith (Mod (binop ghost_rem))
    | DAMPERSAND -> ELogic (BoolExpr (And (binop ghost_bool_and)))
    | LT -> ELogic (CompExpr (Lt (binop ghost_lt)))
    | GT -> ELogic (CompExpr (Gt (binop ghost_gt)))
    | GE -> ELogic (CompExpr (Geq (binop ghost_ge)))
    | LE -> ELogic (CompExpr (Leq (binop ghost_le)))
    | SEQ -> ELogic (CompExpr (Equal (binop ghost_eq)))
    | LTGT -> ELogic (CompExpr (Neq (binop ghost_ne)))
    | DPIPE -> ELogic (BoolExpr (Or (binop ghost_bool_or)))
    | DCOLON
    | WORD_LSL
    | WORD_LSR
    | WORD_LOR
    | WORD_LAND
    | WORD_LXOR
    | EX_MARK
    | WORD_XOR
    | CONTAINS
    | SHARP
    | WORD_OR
    | WORD_MOD
    | WORD_NOT
    | WORD_AND
    | DEQ
    | EQ_SLASH_EQ
    | CARET -> failwith "Impossible")
  | E_unary_op { operator; arg } ->
    let unop op : 'a CST.wrap CST.un_op CST.reg = w @@ CST.{ op; arg } in
    (match Location.unwrap operator with
    | MINUS -> EArith (Neg (unop ghost_minus))
    | WORD_NOT -> ELogic (BoolExpr (Not (unop ghost_bool_not)))
    | _ -> failwith "Impossible")
  | E_literal Literal_unit -> CST.EUnit (w (ghost_lpar, ghost_rpar))
  | E_literal (Literal_int x) -> CST.EArith (Int (w (Z.to_string x, x)))
  | E_literal (Literal_nat x) -> CST.EArith (Int (w (Z.to_string x, x)))
  | E_literal (Literal_string x) -> CST.EString (String (w @@ Ligo_string.extract x))
  | _ ->
    failwith
      (Format.asprintf
         "Can't decompile this node : \n%a"
         Sexp.pp_hum
         (AST.sexp_of_expr_
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            e))


and ty_expr : unit AST.ty_expr_ -> unit = fun _ -> ()
and bracket_compound = CST.Brackets (ghost_lbracket, ghost_rbracket)
and braces_compound = CST.Braces (ghost_lbrace, ghost_rbrace)
