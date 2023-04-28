module CST = Cst.Cameligo
module AST = Ast_unified
open Simple_utils
open Lexing_cameligo.Token

let rec folder =
  let todo _ = failwith ("TODO" ^ __LOC__) in
  AST.Catamorphism.
    { expr
    ; ty_expr
    ; pattern
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
    | CARET -> EString (Cat (binop ghost_caret))
    | PLUS -> EArith (Add (binop ghost_plus))
    | MINUS -> EArith (Sub (binop ghost_minus))
    | STAR -> EArith (Mult (binop ghost_times))
    | SLASH -> EArith (Div (binop ghost_slash))
    | PRCENT -> EArith (Mod (binop ghost_mod))
    | DAMPERSAND -> ELogic (BoolExpr (And (binop ghost_bool_and)))
    | LT -> ELogic (CompExpr (Lt (binop ghost_lt)))
    | GT -> ELogic (CompExpr (Gt (binop ghost_gt)))
    | GE -> ELogic (CompExpr (Geq (binop ghost_ge)))
    | LE -> ELogic (CompExpr (Leq (binop ghost_le)))
    | SEQ -> ELogic (CompExpr (Equal (binop ghost_eq)))
    | LTGT -> ELogic (CompExpr (Neq (binop ghost_ne)))
    | DCOLON -> EList (ECons (binop ghost_cons))
    | WORD_LSL -> EArith (Lsl (binop ghost_lsl))
    | WORD_LSR -> EArith (Lsr (binop ghost_lsr))
    | WORD_LOR -> EArith (Lor (binop ghost_lor))
    | WORD_LAND -> EArith (Land (binop ghost_land))
    | WORD_LXOR -> EArith (Lxor (binop ghost_lxor))
    | DPIPE -> ELogic (BoolExpr (Or (binop ghost_or)))
    | EX_MARK
    | WORD_XOR
    | CONTAINS
    | SHARP
    | WORD_OR
    | WORD_MOD
    | WORD_NOT
    | WORD_AND
    | DEQ
    | EQ_SLASH_EQ -> failwith "Impossible")
  | E_unary_op { operator; arg } ->
    let unop op : 'a CST.wrap CST.un_op CST.reg = w @@ CST.{ op; arg } in
    (match Location.unwrap operator with
    | MINUS -> EArith (Neg (unop ghost_minus))
    | WORD_NOT -> ELogic (BoolExpr (Not (unop ghost_not)))
    | _ -> failwith "Impossible")
  | E_literal Literal_unit -> CST.EUnit (w (ghost_lpar, ghost_rpar))
  | E_literal (Literal_int x) -> CST.EArith (Int (w (Z.to_string x, x)))
  | E_literal (Literal_nat x) -> CST.EArith (Nat (w (Z.to_string x, x)))
  | E_literal (Literal_mutez x) -> CST.EArith (Mutez (w (Z.to_string x, Z.to_int64 x)))
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


and pattern : (CST.pattern, unit) AST.pattern_ -> CST.pattern =
 fun p ->
  let w = Region.wrap_ghost in
  match Location.unwrap p with
  | P_attr (_, p) -> p
  | P_unit -> PUnit (w (ghost_lpar, ghost_rpar))
  | P_typed (type_expr, pattern) ->
    (* should be this:
      PTyped (w CST.{ pattern; colon = ghost_colon; type_expr })
    but since pattern are only used for errors, we don't want them
    *)
    ignore type_expr;
    pattern
  | P_var v ->
    let variable = w (Format.asprintf "%a" AST.Variable.pp v) in
    PVar (w CST.{ variable; attributes = [] })
  | P_list (List lst) ->
    let compound = Some bracket_compound in
    let terminator = Some ghost_semi in
    let elements = Utils.list_to_sepseq lst ghost_semi in
    PList (PListComp (w CST.{ compound; terminator; elements }))
  | P_list (Cons (l, r)) -> PList (PCons (w (l, ghost_cons, r)))
  | P_variant (l, p_opt) ->
    let constr = w @@ AST.Label.to_string l in
    PConstr (w (constr, p_opt))
  | P_tuple lst ->
    let lst = Utils.list_to_nsepseq_opt lst ghost_comma in
    let lst =
      match lst with
      | None -> failwith "Impossible"
      | Some lst -> lst
    in
    PPar (w CST.{ lpar = ghost_lpar; inside = PTuple (w lst); rpar = ghost_rpar })
  | P_pun_record lst ->
    let compound = Some braces_compound in
    let terminator = Some ghost_semi in
    let ne_elements : (CST.field_pattern CST.reg, _) Utils.nsepseq =
      let lst : CST.field_pattern CST.reg list =
        List.map lst ~f:(function
            | Complete (l, p) ->
              w
              @@ CST.
                   { field_name = w (AST.Label.to_string l); eq = ghost_eq; pattern = p }
            | Punned { wrap_content = l; _ } ->
              w
              @@ CST.
                   { field_name = w (AST.Label.to_string l)
                   ; eq = ghost_eq
                   ; pattern =
                       PVar
                         (w
                            CST.{ variable = w @@ AST.Label.to_string l; attributes = [] })
                   })
      in
      let lst = Utils.list_to_nsepseq_opt lst ghost_semi in
      let lst =
        match lst with
        | None -> failwith "Impossible"
        | Some lst -> lst
      in
      lst
    in
    PRecord (w CST.{ compound; terminator; ne_elements; attributes = [] })
  | p when AST.pattern_is_not_initial p -> assert false
  | _ -> assert false


and ty_expr : unit AST.ty_expr_ -> unit = fun _ -> ()
and bracket_compound = CST.Brackets (ghost_lbracket, ghost_rbracket)
and braces_compound = CST.Braces (ghost_lbrace, ghost_rbrace)
