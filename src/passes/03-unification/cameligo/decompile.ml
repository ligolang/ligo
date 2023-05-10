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
  | E_variable v -> E_Var (ghost_ident (Format.asprintf "%a" AST.Variable.pp v))
  | E_binary_op { operator; left; right } ->
    let binop op : 'a CST.wrap CST.bin_op CST.reg =
      w @@ CST.{ op; arg1 = left; arg2 = right }
    in
    (match Location.unwrap operator with
    | CARET -> E_Cat (binop ghost_caret)
    | PLUS -> E_Add (binop ghost_plus)
    | MINUS -> E_Sub (binop ghost_minus)
    | STAR -> E_Mult (binop ghost_times)
    | SLASH -> E_Div (binop ghost_slash)
    | PRCENT -> E_Mod (binop ghost_mod)
    | DAMPERSAND -> E_And (binop ghost_bool_and)
    | LT -> E_Lt (binop ghost_lt)
    | GT -> E_Gt (binop ghost_gt)
    | GE -> E_Geq (binop ghost_ge)
    | LE -> E_Leq (binop ghost_le)
    | SEQ -> E_Equal (binop ghost_eq)
    | LTGT -> E_Neq (binop ghost_ne)
    | DCOLON -> E_Cons (binop ghost_cons)
    | WORD_LSL -> E_Lsl (binop ghost_lsl)
    | WORD_LSR -> E_Lsr (binop ghost_lsr)
    | WORD_LOR -> E_Lor (binop ghost_lor)
    | WORD_LAND -> E_Land (binop ghost_land)
    | WORD_LXOR -> E_Lxor (binop ghost_lxor)
    | DPIPE -> E_Or (binop ghost_or)
    | EX_MARK
    | WORD_XOR
    | CONTAINS
    | SHARP
    | WORD_OR
    | WORD_MOD
    | WORD_NOT
    | WORD_AND
    | DEQ
    | EQ_SLASH_EQ -> failwith "Decompiler: EQ_SLASH_EQ: Impossible")
  | E_unary_op { operator; arg } ->
    let unop op : 'a CST.wrap CST.un_op CST.reg = w @@ CST.{ op; arg } in
    (match Location.unwrap operator with
    | MINUS -> E_Neg (unop ghost_minus)
    | WORD_NOT -> E_Not (unop ghost_not)
    | _ -> failwith "Decompiler: unop: Impossible")
  | E_literal Literal_unit -> CST.E_Unit (w (ghost_lpar, ghost_rpar))
  | E_literal (Literal_int x) -> CST.E_Int (ghost_int x)
  | E_literal (Literal_nat x) -> CST.E_Nat (ghost_nat x)
  | E_literal (Literal_mutez x) -> CST.E_Mutez (ghost_mutez @@ Z.to_int64 x)
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
  | P_attr ({ key; value }, p) ->
    P_Attr (ghost_attr key (Option.map ~f:(fun x -> Attr.String x) value), p)
    (* ^ XXX Attr.String or Attr.Ident? *)
  | P_unit -> P_Unit (w (ghost_lpar, ghost_rpar))
  | P_typed (type_expr, pattern) ->
    (* should be this:
      PTyped (w CST.{ pattern; colon = ghost_colon; type_expr })
    but since pattern are only used for errors, we don't want them
    *)
    ignore type_expr;
    pattern
  | P_var v -> P_Var (ghost_ident @@ Format.asprintf "%a" AST.Variable.pp v)
  | P_list (List lst) ->
    let elements = Utils.list_to_sepseq lst ghost_semi in
    P_List
      (w CST.{ lbracket = ghost_lbracket; inside = elements; rbracket = ghost_rbracket })
  | P_list (Cons (l, r)) -> P_Cons (w (l, ghost_cons, r))
  | P_variant (l, p_opt) ->
    let constr = CST.P_Ctor (ghost_ident @@ AST.Label.to_string l) in
    (match p_opt with
    | None -> constr
    | Some pat -> P_App (w (constr, Some pat)))
  | P_tuple lst ->
    let lst = Utils.list_to_nsepseq_opt lst ghost_comma in
    let lst =
      match lst with
      | None -> failwith "Decompiler: empty P_tuple"
      | Some lst -> lst
    in
    P_Par (w CST.{ lpar = ghost_lpar; inside = P_Tuple (w lst); rpar = ghost_rpar })
  | P_pun_record lst ->
    let ne_elements =
      let lst : (CST.field_name, CST.equal, CST.pattern) CST.field list =
        List.map lst ~f:(function
            | Complete (l, p) ->
              CST.Complete
                (w
                @@ CST.
                     { field_lhs = ghost_ident (AST.Label.to_string l)
                     ; field_lens = ghost_eq
                     ; field_rhs = p
                     ; attributes = []
                     })
            | Punned { wrap_content = l; _ } ->
              (* XXX do we need to extract attributes here?
                              Why this CST node has a separate record for attributes at all?*)
              CST.Punned
                (w @@ CST.{ attributes = []; pun = ghost_ident (AST.Label.to_string l) }))
      in
      Utils.list_to_sepseq lst ghost_semi
    in
    P_Record
      (w CST.{ lbrace = ghost_lbrace; inside = ne_elements; rbrace = ghost_rbrace })
  | P_ctor c -> P_Ctor (CST.Wrap.ghost @@ AST.Label.to_string c)
  | P_app (c, arg) -> P_App (w (c, arg))
  | p when AST.pattern_is_not_initial p ->
    (* in the case of compilation CST's to CST, this can happen*) assert false
  | _ ->
    failwith
      (Format.asprintf
         "Can't decompile this node : \n%a"
         Sexp.pp_hum
         (AST.sexp_of_pattern_ (fun _ -> Sexp.Atom "xx") (fun _ -> Sexp.Atom "xx") p))


and ty_expr : unit AST.ty_expr_ -> unit = fun _ -> ()
