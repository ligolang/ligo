type error =
  IntErr   of LexToken.int_err
| IdentErr of LexToken.ident_err
| NatErr   of LexToken.nat_err
| SymErr   of LexToken.sym_err
| KwdErr   of LexToken.kwd_err

let rec first_of_expr = function
  ECase {value; _} ->
   (match LexToken.mk_kwd "switch" value.kwd_match with
      Error e  -> Error (KwdErr e)
    | Ok token -> Ok token)
| ECond {value; _} ->
   (match LexToken.mk_kwd "if" value.kwd_if with
      Error e  -> Error (KwdErr e)
    | Ok token -> Ok token)
| EPar {value; _} ->
   (match LexToken.mk_sym "(" value.lpar with
      Error e  -> Error (SymErr e)
    | Ok token -> Ok token)
| EAnnot  {value; _} ->
   (match LexToken.mk_sym "(" value.lpar with
      Error e  -> Error (SymErr e)
    | Ok token -> Ok token)
| EUnit {value=opening, _; _} ->
   (match LexToken.mk_sym "(" opening with
      Error e  -> Error (SymErr e)
    | Ok token -> Ok token)
| EBytes b ->
    Ok (LexToken.mk_bytes (fst b.value) b.region)
| EVar v ->
   (match LexToken.mk_ident v.value v.region with
      Error e  -> Error (IdentErr e)
    | Ok token -> Ok token)
| ESeq {value; _} ->
    let opening =
      match value.compound with
        BeginEnd (opening, _)
      | Braces (opening, _)
      | Brackets (opening, _) -> opening
    in (match LexToken.mk_sym "{" opening with
         Error e  -> Error (SymErr e)
       | Ok token -> Ok token)
| EProj {value; _} ->
   let structure = value.struct_name in
   (match LexToken.mk_ident structure.value structure.region with
      Error e  -> Error (IdentErr e)
    | Ok token -> Ok token)
| EFun {value; _} ->
   (match LexToken.mk_kwd "fun" value.kwd_fun with
      Error e  -> Error (KwdErr e)
    | Ok token -> Ok token)
| _ -> failwith "TODO"
(*

| ELogic  expr -> first_of_logic_expr expr
| EArith  expr -> first_of_arith_expr expr
| EString expr -> first_of_string_expr expr
| EList   expr -> first_of_list_expr expr
| EConstr expr -> first_of_constr_expr expr
| ECall {value=expr,_; _} -> first_of_expr expr
| ERecord {value; _} -> (*field_assign reg ne_injection *)
| ETuple {value; _} -> (* (expr, comma) nsepseq *)
| ELetIn {value; _} -> first_of_let_in value
 *)
