open Ligo_helpers.Trace
open Ast_simplified
module Raw = Ligo_parser.AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let npseq_to_nelist (hd, tl) = hd, (List.map snd tl)
let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let type_constants = [
  ("unit", 0) ;
  ("nat", 0) ;
  ("int", 0) ;
  ("bool", 0) ;
  ("list", 1) ;
  ("option", 1) ;
  ("set", 1) ;
  ("map", 2) ;
]

let rec simpl_type_expression (t:Raw.type_expr) : type_expression result =
  match t with
  | TPar x -> simpl_type_expression x.value.inside
  | TAlias v -> (
      match List.assoc_opt v.value type_constants with
      | Some 0 ->
          ok @@ Type_constant (v.value, [])
      | Some _ ->
          simple_fail "type constructor with wrong number of args"
      | None ->
          ok @@ Type_variable v.value
    )
  | TApp x ->
      let (name, tuple) = x.value in
      let lst = npseq_to_list tuple.value.inside in
      let%bind _ = match List.assoc_opt name.value type_constants with
        | Some n when n = List.length lst -> ok ()
        | Some _ -> simple_fail "type constructor with wrong number of args"
        | None -> simple_fail "unrecognized type constants" in
      let%bind lst' = bind_list @@ List.map simpl_type_expression lst in
      ok @@ Type_constant (name.value, lst')
  | TProd p ->
      let%bind tpl = simpl_list_type_expression
        @@ npseq_to_list p.value in
      ok tpl
  | TRecord r ->
      let aux = fun (x, y) -> let%bind y = simpl_type_expression y in ok (x, y) in
      let%bind lst = bind_list
        @@ List.map aux
        @@ List.map (fun (x:Raw.field_decl Raw.reg) -> (x.value.field_name.value, x.value.field_type))
        @@ npseq_to_list r.value.field_decls in
      let m = List.fold_left (fun m (x, y) -> SMap.add x y m) SMap.empty lst in
      ok @@ Type_record m
  | TSum s ->
      let aux (v:Raw.variant Raw.reg) =
        let%bind te = simpl_list_type_expression
          @@ npseq_to_list v.value.product.value in
        ok (v.value.constr.value, te)
      in
      let%bind lst = bind_list
        @@ List.map aux
        @@ npseq_to_list s.value in
      let m = List.fold_left (fun m (x, y) -> SMap.add x y m) SMap.empty lst in
      ok @@ Type_sum m

and simpl_list_type_expression (lst:Raw.type_expr list) : type_expression result =
  match lst with
  | [] -> assert false
  | [hd] -> simpl_type_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_type_expression lst in
      ok @@ Type_tuple lst

let constants = [
  ("get_force", 2) ;
]

let rec simpl_expression (t:Raw.expr) : ae result =
  let return x = ok @@ ae x in
  let simpl_projection = fun (p:Raw.projection) ->
    let var =
      let name = p.record_name.value in
      ae @@ Variable name in
    let path = p.field_path in
    let path' =
      let aux (s:Raw.selection) =
        match s with
        | FieldName property -> Record_access property.value
        | Component index -> Tuple_access (Z.to_int (snd index.value))
      in
      List.map aux @@ npseq_to_list path in
    ok @@ ae @@ Accessor (var, path')
  in
  match t with
  | EVar c ->
      if c.value = "unit"
      then ok @@ ae @@ Literal Unit
      else ok @@ ae @@ Variable c.value
  | ECall x -> (
      let (name, args) = x.value in
      let f = name.value in
      let args' = npseq_to_list args.value.inside in
      match List.assoc_opt f constants with
      | None ->
          let%bind arg = simpl_list_expression args' in
          ok @@ ae @@ Application (ae @@ Variable f, arg)
      | Some arity ->
          let%bind _arity =
            trace (simple_error "wrong arity for constants") @@
            Assert.assert_equal_int arity (List.length args') in
          let%bind lst = bind_map_list simpl_expression args' in
          ok @@ ae @@ Constant (f, lst)
    )
  | EPar x -> simpl_expression x.value.inside
  | EUnit _ -> ok @@ ae @@ Literal Unit
  | EBytes x -> ok @@ ae @@ Literal (Bytes (Bytes.of_string @@ fst x.value))
  | ETuple tpl ->
      let (Raw.TupleInj tpl') = tpl in
      simpl_list_expression
      @@ npseq_to_list tpl'.value.inside
  | ERecord (RecordInj r) ->
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = simpl_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
        @@ npseq_to_list r.value.fields in
      let aux prev (k, v) = SMap.add k v prev in
      ok @@ ae @@ Record (List.fold_left aux SMap.empty fields)
  | EProj p' -> (
      let p = p'.value in
      simpl_projection p
    )
  | EConstr (ConstrApp c) ->
      let (c, args) = c.value in
      let%bind arg =
        simpl_list_expression
        @@ npseq_to_list args.value.inside in
      ok @@ ae @@ Constructor (c.value, arg)
  | EConstr (SomeApp a) ->
      let (_, args) = a.value in
      let%bind arg =
        simpl_list_expression
        @@ npseq_to_list args.value.inside in
      ok @@ ae @@ Constant ("SOME", [arg])
  | EConstr (NoneExpr n) ->
      let type_expr = n.value.inside.opt_type in
      let%bind type_expr' = simpl_type_expression type_expr in
      ok @@ annotated_expression (Constant ("NONE", [])) (Some (Combinators.t_option type_expr'))
  | EArith (Add c) ->
      simpl_binop "ADD" c.value
  | EArith (Int n) ->
      let n = Z.to_int @@ snd @@ n.value in
      ok @@ ae @@ Literal (Number n)
  | EArith _ -> simple_fail "arith: not supported yet"
  | EString (String s) ->
      ok @@ ae @@ Literal (String s.value)
  | EString _ -> simple_fail "string: not supported yet"
  | ELogic l -> simpl_logic_expression l
  | EList _ -> simple_fail "list: not supported yet"
  | ESet _ -> simple_fail "set: not supported yet"
  | ECase c ->
      let%bind e = simpl_expression c.value.expr in
      let%bind lst =
        let aux (x:Raw.case_clause_expr) =
          let%bind expr = simpl_expression x.expr in
          ok (x.pattern, expr) in
        bind_list
        @@ List.map aux
        @@ List.map get_value
        @@ npseq_to_list c.value.cases_expr.value in
      let%bind cases = simpl_cases lst in
      ok @@ ae @@ Matching_expr (e, cases)
  | EMap (MapInj mi) ->
      let%bind lst =
        let lst = List.map get_value @@ pseq_to_list mi.value.elements in
        let aux : Raw.binding -> (ae * ae) result = fun b ->
          let%bind src = simpl_expression b.source in
          let%bind dst = simpl_expression b.image in
          ok (src, dst) in
        bind_map_list aux lst in
      return (Map lst)
  | EMap (MapLookUp lu) ->
      let%bind path = match lu.value.path with
        | Name v -> return (Variable v.value)
        | Path p -> simpl_projection p.value
      in
      let%bind index = simpl_expression lu.value.index.value.inside in
      return (LookUp (path, index))

and simpl_logic_expression (t:Raw.logic_expr) : ae result =
  match t with
  | BoolExpr (False _) ->
      ok @@ ae @@ Literal (Bool false)
  | BoolExpr (True _) ->
      ok @@ ae @@ Literal (Bool true)
  | BoolExpr (Or b) ->
      simpl_binop "OR" b.value
  | BoolExpr (And b) ->
      simpl_binop "AND" b.value
  | BoolExpr (Not b) ->
      simpl_unop "NOT" b.value
  | CompExpr (Lt c) ->
      simpl_binop "LT" c.value
  | CompExpr (Gt c) ->
      simpl_binop "GT" c.value
  | CompExpr (Leq c) ->
      simpl_binop "LE" c.value
  | CompExpr (Geq c) ->
      simpl_binop "GE" c.value
  | CompExpr (Equal c) ->
      simpl_binop "EQ" c.value
  | CompExpr (Neq c) ->
      simpl_binop "NEQ" c.value

and simpl_binop (name:string) (t:_ Raw.bin_op) : ae result =
  let%bind a = simpl_expression t.arg1 in
  let%bind b = simpl_expression t.arg2 in
  ok @@ ae @@ Constant (name, [a;b])

and simpl_unop (name:string) (t:_ Raw.un_op) : ae result =
  let%bind a = simpl_expression t.arg in
  ok @@ ae @@ Constant (name, [a])

and simpl_list_expression (lst:Raw.expr list) : ae result =
  match lst with
  | [] -> ok @@ ae @@ Literal Unit
  | [hd] -> simpl_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_expression lst in
      ok @@ ae @@ Tuple lst

and simpl_local_declaration (t:Raw.local_decl) : (instruction * named_expression) result =
  match t with
  | LocalData d -> simpl_data_declaration d
  | LocalLam _ -> simple_fail "no local lambdas yet"

and simpl_data_declaration (t:Raw.data_decl) : (instruction * named_expression) result =
  let return x = ok (Assignment x, x) in
  match t with
  | LocalVar x ->
      let x = x.value in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.var_type in
      let type_annotation = Some t in
      let%bind expression = simpl_expression x.init in
      return {name;annotated_expression={expression with type_annotation}}
  | LocalConst x ->
      let x = x.value in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.const_type in
      let type_annotation = Some t in
      let%bind expression = simpl_expression x.init in
      return {name;annotated_expression={expression with type_annotation}}


and simpl_param : Raw.param_decl -> named_type_expression result = fun t ->
  match t with
  | ParamConst c ->
      let c = c.value in
      let type_name = c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok { type_name ; type_expression }
  | ParamVar v ->
      let c = v.value in
      let type_name = c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok { type_name ; type_expression }

and simpl_declaration : Raw.declaration -> declaration result = fun t ->
  let open! Raw in
  match t with
  | TypeDecl x ->
      let {name;type_expr} : Raw.type_decl = x.value in
      let%bind type_expression = simpl_type_expression type_expr in
      ok @@ Type_declaration {type_name=name.value;type_expression}
  | ConstDecl x ->
      let {name;const_type;init} = x.value in
      let%bind expression = simpl_expression init in
      let%bind t = simpl_type_expression const_type in
      let type_annotation = Some t in
      ok @@ Constant_declaration {name=name.value;annotated_expression={expression with type_annotation}}
  | LambdaDecl (FunDecl x) ->
      let {name;param;ret_type;local_decls;block;return} : fun_decl = x.value in
      (match npseq_to_list param.value.inside with
       | [] -> simple_fail "function without parameters are not allowed"
       | [a] -> (
           let%bind input = simpl_param a in
           let name = name.value in
           let binder = input.type_name in
           let input_type = input.type_expression in
           let%bind local_declarations =
             let%bind tmp = bind_list
               @@ List.map simpl_local_declaration local_decls in
             ok (List.map fst tmp) in
           let%bind instructions = bind_list
             @@ List.map simpl_statement
             @@ npseq_to_list block.value.statements in
           let%bind result = simpl_expression return in
           let%bind output_type = simpl_type_expression ret_type in
           let body = local_declarations @ instructions in
           let decl =
             let expression = Lambda {binder ; input_type ; output_type ; result ; body } in
             let type_annotation = Some (Type_function (input_type, output_type)) in
             Constant_declaration {name;annotated_expression = {expression;type_annotation}}
           in
           ok decl
         )
       | lst -> (
           let%bind params = bind_map_list simpl_param lst in
           let input =
             let type_expression = Type_record (
                 SMap.of_list
                 @@ List.map (fun (x:named_type_expression) -> x.type_name, x.type_expression)
                   params
               ) in
             { type_name = "arguments" ; type_expression } in
           let binder = input.type_name in
           let input_type = input.type_expression in
           let%bind local_declarations =
             let%bind typed = bind_map_list simpl_local_declaration local_decls in
             ok (List.map fst typed)
           in
           let%bind output_type = simpl_type_expression ret_type in
           let%bind instructions = bind_list
             @@ List.map simpl_statement
             @@ npseq_to_list block.value.statements in
           let%bind (body, result) =
             let renamings =
               let aux ({type_name}:named_type_expression) : Rename.Value.renaming =
                 type_name, ("arguments", [Record_access type_name])
               in
               List.map aux params
             in
             let%bind r =
               let%bind tmp = simpl_expression return in
               Rename.Value.rename_annotated_expression renamings tmp
             in
             let%bind b =
               let tmp = local_declarations @ instructions in
               Rename.Value.rename_block renamings tmp
             in
             ok (b, r) in
           let decl =
             let expression = Lambda {binder ; input_type ; output_type ; result ; body } in
             let type_annotation = Some (Type_function (input_type, output_type)) in
             Constant_declaration {name = name.value;annotated_expression = {expression;type_annotation}}
           in
           ok decl
         )
      )
  | LambdaDecl (ProcDecl _) -> simple_fail "no proc declaration yet"
  | LambdaDecl (EntryDecl _)-> simple_fail "no entry point yet"

and simpl_statement : Raw.statement -> instruction result = fun s ->
  match s with
  | Instr i -> simpl_instruction i
  | Data d -> let%bind (i, _) = simpl_data_declaration d in ok i

and simpl_single_instruction : Raw.single_instr -> instruction result = fun t ->
  match t with
  | ProcCall _ -> simple_fail "no proc call"
  | Fail e ->
      let%bind expr = simpl_expression e.value.fail_expr in
      ok @@ Fail expr
  | Skip _ -> ok @@ Skip
  | Loop (While l) ->
      let l = l.value in
      let%bind cond = simpl_expression l.cond in
      let%bind body = simpl_block l.block.value in
      ok @@ Loop (cond, body)
  | Loop (For _) ->
      simple_fail "no for yet"
  | Cond c ->
      let c = c.value in
      let%bind expr = simpl_expression c.test in
      let%bind match_true = match c.ifso with
        | ClauseInstr i -> let%bind i = simpl_instruction i in ok [i]
        | ClauseBlock b -> simpl_statements @@ fst b.value.inside in
      let%bind match_false = match c.ifnot with
        | ClauseInstr i -> let%bind i = simpl_instruction i in ok [i]
        | ClauseBlock b -> simpl_statements @@ fst b.value.inside in
      ok @@ Matching_instr (expr, (Match_bool {match_true; match_false}))
  | Assign a ->
      let a = a.value in
      let%bind name = match a.lhs with
        | Path (Name v) -> ok v.value
        | _ -> simple_fail "no complex assignments yet"
      in
      let%bind annotated_expression = match a.rhs with
        | Expr e -> simpl_expression e
        | _ -> simple_fail "no weird assignments yet"
      in
      ok @@ Assignment {name ; annotated_expression}
  | Case_instr c ->
      let c = c.value in
      let%bind expr = simpl_expression c.expr in
      let%bind cases =
        let aux (x : Raw.case_clause_instr Raw.reg) =
          let%bind i = simpl_instruction_block x.value.instr in
          ok (x.value.pattern, i) in
        bind_list
        @@ List.map aux
        @@ npseq_to_list c.cases_instr.value in
      let%bind m = simpl_cases cases in
      ok @@ Matching_instr (expr, m)
  | RecordPatch r ->
      let r = r.value in
      let%bind record = match r.path with
        | Name v -> ok v.value
        | _ -> simple_fail "no complex assignments yet"
      in
      let%bind inj = bind_list
        @@ List.map (fun (x:Raw.field_assign) -> let%bind e = simpl_expression x.field_expr in ok (x.field_name.value, e))
        @@ List.map (fun (x:_ Raw.reg) -> x.value)
        @@ npseq_to_list r.record_inj.value.fields in
      ok @@ Record_patch (record, [], inj)
  | MapPatch _ -> simple_fail "no map patch yet"
  | SetPatch _ -> simple_fail "no set patch yet"
  | MapRemove _ -> simple_fail "no map remove yet"
  | SetRemove _ -> simple_fail "no set remove yet"

and simpl_cases : type a . (Raw.pattern * a) list -> a matching result = fun t ->
  let open Raw in
  let get_var (t:Raw.pattern) = match t with
    | PVar v -> ok v.value
    | _ -> simple_fail "not a var"
  in
  let%bind _assert =
    trace_strong (simple_error "only pattern with two cases supported now") @@
    Assert.assert_equal_int 2 (List.length t) in
  let ((pa, ba), (pb, bb)) = List.(hd t, hd @@ tl t) in
  let uncons p = match p with
    | PCons {value = (hd, _)} -> ok hd
    | _ -> simple_fail "uncons fail" in
  let%bind (pa, pb) = bind_map_pair uncons (pa, pb) in
  match (pa, ba), (pb, bb) with
  | (PFalse _, f), (PTrue _, t)
  | (PTrue _, t), (PFalse _, f) -> ok @@ Match_bool {match_true = t ; match_false = f}
  | (PSome v, some), (PNone _, none)
  | (PNone _, none), (PSome v, some) -> (
      let (_, v) = v.value in
      let%bind v = match v.value.inside with
        | PVar v -> ok v.value
        | _ -> simple_fail "complex none patterns not supported yet" in
      ok @@ Match_option {match_none = none ; match_some = (v, some) }
    )
  | (PCons c, cons), (PList (PNil _), nil)
  | (PList (PNil _), nil), (PCons c, cons) ->
      let%bind (a, b) =
        match c.value with
        | a, [(_, b)] ->
            let%bind a = get_var a in
            let%bind b = get_var b in
            ok (a, b)
        | _ -> simple_fail "complex list patterns not supported yet"
      in
      ok @@ Match_list {match_cons = (a, b, cons) ; match_nil = nil}
  | _ ->
      let error = simple_error "multi-level patterns not supported yet" in
      fail error

and simpl_instruction_block : Raw.instruction -> block result = fun t ->
  match t with
  | Single s -> let%bind i = simpl_single_instruction s in ok [ i ]
  | Block b -> simpl_block b.value

and simpl_instruction : Raw.instruction -> instruction result = fun t ->
  match t with
  | Single s -> simpl_single_instruction s
  | Block _ -> simple_fail "no block instruction yet"

and simpl_statements : Raw.statements -> block result = fun ss ->
  let lst = npseq_to_list ss in
  bind_map_list simpl_statement lst

and simpl_block : Raw.block -> block result = fun t ->
  simpl_statements t.statements

let simpl_program : Raw.ast -> program result = fun t ->
  bind_list @@ List.map simpl_declaration @@ nseq_to_list t.decl
