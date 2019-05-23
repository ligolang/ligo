[@@@warning "-45"]

open Trace
open Ast_simplified

module Raw = Parser.Ligodity.AST
module SMap = Map.String
module Option = Simple_utils.Option

open Combinators

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let npseq_to_nelist (hd, tl) = hd, (List.map snd tl)
let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let type_constants = Operators.Simplify.type_constants
let constants = Operators.Simplify.constants

let rec simpl_type_expression : Raw.type_expr -> type_expression result =
  function
  | TPar x -> simpl_type_expression x.value.inside
  | TAlias v -> (
      match List.assoc_opt v.value type_constants with
      | Some 0 ->
          ok @@ T_constant (v.value, [])
      | Some _ ->
          simple_fail "type constructor with wrong number of args"
      | None ->
          ok @@ T_variable v.value
    )
  | TFun x -> (
      let%bind (a , b) =
        let (a , _ , b) = x.value in
        bind_map_pair simpl_type_expression (a , b) in
      ok @@ T_function (a , b)
    )
  | TApp x ->
      let (name, tuple) = x.value in
      let lst = npseq_to_list tuple.value.inside in
      let%bind _ = match List.assoc_opt name.value type_constants with
        | Some n when n = List.length lst -> ok ()
        | Some _ -> simple_fail "type constructor with wrong number of args"
        | None -> simple_fail "unrecognized type constants" in
      let%bind lst' = bind_list @@ List.map simpl_type_expression lst in
      ok @@ T_constant (name.value, lst')
  | TProd p ->
      let%bind tpl = simpl_list_type_expression
        @@ npseq_to_list p.value in
      ok tpl
  | TRecord r ->
      let aux = fun (x, y) -> let%bind y = simpl_type_expression y in ok (x, y) in
      let%bind lst = bind_list
        @@ List.map aux
        @@ List.map (fun (x:Raw.field_decl Raw.reg) -> (x.value.field_name.value, x.value.field_type))
        @@ pseq_to_list r.value.elements in
      let m = List.fold_left (fun m (x, y) -> SMap.add x y m) SMap.empty lst in
      ok @@ T_record m
  | TSum s ->
      let aux (v:Raw.variant Raw.reg) =
        let args =
          match v.value.args with
            None -> []
          | Some (_, cartesian) ->
              npseq_to_list cartesian.value in
        let%bind te = simpl_list_type_expression
          @@ args in
        ok (v.value.constr.value, te)
      in
      let%bind lst = bind_list
        @@ List.map aux
        @@ npseq_to_list s.value in
      let m = List.fold_left (fun m (x, y) -> SMap.add x y m) SMap.empty lst in
      ok @@ T_sum m

and simpl_list_type_expression (lst:Raw.type_expr list) : type_expression result =
  match lst with
  | [] -> assert false
  | [hd] -> simpl_type_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_type_expression lst in
      ok @@ T_tuple lst

let rec simpl_expression :
  ?te_annot:type_expression -> Raw.expr -> ae result = fun ?te_annot t ->
  let return x = ok @@ make_e_a ?type_annotation:te_annot x in
  let simpl_projection = fun (p:Raw.projection) ->
    let var =
      let name = p.struct_name.value in
      make_e_a @@ E_variable name in
    let path = p.field_path in
    let path' =
      let aux (s:Raw.selection) =
        match s with
        | FieldName property -> Access_record property.value
        | Component index ->
            let index = index.value.inside in
            Access_tuple (Z.to_int (snd index.value))
      in
      List.map aux @@ npseq_to_list path in
    return @@ E_accessor (var, path')
  in
  let mk_let_in binder rhs result =
    E_let_in {binder; rhs; result} in

  match t with
  | Raw.ELetIn e -> (
      let Raw.{binding; body; _} = e.value in
      let Raw.{pattern; lhs_type; let_rhs; _} = binding in
      let%bind type_annotation = bind_map_option
          (fun (_,type_expr) -> simpl_type_expression type_expr)
          lhs_type in
      let%bind rhs = simpl_expression ?te_annot:type_annotation let_rhs in
      let%bind body = simpl_expression body in
      match pattern with
        Raw.PVar v -> return (mk_let_in v.value rhs body)
      | _ -> let%bind case = simpl_cases [(pattern, body)]
            in return (E_matching (rhs, case))
    )
  | Raw.EAnnot a -> (
      let (expr , type_expr) = a.value in
      match te_annot with
      | None -> (
          let%bind te_annot = simpl_type_expression type_expr in
          let%bind expr' = simpl_expression ~te_annot expr in
          ok expr'
        )
      | Some _ -> simple_fail "no double annotation"
    )
  | EVar c -> (
      let c' = c.value in
      match List.assoc_opt c' constants with
      | None -> return @@ E_variable c.value
      | Some 0 -> return @@ E_constant (c' , [])
      | Some n -> (
          let error =
            let title () = "non nullary constant without parameters" in
            let content () = Format.asprintf "%s (%d)" c' n in
            error title content in
          fail error
        )
    )
  | ECall x -> (
      let (e1, e2) = x.value in
      let%bind args = bind_map_list simpl_expression (nseq_to_list e2) in
      match e1 with
        EVar f ->
          (match List.assoc_opt f.value constants with
          | None ->
              let%bind arg = simpl_tuple_expression (nseq_to_list e2) in
              return @@ E_application (make_e_a @@ E_variable f.value, arg)
          | Some arity ->
              let%bind _arity =
                trace (simple_error "wrong arity for constants") @@
                Assert.assert_equal_int arity (List.length args) in
              return @@ E_constant (f.value, args))
      | e1 -> let%bind e1' = simpl_expression e1 in
             let%bind arg = simpl_tuple_expression (nseq_to_list e2) in
             return @@ E_application (e1', arg)
    )
  | EPar x -> simpl_expression ?te_annot x.value.inside
  | EUnit _ -> return @@ E_literal Literal_unit
  | EBytes x -> return @@ E_literal (Literal_bytes (Bytes.of_string @@ fst x.value))
  | ETuple tpl -> simpl_tuple_expression ?te_annot @@ (npseq_to_list tpl.value)
  | ERecord r ->
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = simpl_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
        @@ pseq_to_list r.value.elements in
      let aux prev (k, v) = SMap.add k v prev in
      return @@ E_record (List.fold_left aux SMap.empty fields)
  | EProj p' -> (
      let p = p'.value in
      simpl_projection p
    )
  | EConstr c ->
      let (c, args) = c.value in
      let args =
        match args with
          None -> []
        | Some arg -> [arg] in
      let%bind arg = simpl_tuple_expression @@ args in
      return @@ E_constructor (c.value, arg)
  | EArith (Add c) ->
      simpl_binop ?te_annot "ADD" c.value
  | EArith (Sub c) ->
      simpl_binop ?te_annot "SUB" c.value
  | EArith (Mult c) ->
      simpl_binop ?te_annot "TIMES" c.value
  | EArith (Div c) ->
      simpl_binop ?te_annot "DIV" c.value
  | EArith (Mod c) ->
      simpl_binop ?te_annot "MOD" c.value
  | EArith (Int n) ->
      let n = Z.to_int @@ snd @@ n.value in
      return @@ E_literal (Literal_int n)
  | EArith (Nat n) ->
      let n = Z.to_int @@ snd @@ n.value in
      return @@ E_literal (Literal_nat n)
  | EArith (Mtz n) ->
      let n = Z.to_int @@ snd @@ n.value in
      return @@ E_literal (Literal_tez n)
  | EArith _ -> simple_fail "arith: not supported yet"
  | EString (String s) ->
      let s' =
        let s = s.value in
        String.(sub s 1 ((length s) - 2))
      in
      return @@ E_literal (Literal_string s')
  | EString _ -> simple_fail "string: not supported yet"
  | ELogic l -> simpl_logic_expression ?te_annot l
  | EList l -> simpl_list_expression ?te_annot l
  | ECase c ->
      let%bind e = simpl_expression c.value.expr in
      let%bind lst =
        let aux (x : Raw.expr Raw.case_clause) =
          let%bind expr = simpl_expression x.rhs in
          ok (x.pattern, expr) in
        bind_list
        @@ List.map aux
        @@ List.map get_value
        @@ npseq_to_list c.value.cases.value in
      let%bind cases = simpl_cases lst in
      return @@ E_matching (e, cases)
  | _ -> failwith "XXX" (* TODO *)

and simpl_logic_expression ?te_annot (t:Raw.logic_expr) : annotated_expression result =
  let return x = ok @@ make_e_a ?type_annotation:te_annot x in
  match t with
  | BoolExpr (False _) ->
      return @@ E_literal (Literal_bool false)
  | BoolExpr (True _) ->
      return @@ E_literal (Literal_bool true)
  | BoolExpr (Or b) ->
      simpl_binop ?te_annot "OR" b.value
  | BoolExpr (And b) ->
      simpl_binop ?te_annot "AND" b.value
  | BoolExpr (Not b) ->
      simpl_unop ?te_annot "NOT" b.value
  | CompExpr (Lt c) ->
      simpl_binop ?te_annot "LT" c.value
  | CompExpr (Gt c) ->
      simpl_binop ?te_annot "GT" c.value
  | CompExpr (Leq c) ->
      simpl_binop ?te_annot "LE" c.value
  | CompExpr (Geq c) ->
      simpl_binop ?te_annot "GE" c.value
  | CompExpr (Equal c) ->
      simpl_binop ?te_annot "EQ" c.value
  | CompExpr (Neq c) ->
      simpl_binop ?te_annot "NEQ" c.value

and simpl_list_expression ?te_annot (t:Raw.list_expr) : annotated_expression result =
  let return x = ok @@ make_e_a ?type_annotation:te_annot x in
  match t with
  | Cons c ->
      simpl_binop ?te_annot "CONS" c.value
  | List lst ->
      let%bind lst' =
        bind_map_list simpl_expression @@
        pseq_to_list lst.value.elements in
      return @@ E_list lst'

and simpl_binop ?te_annot (name:string) (t:_ Raw.bin_op) : annotated_expression result =
  let return x = ok @@ make_e_a ?type_annotation:te_annot x in
  let%bind a = simpl_expression t.arg1 in
  let%bind b = simpl_expression t.arg2 in
  return @@ E_constant (name, [a;b])

and simpl_unop ?te_annot (name:string) (t:_ Raw.un_op) : annotated_expression result =
  let return x = ok @@ make_e_a ?type_annotation:te_annot x in
  let%bind a = simpl_expression t.arg in
  return @@ E_constant (name, [a])

and simpl_tuple_expression ?te_annot (lst:Raw.expr list) : annotated_expression result =
  let return x = ok @@ make_e_a ?type_annotation:te_annot x in
  match lst with
  | [] -> return @@ E_literal Literal_unit
  | [hd] -> simpl_expression ?te_annot hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_expression lst in
      return @@ E_tuple lst

and simpl_local_declaration (t:Raw.local_decl) : (instruction * named_expression) result =
  match t with
  | LocalData d -> simpl_data_declaration d
  | LocalLam l -> simpl_lambda_declaration l

and simpl_lambda_declaration : Raw.lambda_decl -> (instruction * named_expression) result =
  fun l ->
  match l with
  | FunDecl f ->
      let%bind e = simpl_fun_declaration (f.value) in
      ok (I_assignment e, e)
  | ProcDecl _ -> simple_fail "no local procedure yet"
  | EntryDecl _ -> simple_fail "no local entry-point yet"

and simpl_data_declaration (t:Raw.data_decl) : (instruction * named_expression) result =
  let return x = ok (I_assignment x, x) in
  match t with
  | LocalVar x ->
      let x = x.value in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.var_type in
      let%bind annotated_expression = simpl_expression ~te_annot:t x.init in
      return {name;annotated_expression}
  | LocalConst x ->
      let x = x.value in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.const_type in
      let%bind annotated_expression = simpl_expression ~te_annot:t x.init in
      return {name;annotated_expression}


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

and simpl_fun_declaration : Raw.fun_decl -> named_expression result = fun x ->
  let open! Raw in
  let {name;param;ret_type;local_decls;block;return} : fun_decl = x in
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
       let expression = E_lambda {binder ; input_type = Some input_type;
                                  output_type = Some input_type; result ; body } in
       let type_annotation = Some (T_function (input_type, output_type)) in
       ok {name;annotated_expression = {expression;type_annotation}}
     )
   | lst -> (
       let arguments_name = "arguments" in
       let%bind params = bind_map_list simpl_param lst in
       let input =
         let aux = fun x -> x.type_expression in
         let type_expression = T_tuple (List.map aux params) in
         { type_name = arguments_name ; type_expression } in
       let binder = input.type_name in
       let input_type = input.type_expression in
       let tpl_declarations =
         let aux = fun i (x:named_type_expression) ->
           let ass = I_assignment {
             name = x.type_name ;
             annotated_expression = {
               expression = E_accessor ({
                   expression = E_variable arguments_name ;
                   type_annotation = Some input.type_expression ;
                 } , [ Access_tuple i ] ) ;
               type_annotation = Some (x.type_expression) ;
             }
           } in
           ass
         in
         List.mapi aux params in
       let%bind local_declarations =
         let%bind typed = bind_map_list simpl_local_declaration local_decls in
         ok (List.map fst typed)
       in
       let%bind output_type = simpl_type_expression ret_type in
       let%bind instructions = bind_list
         @@ List.map simpl_statement
         @@ npseq_to_list block.value.statements in

       let body = tpl_declarations @ local_declarations @ instructions in
       let%bind result = simpl_expression return in
       let expression = E_lambda {binder ; input_type = Some input_type;
                                  output_type = Some output_type; result ; body } in
       let type_annotation = Some (T_function (input_type, output_type)) in
       ok {name = name.value;annotated_expression = {expression;type_annotation}}
     )
  )
and simpl_declaration : Raw.declaration -> declaration Location.wrap result = fun t ->
  let open! Raw in
  let loc : 'a . 'a Raw.reg -> _ -> _ = fun x v -> Location.wrap ~loc:(File x.region) v in
  match t with
  | TypeDecl x ->
      let {name;type_expr} : Raw.type_decl = x.value in
      let%bind type_expression = simpl_type_expression type_expr in
      ok @@ loc x @@ Declaration_type {type_name=name.value;type_expression}
  | LetEntry _ -> simple_fail "no entry point yet"
  | Let x ->
      let _, binding = x.value in
      let {pattern; lhs_type; let_rhs} = binding in
      let%bind type_annotation = bind_map_option
        (fun (_,type_expr) -> simpl_type_expression type_expr)
        lhs_type in
      let%bind rhs = simpl_expression ?te_annot:type_annotation let_rhs in
      match pattern with
        Raw.PVar v ->
          let name = v.value in
          let named_expr = {name; annotated_expression=rhs}
          in return (Declaration_constant named_expr)
      | _ -> let%bind case = simpl_cases [(pattern, rhs)]
            in return (Declaration_constant (E_matching (rhs, case)))

(*
  | ConstDecl x ->
      let simpl_const_decl = fun {name;const_type;init} ->
        let%bind expression = simpl_expression init in
        let%bind t = simpl_type_expression const_type in
        let type_annotation = Some t in
        ok @@ Declaration_constant {name=name.value;annotated_expression={expression with type_annotation}}
      in
      bind_map_location simpl_const_decl (Location.lift_region x)
  | LambdaDecl (FunDecl x) ->
      let aux f x =
        let%bind x' = f x in
        ok @@ Declaration_constant x' in
      bind_map_location (aux simpl_fun_declaration) (Location.lift_region x)
  | LambdaDecl (ProcDecl _) -> simple_fail "no proc declaration yet"
*)

and simpl_statement : Raw.statement -> instruction result = fun s ->
  match s with
  | Instr i -> simpl_instruction i
  | Data d -> let%bind (i, _) = simpl_data_declaration d in ok i

and simpl_single_instruction : Raw.single_instr -> instruction result = fun t ->
  match t with
  | ProcCall _ -> simple_fail "no proc call"
  | Fail e ->
      let%bind expr = simpl_expression e.value.fail_expr in
      ok @@ I_do (untyped_expression @@ E_failwith expr)
  | Skip _ -> ok @@ I_skip
  | Loop (While l) ->
      let l = l.value in
      let%bind cond = simpl_expression l.cond in
      let%bind body = simpl_block l.block.value in
      ok @@ I_loop (cond, body)
  | Loop (For _) ->
      simple_fail "no for yet"
  | Cond c ->
      let c = c.value in
      let%bind expr = simpl_expression c.test in
      let%bind match_true = match c.ifso with
        | ClauseInstr i -> simpl_instruction_block i
        | ClauseBlock b -> simpl_statements @@ fst b.value.inside in
      let%bind match_false = match c.ifnot with
        | ClauseInstr i -> simpl_instruction_block i
        | ClauseBlock b -> simpl_statements @@ fst b.value.inside in
      ok @@ I_matching (expr, (Match_bool {match_true; match_false}))
  | Assign a -> (
      let a = a.value in
      let%bind value_expr = match a.rhs with
        | Expr e -> simpl_expression e
        | NoneExpr _ -> simple_fail "no none assignments yet"
      in
      match a.lhs with
        | Path path -> (
            let (name , path') = simpl_path path in
            match List.rev_uncons_opt path' with
            | None -> (
                ok @@ I_assignment {name ; annotated_expression = value_expr}
              )
            | Some (hds , last) -> (
                match last with
                | Access_record property -> ok @@ I_record_patch (name , hds , [(property , value_expr)])
                | Access_tuple index -> ok @@ I_tuple_patch (name , hds , [(index , value_expr)])
                | _ -> simple_fail "no map assignment in this weird case yet"
              )
          )
        | MapPath v -> (
            let v' = v.value in
            let%bind name = match v'.path with
              | Name name -> ok name
              | _ -> simple_fail "no complex map assignments yet" in
            let%bind key_expr = simpl_expression v'.index.value.inside in
            let old_expr = make_e_a @@ E_variable name.value in
            let expr' = make_e_a @@ E_constant ("MAP_UPDATE", [key_expr ; value_expr ; old_expr]) in
            ok @@ I_assignment {name = name.value ; annotated_expression = expr'}
          )
    )
  | CaseInstr c ->
      let c = c.value in
      let%bind expr = simpl_expression c.expr in
      let%bind cases =
        let aux (x : Raw.instruction Raw.case_clause Raw.reg) =
          let%bind i = simpl_instruction_block x.value.rhs in
          ok (x.value.pattern, i) in
        bind_list
        @@ List.map aux
        @@ npseq_to_list c.cases.value in
      let%bind m = simpl_cases cases in
      ok @@ I_matching (expr, m)
  | RecordPatch r -> (
      let r = r.value in
      let (name , access_path) = simpl_path r.path in
      let%bind inj = bind_list
        @@ List.map (fun (x:Raw.field_assign) -> let%bind e = simpl_expression x.field_expr in ok (x.field_name.value, e))
        @@ List.map (fun (x:_ Raw.reg) -> x.value)
        @@ pseq_to_list r.record_inj.value.elements in
      ok @@ I_record_patch (name, access_path, inj)
    )
  | MapPatch _ -> simple_fail "no map patch yet"
  | SetPatch _ -> simple_fail "no set patch yet"
  | MapRemove r ->
      let v = r.value in
      let key = v.key in
      let%bind map = match v.map with
        | Name v -> ok v.value
        | _ -> simple_fail "no complex map remove yet" in
      let%bind key' = simpl_expression key in
      let expr = E_constant ("MAP_REMOVE", [key' ; make_e_a (E_variable map)]) in
      ok @@ I_assignment {name = map ; annotated_expression = make_e_a expr}
  | SetRemove _ -> simple_fail "no set remove yet"

and simpl_path : Raw.path -> string * Ast_simplified.access_path = fun p ->
  match p with
  | Raw.Name v -> (v.value , [])
  | Raw.Path p -> (
      let p' = p.value in
      let var = p'.struct_name.value in
      let path = p'.field_path in
      let path' =
        let aux (s:Raw.selection) =
          match s with
          | FieldName property -> Access_record property.value
          | Component index -> Access_tuple (Z.to_int (snd index.value))
        in
        List.map aux @@ npseq_to_list path in
      (var , path')
    )

and simpl_cases : type a . (Raw.pattern * a) list -> a matching result = fun t ->
  let open Raw in
  let get_var (t:Raw.pattern) = match t with
    | PVar v -> ok v.value
    | _ ->
        let error =
          let title () = "not a var" in
          let content () = Format.asprintf "%a" (PP_helpers.printer Raw.print_pattern) t in
          error title content
        in
        fail error
  in
  let get_tuple (t:Raw.pattern) = match t with
    | PCons v -> npseq_to_list v.value
    | PTuple v -> npseq_to_list v.value.inside
    | x -> [ x ]
  in
  let get_single (t:Raw.pattern) =
    let t' = get_tuple t in
    let%bind () =
      trace_strong (simple_error "not single") @@
      Assert.assert_list_size t' 1 in
    ok (List.hd t') in
  let get_constr (t:Raw.pattern) = match t with
    | PConstr v ->
        let%bind var = get_single (snd v.value).value >>? get_var in
        ok ((fst v.value).value , var)
    | _ -> simple_fail "not a constr"
  in
  let%bind patterns =
    let aux (x , y) =
      let xs = get_tuple x in
      trace_strong (simple_error "no tuple in patterns yet") @@
      Assert.assert_list_size xs 1 >>? fun () ->
      ok (List.hd xs , y)
    in
    bind_map_list aux t in
  match patterns with
  | [(PFalse _ , f) ; (PTrue _ , t)]
  | [(PTrue _ , t) ; (PFalse _ , f)] -> ok @@ Match_bool {match_true = t ; match_false = f}
  | [(PSome v , some) ; (PNone _ , none)]
  | [(PNone _ , none) ; (PSome v , some)] -> (
      let (_, v) = v.value in
      let%bind v = match v.value.inside with
        | PVar v -> ok v.value
        | _ -> simple_fail "complex none patterns not supported yet" in
      ok @@ Match_option {match_none = none ; match_some = (v, some) }
    )
  | [(PCons c , cons) ; (PList (PNil _) , nil)]
  | [(PList (PNil _) , nil) ; (PCons c,  cons)] ->
      let%bind (a, b) =
        match c.value with
        | a, [(_, b)] ->
            let%bind a = get_var a in
            let%bind b = get_var b in
            ok (a, b)
        | _ -> simple_fail "complex list patterns not supported yet"
      in
      ok @@ Match_list {match_cons = (a, b, cons) ; match_nil = nil}
  | lst ->
      trace (simple_error "weird patterns not supported yet") @@
      let%bind constrs =
        let aux (x , y) =
          let error =
            let title () = "Pattern" in
            let content () =
              Format.asprintf "Pattern : %a" (PP_helpers.printer Raw.print_pattern) x in
            error title content in
          let%bind x' =
            trace error @@
            get_constr x in
          ok (x' , y) in
        bind_map_list aux lst in
      ok @@ Match_variant constrs

and simpl_instruction_block : Raw.instruction -> block result = fun t ->
  match t with
  | Single s -> let%bind i = simpl_single_instruction s in ok [ i ]
  | Block b -> simpl_block b.value

and simpl_instruction : Raw.instruction -> instruction result = fun t ->
  let main_error =
    let title () = "simplifiying instruction" in
    let content () = Format.asprintf "%a" PP_helpers.(printer Raw.print_instruction) t in
    error title content in
  trace main_error @@
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
