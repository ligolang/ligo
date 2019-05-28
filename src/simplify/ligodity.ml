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

open Operators.Simplify.Ligodity

let r_split = Location.r_split

let rec simpl_type_expression : Raw.type_expr -> type_expression result =
  function
  | TPar x -> simpl_type_expression x.value.inside
  | TAlias v -> (
      match List.assoc_opt v.value type_constants with
      | Some s -> ok @@ T_constant (s , [])
      | None -> ok @@ T_variable v.value
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
      let%bind cst =
        trace_option (simple_error "unrecognized type constants") @@
        List.assoc_opt name.value type_constants in
      let%bind lst' = bind_list @@ List.map simpl_type_expression lst in
      ok @@ T_constant (cst , lst')
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
  Raw.expr -> expr result = fun t ->
  let return x = ok x in
  let simpl_projection = fun (p:Raw.projection Region.reg) ->
    let (p , loc) = r_split p in
    let var =
      let name = p.struct_name.value in
      e_variable name in
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
    return @@ e_accessor ~loc var path'
  in

  match t with
  | Raw.ELetIn e -> (
      let Raw.{binding ; body ; _} = e.value in
      let Raw.{variable ; lhs_type ; let_rhs ; _} = binding in
      let%bind ty_opt =
        bind_map_option
          (fun (_ , type_expr) -> simpl_type_expression type_expr)
          lhs_type in
      let%bind rhs = simpl_expression let_rhs in
      let rhs' =
        match ty_opt with
        | None -> rhs
        | Some ty -> e_annotation rhs ty in
      let%bind body = simpl_expression body in
      return @@ e_let_in (variable.value , None) rhs' body
    )
  | Raw.EAnnot a -> (
      let (a , loc) = r_split a in
      let (expr , type_expr) = a in
      let%bind expr' = simpl_expression expr in
      let%bind type_expr' = simpl_type_expression type_expr in
      return @@ e_annotation ~loc expr' type_expr'
    )
  | EVar c -> (
      let c' = c.value in
      match List.assoc_opt c' constants with
      | None -> return @@ e_variable c.value
      | Some s -> return @@ e_constant s []
    )
  | ECall x -> (
      let ((e1 , e2) , loc) = r_split x in
      let%bind args = bind_map_list simpl_expression (nseq_to_list e2) in
      match e1 with
      | EVar f -> (
          let (f , f_loc) = r_split f in
          match List.assoc_opt f constants with
          | None -> (
              let%bind arg = simpl_tuple_expression (nseq_to_list e2) in
              return @@ e_application ~loc (e_variable ~loc:f_loc f) arg
            )
          | Some s -> return @@ e_constant ~loc s args
        )
      | e1 -> (
          let%bind e1' = simpl_expression e1 in
          let%bind arg = simpl_tuple_expression (nseq_to_list e2) in
          return @@ e_application ~loc e1' arg
      )
    )
  | EPar x -> simpl_expression x.value.inside
  | EUnit reg -> (
      let (_ , loc) = r_split reg in
      return @@ e_literal ~loc Literal_unit
    )
  | EBytes x -> (
      let (x , loc) = r_split x in
      return @@ e_literal ~loc (Literal_bytes (Bytes.of_string @@ fst x))
    )
  | ETuple tpl -> simpl_tuple_expression @@ (npseq_to_list tpl.value)
  | ERecord r -> (
      let (r , loc) = r_split r in
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = simpl_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
        @@ pseq_to_list r.elements in
      let map = SMap.of_list fields in
      return @@ e_record ~loc map
    )
  | EProj p -> simpl_projection p
  | EConstr c -> (
      let ((c_name , args) , loc) = r_split c in
      let (c_name , _c_loc) = r_split c_name in
      let args =
        match args with
          None -> []
        | Some arg -> [arg] in
      let%bind arg = simpl_tuple_expression @@ args in
      return @@ e_constructor ~loc c_name arg
    )
  | EArith (Add c) ->
      simpl_binop "ADD" c
  | EArith (Sub c) ->
      simpl_binop "SUB" c
  | EArith (Mult c) ->
      simpl_binop "TIMES" c
  | EArith (Div c) ->
      simpl_binop "DIV" c
  | EArith (Mod c) ->
      simpl_binop "MOD" c
  | EArith (Int n) -> (
      let (n , loc) = r_split n in
      let n = Z.to_int @@ snd @@ n in
      return @@ e_literal ~loc (Literal_int n)
    )
  | EArith (Nat n) -> (
      let (n , loc) = r_split n in
      let n = Z.to_int @@ snd @@ n in
      return @@ e_literal ~loc (Literal_nat n)
    )
  | EArith (Mtz n) -> (
      let (n , loc) = r_split n in
      let n = Z.to_int @@ snd @@ n in
      return @@ e_literal ~loc (Literal_tez n)
    )
  | EArith _ -> simple_fail "arith: not supported yet"
  | EString (String s) -> (
      let (s , loc) = r_split s in
      let s' =
        let s = s in
        String.(sub s 1 ((length s) - 2))
      in
      return @@ e_literal ~loc (Literal_string s')
    )
  | EString _ -> simple_fail "string: not supported yet"
  | ELogic l -> simpl_logic_expression l
  | EList l -> simpl_list_expression l
  | ECase c -> (
      let (c , loc) = r_split c in
      let%bind e = simpl_expression c.expr in
      let%bind lst =
        let aux (x : Raw.expr Raw.case_clause) =
          let%bind expr = simpl_expression x.rhs in
          ok (x.pattern, expr) in
        bind_list
        @@ List.map aux
        @@ List.map get_value
        @@ npseq_to_list c.cases.value in
      let%bind cases = simpl_cases lst in
      return @@ e_matching ~loc e cases
    )
  | EFun lamb -> (
      let (lamb , loc) = r_split lamb in
      let%bind input_type = bind_map_option
        (fun (_,type_expr) -> simpl_type_expression type_expr)
        lamb.p_annot in
      let body, body_type =
        match lamb.body with
        | EAnnot {value = expr, type_expr} -> expr, Some type_expr
        | expr -> expr, None in
      let%bind output_type =
        bind_map_option simpl_type_expression body_type in
      let%bind result = simpl_expression body in
      let binder = lamb.param.value in
      return @@ e_lambda ~loc binder input_type output_type result
    )
  | ESeq s -> (
      let (s , loc) = r_split s in
      let items : Raw.expr list = pseq_to_list s.elements in
      match items with
      | [] -> return @@ e_skip ~loc ()
      | expr :: more -> (
        let expr' = simpl_expression expr in
        let apply (e1: Raw.expr) (e2: expression Trace.result) =
          let%bind a = simpl_expression e1 in
          let%bind e2' = e2 in
          return @@ e_sequence ~loc a e2'
        in List.fold_right apply more expr'
      )
    )
  | ECond c -> (
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.test in
      let%bind match_true = simpl_expression c.ifso in
      let%bind match_false = simpl_expression c.ifnot in
      let match_bool = Match_bool { match_true ; match_false } in
      return @@ e_matching ~loc expr match_bool
    )

and simpl_logic_expression (t:Raw.logic_expr) : expr result =
  let return x = ok @@ x in
  match t with
  | BoolExpr (False reg) -> (
      let loc = Location.lift reg in
      return @@ e_literal ~loc (Literal_bool false)
    )
  | BoolExpr (True reg) -> (
      let loc = Location.lift reg in
      return @@ e_literal ~loc (Literal_bool true)
    )
  | BoolExpr (Or b) ->
      simpl_binop "OR" b
  | BoolExpr (And b) ->
      simpl_binop "AND" b
  | BoolExpr (Not b) ->
      simpl_unop "NOT" b
  | CompExpr (Lt c) ->
      simpl_binop "LT" c
  | CompExpr (Gt c) ->
      simpl_binop "GT" c
  | CompExpr (Leq c) ->
      simpl_binop "LE" c
  | CompExpr (Geq c) ->
      simpl_binop "GE" c
  | CompExpr (Equal c) ->
      simpl_binop "EQ" c
  | CompExpr (Neq c) ->
      simpl_binop "NEQ" c

and simpl_list_expression (t:Raw.list_expr) : expression result =
  let return x = ok @@ x in
  match t with
  | Cons c -> simpl_binop "CONS" c
  | List lst -> (
      let (lst , loc) = r_split lst in
      let%bind lst' =
        bind_map_list simpl_expression @@
        pseq_to_list lst.elements in
      return @@ e_list ~loc lst'
    )

and simpl_binop (name:string) (t:_ Raw.bin_op Region.reg) : expression result =
  let return x = ok @@ x in
  let (args , loc) = r_split t in
  let%bind a = simpl_expression args.arg1 in
  let%bind b = simpl_expression args.arg2 in
  return @@ e_constant ~loc name [ a ; b ]

and simpl_unop (name:string) (t:_ Raw.un_op Region.reg) : expression result =
  let return x = ok @@ x in
  let (t , loc) = r_split t in
  let%bind a = simpl_expression t.arg in
  return @@ e_constant ~loc name [ a ]

and simpl_tuple_expression ?loc (lst:Raw.expr list) : expression result =
  let return x = ok @@ x in
  match lst with
  | [] -> return @@ e_literal ?loc Literal_unit
  | [hd] -> simpl_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_expression lst in
      return @@ e_tuple ?loc lst

and simpl_declaration : Raw.declaration -> declaration Location.wrap result = fun t ->
  let open! Raw in
  let loc : 'a . 'a Raw.reg -> _ -> _ = fun x v -> Location.wrap ~loc:(File x.region) v in
  match t with
  | TypeDecl x ->
      let {name;type_expr} : Raw.type_decl = x.value in
      let%bind type_expression = simpl_type_expression type_expr in
      ok @@ loc x @@ Declaration_type (name.value , type_expression)
  | LetEntry _ -> simple_fail "no entry point yet"
  | Let x -> (
      let _, binding = x.value in
      let {variable ; lhs_type ; let_rhs} = binding in
      let%bind type_annotation = bind_map_option
        (fun (_,type_expr) -> simpl_type_expression type_expr)
        lhs_type in
      let%bind rhs = simpl_expression let_rhs in
      let name = variable.value in
      ok @@ loc x @@ (Declaration_constant (name , type_annotation , rhs))
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
    | PTuple v -> npseq_to_list v.value
    | x -> [ x ]
  in
  let get_single (t:Raw.pattern) =
    let t' = get_tuple t in
    let%bind () =
      trace_strong (simple_error "not single") @@
      Assert.assert_list_size t' 1 in
    ok (List.hd t') in
  let get_constr (t:Raw.pattern) = match t with
    | PConstr v -> (
        let (const , pat_opt) = v.value in
        let%bind pat =
          trace_option (simple_error "No constructor without variable yet") @@
          pat_opt in
        let%bind single_pat = get_single pat in
        let%bind var = get_var single_pat in
        ok (const.value , var)
      )
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
  | [(PList (PCons c) , cons) ; (PList (Sugar sugar_nil) , nil)]
  | [(PList (Sugar sugar_nil) , nil) ; (PList (PCons c),  cons)] -> (
      let%bind () =
        trace_strong (simple_error "Only empty list patterns and cons are allowed yet")
        @@ Assert.assert_list_empty
        @@ pseq_to_list
        @@ sugar_nil.value.elements in
      let%bind (a, b) =
        let (a , _ , b) = c.value in
        let%bind a = get_var a in
        let%bind b = get_var b in
        ok (a, b)
      in
      ok @@ Match_list {match_cons = (a, b, cons) ; match_nil = nil}
    )
  | lst -> (
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
    )

let simpl_program : Raw.ast -> program result = fun t ->
  bind_list @@ List.map simpl_declaration @@ nseq_to_list t.decl
