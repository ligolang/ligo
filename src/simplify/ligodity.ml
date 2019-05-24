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
  ?te_annot:type_expression -> Raw.expr -> expr result = fun ?te_annot t ->
  let return x = ok @@ make_option_typed x te_annot in
  let simpl_projection = fun (p:Raw.projection) ->
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
    return @@ E_accessor (var, path')
  in
  let mk_let_in binder rhs result =
    E_let_in {binder; rhs; result} in

  match t with
  | Raw.ELetIn e -> (
      let Raw.{binding; body; _} = e.value in
      let Raw.{variable; lhs_type; let_rhs; _} = binding in
      let%bind type_annotation = bind_map_option
          (fun (_,type_expr) -> simpl_type_expression type_expr)
          lhs_type in
      let%bind rhs = simpl_expression ?te_annot:type_annotation let_rhs in
      let%bind body = simpl_expression body in
      return (mk_let_in (variable.value , None) rhs body)
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
      | Some s -> return @@ E_constant (s , [])
    )
  | ECall x -> (
      let (e1, e2) = x.value in
      let%bind args = bind_map_list simpl_expression (nseq_to_list e2) in
      match e1 with
      | EVar f ->
          (match List.assoc_opt f.value constants with
          | None ->
              let%bind arg = simpl_tuple_expression (nseq_to_list e2) in
              return @@ E_application (e_variable f.value, arg)
          | Some s -> return @@ E_constant (s , args))
      | e1 ->
        let%bind e1' = simpl_expression e1 in
        let%bind arg = simpl_tuple_expression (nseq_to_list e2) in
        return @@ E_application (e1' , arg)
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

and simpl_logic_expression ?te_annot (t:Raw.logic_expr) : expr result =
  let return x = ok @@ make_option_typed x te_annot in
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

and simpl_list_expression ?te_annot (t:Raw.list_expr) : expression result =
  let return x = ok @@ make_option_typed x te_annot in
  match t with
  | Cons c ->
      simpl_binop ?te_annot "CONS" c.value
  | List lst ->
      let%bind lst' =
        bind_map_list simpl_expression @@
        pseq_to_list lst.value.elements in
      return @@ E_list lst'

and simpl_binop ?te_annot (name:string) (t:_ Raw.bin_op) : expression result =
  let return x = ok @@ make_option_typed x te_annot in
  let%bind a = simpl_expression t.arg1 in
  let%bind b = simpl_expression t.arg2 in
  return @@ E_constant (name, [a;b])

and simpl_unop ?te_annot (name:string) (t:_ Raw.un_op) : expression result =
  let return x = ok @@ make_option_typed x te_annot in
  let%bind a = simpl_expression t.arg in
  return @@ E_constant (name, [a])

and simpl_tuple_expression ?te_annot (lst:Raw.expr list) : expression result =
  let return x = ok @@ make_option_typed x te_annot in
  match lst with
  | [] -> return @@ E_literal Literal_unit
  | [hd] -> simpl_expression ?te_annot hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_expression lst in
      return @@ E_tuple lst

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
      let%bind rhs = simpl_expression ?te_annot:type_annotation let_rhs in
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
