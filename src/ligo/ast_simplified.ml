module SMap = Ligo_helpers.X_map.String

type name = string
type type_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration list

and declaration =
  | Type_declaration of named_type_expression
  | Constant_declaration of named_expression
  (* | Macro_declaration of macro_declaration *)

and annotated_expression = {
  expression: expression ;
  type_annotation: te option ;
}

and named_expression = {
  name: name ;
  annotated_expression: ae ;
}

and named_type_expression = {
  type_name: type_name ;
  type_expression: type_expression ;
}

and te = type_expression
and ae = annotated_expression
and te_map = type_expression type_name_map
and ae_map = annotated_expression name_map

and type_expression =
  | Type_tuple of te list
  | Type_sum of te_map
  | Type_record of te_map
  | Type_function of te * te
  | Type_variable of type_name
  | Type_constant of type_name * te list

and lambda = {
  binder: name ;
  input_type: type_expression ;
  output_type: type_expression ;
  result: ae ;
  body: block ;
}

and expression =
  (* Base *)
  | Literal of literal
  | Constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | Variable of name
  | Lambda of lambda
  | Application of ae * ae
  (* Tuple *)
  | Tuple of ae list
  | Tuple_accessor of ae * int (* Access n'th tuple's element *)
  (* Sum *)
  | Constructor of name * ae (* For user defined constructors *)
  (* Record *)
  | Record of ae_map
  | Record_accessor of ae * string

and literal =
  | Unit
  | Bool of bool
  | Number of int
  | String of string
  | Bytes of bytes

and block = instruction list
and b = block

and instruction =
  | Assignment of named_expression
  | Matching of ae * matching
  | Loop of ae * b
  | Skip
  | Fail of ae

and matching =
  | Match_bool of {
      match_true : b ;
      match_false : b ;
    }
  | Match_list of {
      match_nil : b ;
      match_cons : name * name * b ;
    }
  | Match_option of {
      match_none : b ;
      match_some : name * b ;
    }
  | Match_tuple of (name * b) list

let ae expression = {expression ; type_annotation = None}

open Ligo_helpers.Trace

module Simplify = struct
  module Raw = Ligo_parser.AST

  let nseq_to_list (hd, tl) = hd :: tl
  let npseq_to_list (hd, tl) = hd :: (List.map snd tl)

  let rec simpl_type_expression (t:Raw.type_expr) : type_expression result =
    match t with
    | TPar x -> simpl_type_expression x.value.inside
    | TAlias v -> ok @@ Type_variable v.value
    | TApp x ->
        let (name, tuple) = x.value in
        let%bind lst = bind_list
          @@ List.map simpl_type_expression
          @@ npseq_to_list tuple.value.inside in
        ok @@ Type_constant (name.value, lst)
    | TProd p ->
        let%bind lst = bind_list
          @@ List.map simpl_type_expression
          @@ npseq_to_list p.value in
        ok @@ Type_tuple lst
    | TRecord r ->
        let aux = fun (x, y) -> let%bind y = simpl_type_expression y in ok (x, y) in
        let%bind lst = bind_list
          @@ List.map aux
          @@ List.map (fun (x:Raw.field_decl Raw.reg) -> (x.value.field_name.value, x.value.field_type))
          @@ npseq_to_list r.value.fields in
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

  let rec simpl_expression (t:Raw.expr) : ae result =
    match t with
    | EVar c -> ok @@ ae @@ Variable c.value
    | ECall x ->
        let (name, args) = x.value in
        let f = name.value in
        let%bind arg = simpl_list_expression
          @@ npseq_to_list args.value.inside in
        ok @@ ae @@ Application (ae @@ Variable f, arg)
    | EPar x -> simpl_expression x.value.inside
    | EUnit _ -> ok @@ ae @@ Literal Unit
    | EBytes x -> ok @@ ae @@ Literal (Bytes (fst x.value))
    | ETuple tpl ->
        simpl_list_expression
        @@ npseq_to_list tpl.value.inside
    | EConstr (ConstrApp c) ->
        let (c, args) = c.value in
        let%bind arg =
          simpl_list_expression
          @@ npseq_to_list args.value.inside in
        ok @@ ae @@ Constructor (c.value, arg)
    | EArith (Add c) ->
        let%bind (a, b) = simpl_binop c.value in
        ok @@ ae @@ Constant ("ADD", [a;b])
    | EArith (Int n) ->
        let n = Z.to_int @@ snd @@ n.value in
        ok @@ ae @@ Literal (Number n)
    | EArith _ -> simple_fail "arith: not supported yet"
    | EString (String s) ->
        ok @@ ae @@ Literal (String s.value)
    | EString _ -> simple_fail "string: not supported yet"
    | _ -> simple_fail "todo"

  and simpl_binop (t:_ Raw.bin_op) : (ae * ae) result =
    let%bind a = simpl_expression t.arg1 in
    let%bind b = simpl_expression t.arg2 in
    ok (a, b)

  and simpl_list_expression (lst:Raw.expr list) : ae result =
    match lst with
    | [] -> ok @@ ae @@ Literal Unit
    | [hd] -> simpl_expression hd
    | lst ->
        let%bind lst = bind_list @@ List.map simpl_expression lst in
        ok @@ ae @@ Tuple lst

  and simpl_lambda (t:Raw.lambda_decl) : lambda result = simple_fail "todo"

  and simpl_declaration (t:Raw.declaration) : declaration result =
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
        simple_fail "todo"
    | _ -> simple_fail "todo"

  let simpl_program (t:Raw.ast) : program result =
    bind_list @@ List.map simpl_declaration @@ nseq_to_list t.decl
end
