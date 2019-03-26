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
  | Record_patch of ae * (string * ae) list

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
  | Match_tuple of name list * b

let ae expression = {expression ; type_annotation = None}

let annotated_expression expression type_annotation = {expression ; type_annotation}

open Ligo_helpers.Trace

module PP = struct
  open Ligo_helpers.PP
  open Format

  let rec type_expression ppf (te:type_expression) = match te with
    | Type_tuple lst -> fprintf ppf "tuple[%a]" (list_sep type_expression) lst
    | Type_sum m -> fprintf ppf "sum[%a]" (smap_sep type_expression) m
    | Type_record m -> fprintf ppf "record[%a]" (smap_sep type_expression) m
    | Type_function (p, r) -> fprintf ppf "%a -> %a" type_expression p type_expression r
    | Type_variable name -> fprintf ppf "%s" name
    | Type_constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep type_expression) lst

  let literal ppf (l:literal) = match l with
    | Unit -> fprintf ppf "Unit"
    | Bool b -> fprintf ppf "%b" b
    | Number n -> fprintf ppf "%d" n
    | String s -> fprintf ppf "%S" s
    | Bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b

  let rec expression ppf (e:expression) = match e with
    | Literal l -> literal ppf l
    | Variable name -> fprintf ppf "%s" name
    | Application (f, arg) -> fprintf ppf "(%a) (%a)" annotated_expression f annotated_expression arg
    | Constructor (name, ae) -> fprintf ppf "%s(%a)" name annotated_expression ae
    | Constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep annotated_expression) lst
    | Tuple lst -> fprintf ppf "tuple[%a]" (list_sep annotated_expression) lst
    | Tuple_accessor (ae, i) -> fprintf ppf "%a.%d" annotated_expression ae i
    | Record m -> fprintf ppf "record[%a]" (smap_sep annotated_expression) m
    | Record_accessor (ae, s) -> fprintf ppf "%a.%s" annotated_expression ae s
    | Lambda {binder;input_type;output_type;result;body} ->
        fprintf ppf "lambda (%s:%a) : %a {%a} return %a"
          binder type_expression input_type type_expression output_type
          block body annotated_expression result

  and annotated_expression ppf (ae:annotated_expression) = match ae.type_annotation with
    | None -> fprintf ppf "%a" expression ae.expression
    | Some t -> fprintf ppf "(%a) : %a" expression ae.expression type_expression t

  and block ppf (b:block) = (list_sep instruction) ppf b

  and single_record_patch ppf ((s, ae) : string * ae) =
    fprintf ppf "%s <- %a" s annotated_expression ae

  and matching ppf (m:matching) = match m with
    | Match_tuple (lst, b) ->
        fprintf ppf "let (%a) = %a" (list_sep (fun ppf -> fprintf ppf "%s")) lst block b
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" block match_true block match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" block match_nil hd tl block match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %s -> %a" block match_none some block match_some

  and instruction ppf (i:instruction) = match i with
    | Skip -> fprintf ppf "skip"
    | Fail ae -> fprintf ppf "fail with (%a)" annotated_expression ae
    | Record_patch (ae, lst) -> fprintf ppf "%a.[%a]" annotated_expression ae (list_sep single_record_patch) lst
    | Loop (cond, b) -> fprintf ppf "while (%a) { %a }" annotated_expression cond block b
    | Assignment {name;annotated_expression = ae} ->
        fprintf ppf "%s := %a" name annotated_expression ae
    | Matching (ae, m) ->
        fprintf ppf "match %a with %a" annotated_expression ae matching m

  let declaration ppf (d:declaration) = match d with
    | Type_declaration {type_name ; type_expression = te} ->
        fprintf ppf "type %s = %a" type_name type_expression te
    | Constant_declaration {name ; annotated_expression = ae} ->
        fprintf ppf "const %s = %a" name annotated_expression ae

  let program ppf (p:program) =
    fprintf ppf "%a" (list_sep declaration) p
end

module Simplify = struct
  module Raw = Ligo_parser.AST

  let nseq_to_list (hd, tl) = hd :: tl
  let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
  let npseq_to_nelist (hd, tl) = hd, (List.map snd tl)
  let pseq_to_list = function
    | None -> []
    | Some lst -> npseq_to_list lst

  let type_constants = [
    ("nat", 0) ;
    ("int", 0) ;
  ]

  let rec simpl_type_expression (t:Raw.type_expr) : type_expression result =
    match t with
    | TPar x -> simpl_type_expression x.value.inside
    | TAlias v -> (
        match List.assoc_opt v.value type_constants with
        | Some 0 -> ok @@ Type_constant (v.value, [])
        | Some _ -> simple_fail "type constructor with wrong number of args"
        | None -> ok @@ Type_variable v.value
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
    | EBytes x -> ok @@ ae @@ Literal (Bytes (Bytes.of_string @@ fst x.value))
    | ETuple tpl ->
        simpl_list_expression
        @@ npseq_to_list tpl.value.inside
    | ERecord (RecordInj r) ->
        let%bind fields = bind_list
          @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = simpl_expression v in ok (k.value, v))
          @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
          @@ npseq_to_list r.value.fields in
        let aux prev (k, v) = SMap.add k v prev in
        ok @@ ae @@ Record (List.fold_left aux SMap.empty fields)
    | ERecord (RecordProj p) ->
        let record = p.value.record_name.value in
        let lst = List.map (fun (x:_ Raw.reg) -> x.value) @@ npseq_to_list p.value.field_path in
        let aux prev cur =
          ae @@ Record_accessor (prev, cur)
        in
        let init = ae @@ Variable record in
        ok @@ List.fold_left aux init lst
    | EConstr (ConstrApp c) ->
        let (c, args) = c.value in
        let%bind arg =
          simpl_list_expression
          @@ npseq_to_list args.value.inside in
        ok @@ ae @@ Constructor (c.value, arg)
    | EConstr _ -> simple_fail "econstr: not supported yet"
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
    | ELogic (BoolExpr (False _)) ->
        ok @@ ae @@ Literal (Bool false)
    | ELogic (BoolExpr (True _)) ->
        ok @@ ae @@ Literal (Bool true)
    | ELogic _ -> simple_fail "logic: not supported yet"
    | EList _ -> simple_fail "list: not supported yet"
    | ESet _ -> simple_fail "set: not supported yet"
    | EMap _ -> simple_fail "map: not supported yet"


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

  and simpl_local_declaration (t:Raw.local_decl) : instruction result =
    match t with
    | LocalVar x ->
        let x = x.value in
        let name = x.name.value in
        let%bind t = simpl_type_expression x.var_type in
        let type_annotation = Some t in
        let%bind expression = simpl_expression x.init in
        ok @@ Assignment {name;annotated_expression={expression with type_annotation}}
    | LocalConst x ->
        let x = x.value in
        let name = x.name.value in
        let%bind t = simpl_type_expression x.const_type in
        let type_annotation = Some t in
        let%bind expression = simpl_expression x.init in
        ok @@ Assignment {name;annotated_expression={expression with type_annotation}}
    | _ -> simple_fail "todo"

  and simpl_param (t:Raw.param_decl) : named_type_expression result =
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
        let%bind param = match npseq_to_list param.value.inside with
          | [a] -> ok a
          | _ -> simple_fail "only one param allowed" in
        let%bind input = simpl_param param in
        let name = name.value in
        let binder = input.type_name in
        let input_type = input.type_expression in
        let%bind local_declarations = bind_list @@ List.map simpl_local_declaration local_decls in
        let%bind instructions = bind_list
          @@ List.map simpl_instruction
          @@ npseq_to_list block.value.instr in
        let%bind result = simpl_expression return in
        let%bind output_type = simpl_type_expression ret_type in
        let body = local_declarations @ instructions in
        let expression = Lambda {binder ; input_type ; output_type ; result ; body } in
        let type_annotation = Some (Type_function (input_type, output_type)) in
        ok @@ Constant_declaration {name;annotated_expression = {expression;type_annotation}}
    | LambdaDecl (ProcDecl _) -> simple_fail "no proc declaration yet"
    | LambdaDecl (EntryDecl _)-> simple_fail "no entry point yet"


  and simpl_single_instruction (t:Raw.single_instr) : instruction result =
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
        let%bind match_true = simpl_instruction_block c.ifso in
        let%bind match_false = simpl_instruction_block c.ifnot in
        ok @@ Matching (expr, (Match_bool {match_true; match_false}))
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
    | Case c ->
        let c = c.value in
        let%bind expr = simpl_expression c.expr in
        let%bind cases = bind_list
          @@ List.map (fun (x:Raw.case) -> let%bind i = simpl_instruction_block x.instr in ok (x.pattern, i))
          @@ List.map (fun (x:_ Raw.reg) -> x.value)
          @@ npseq_to_list c.cases.value in
        let%bind m = simpl_cases cases in
        ok @@ Matching (expr, m)
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
        ok @@ Record_patch ({expression=Variable record;type_annotation=None}, inj)
    | MapPatch _ -> simple_fail "no map patch yet"

  and simpl_cases (t:(Raw.pattern * block) list) : matching result =
    let open Raw in
    let get_var (t:Raw.pattern) = match t with
      | PVar v -> ok v.value
      | _ -> simple_fail "not a var"
    in
    match t with
    | [(PFalse _, f) ; (PTrue _, t)]
    | [(PTrue _, f) ; (PFalse _, t)] -> ok @@ Match_bool {match_true = t ; match_false = f}
    | [(PSome v, some) ; (PNone _, none)]
    | [(PNone _, none) ; (PSome v, some)] -> (
        let (_, v) = v.value in
        let%bind v = match v.value.inside with
        | PVar v -> ok v.value
        | _ -> simple_fail "complex patterns not supported yet" in
        ok @@ Match_option {match_none = none ; match_some = (v, some) }
      )
    | [(PCons c, cons) ; (PList n, nil)]
    | [(PList n, nil) ; (PCons c, cons)] ->
        let%bind _ = match n with
          | Sugar c -> (
              match pseq_to_list c.value.inside with
              | [] -> ok ()
              | _ -> simple_fail "complex patterns not supported yet"
            )
          | Raw _ -> simple_fail "complex patterns not supported yet"
        in
        let%bind (a, b) =
          match c.value with
          | a, [(_, b)] ->
              let%bind a = get_var a in
              let%bind b = get_var b in
              ok (a, b)
          | _ -> simple_fail "complex patterns not supported yet"
        in
        ok @@ Match_list {match_cons = (a, b, cons) ; match_nil = nil}
    | _ -> simple_fail "complex patterns not supported yet"

  and simpl_instruction_block (t:Raw.instruction) : block result =
    match t with
    | Single s -> let%bind i = simpl_single_instruction s in ok [ i ]
    | Block b -> simpl_block b.value

  and simpl_instruction (t:Raw.instruction) : instruction result =
    match t with
    | Single s -> simpl_single_instruction s
    | Block _ -> simple_fail "no block instruction yet"

  and simpl_block (t:Raw.block) : block result =
    bind_list @@ List.map simpl_instruction (npseq_to_list t.instr)

  let simpl_program (t:Raw.ast) : program result =
    bind_list @@ List.map simpl_declaration @@ nseq_to_list t.decl
end

module Combinators = struct
  let annotated_expression ?type_annotation expression = {expression ; type_annotation}

  let number n : expression = Literal (Number n)

  let record (lst : (string * ae) list) : expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Record map
end
