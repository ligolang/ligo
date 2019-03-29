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
  (* Sum *)
  | Constructor of name * ae (* For user defined constructors *)
  (* Record *)
  | Record of ae_map
  | Accessor of ae * access_path

and access =
  | Tuple_access of int
  | Record_access of string

and access_path = access list

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
  | Record_patch of name * access_path * (string * ae) list

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
    | Accessor (ae, p) -> fprintf ppf "%a.%a" annotated_expression ae access_path p
    | Record m -> fprintf ppf "record[%a]" (smap_sep annotated_expression) m
    | Lambda {binder;input_type;output_type;result;body} ->
        fprintf ppf "lambda (%s:%a) : %a {%a} return %a"
          binder type_expression input_type type_expression output_type
          block body annotated_expression result

  and access ppf (a:access) =
    match a with
    | Tuple_access n -> fprintf ppf "%d" n
    | Record_access s -> fprintf ppf "%s" s

  and access_path ppf (p:access_path) =
    fprintf ppf "%a" (list_sep ~pp_sep:(const ".") access) p

  and type_annotation ppf (ta:type_expression option) = match ta with
    | None -> fprintf ppf ""
    | Some t -> type_expression ppf t

  and annotated_expression ppf (ae:annotated_expression) = match ae.type_annotation with
    | None -> fprintf ppf "%a" expression ae.expression
    | Some t -> fprintf ppf "(%a) : %a" expression ae.expression type_expression t

  and block ppf (b:block) = (list_sep instruction) ppf b

  and single_record_patch ppf ((p, ae) : string * ae) =
    fprintf ppf "%s <- %a" p annotated_expression ae

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
    | Record_patch (name, path, lst) -> fprintf ppf "%s.%a[%a]" name access_path path (list_sep single_record_patch) lst
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

module Rename = struct
  module Type = struct
    (* Type renaming, not needed. Yet. *)
  end

  module Value = struct
    type renaming = string * (string * access_path) (* src -> dst *)
    type renamings = renaming list
    let filter (r:renamings) (s:string) : renamings =
      List.filter (fun (x, _) -> not (x = s)) r
    let filters (r:renamings) (ss:string list) : renamings =
      List.filter (fun (x, _) -> not (List.mem x ss)) r

    let rec rename_instruction (r:renamings) (i:instruction) : instruction result =
      match i with
      | Assignment ({name;annotated_expression = e} as a) ->
          let%bind annotated_expression = rename_annotated_expression (filter r name) e in
          ok (Assignment {a with annotated_expression})
      | Skip -> ok Skip
      | Fail e ->
          let%bind e' = rename_annotated_expression r e in
          ok (Fail e')
      | Loop (cond, body) ->
          let%bind cond' = rename_annotated_expression r cond in
          let%bind body' = rename_block r body in
          ok (Loop (cond', body'))
      | Matching (ae, m) ->
          let%bind ae' = rename_annotated_expression r ae in
          let%bind m' = rename_matching r m in
          ok (Matching (ae', m'))
      | Record_patch (v, path, lst) ->
          let aux (x, y) =
            let%bind y' = rename_annotated_expression (filter r v) y in
            ok (x, y') in
          let%bind lst' = bind_map_list aux lst in
          match List.assoc_opt v r with
          | None -> (
              ok (Record_patch (v, path, lst'))
            )
          | Some (v, path') -> (
              ok (Record_patch (v, path' @ path, lst'))
            )
    and rename_block (r:renamings) (bl:block) : block result =
      bind_map_list (rename_instruction r) bl

    and rename_matching (r:renamings) (m:matching) : matching result =
      match m with
      | Match_bool { match_true = mt ; match_false = mf } ->
          let%bind match_true = rename_block r mt in
          let%bind match_false = rename_block r mf in
          ok (Match_bool {match_true ; match_false})
      | Match_option { match_none = mn ; match_some = (some, ms) } ->
          let%bind match_none = rename_block r mn in
          let%bind ms' = rename_block (filter r some) ms in
          ok (Match_option {match_none ; match_some = (some, ms')})
      | Match_list { match_nil = mn ; match_cons = (hd, tl, mc) } ->
          let%bind match_nil = rename_block r mn in
          let%bind mc' = rename_block (filters r [hd;tl]) mc in
          ok (Match_list {match_nil ; match_cons = (hd, tl, mc')})
      | Match_tuple (lst, body) ->
          let%bind body' = rename_block (filters r lst) body in
          ok (Match_tuple (lst, body'))

    and rename_annotated_expression (r:renamings) (ae:annotated_expression) : annotated_expression result =
      let%bind expression = rename_expression r ae.expression in
      ok {ae with expression}

    and rename_expression (r:renamings) (e:expression) : expression result =
      match e with
      | Literal _ as l -> ok l
      | Constant (name, lst) ->
          let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
          ok (Constant (name, lst'))
      | Constructor (name, ae) ->
          let%bind ae' = rename_annotated_expression r ae in
          ok (Constructor (name, ae'))
      | Variable v -> (
          match List.assoc_opt v r with
          | None -> ok (Variable v)
          | Some (name, path) -> ok (Accessor (ae (Variable (name)), path))
        )
      | Lambda ({binder;body;result} as l) ->
          let r' = filter r binder in
          let%bind body = rename_block r' body in
          let%bind result = rename_annotated_expression r' result in
          ok (Lambda {l with body ; result})
      | Application (f, arg) ->
          let%bind f' = rename_annotated_expression r f in
          let%bind arg' = rename_annotated_expression r arg in
          ok (Application (f', arg'))
      | Tuple lst ->
          let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
          ok (Tuple lst')
      | Accessor (ae, p) ->
          let%bind ae' = rename_annotated_expression r ae in
          ok (Accessor (ae', p))
          (* let aux prev hd =
           *   match hd with
           *   | Tuple_access n -> Tuple_accessor (prev, n)
           *   | Record_access s -> Record_accessor (prev, s)
           * in
           * let lst = List.fold_left aux ae p in
           * ok lst *)
      | Record sm ->
          let%bind sm' = bind_smap
            @@ SMap.map (rename_annotated_expression r) sm in
          ok (Record sm')
  end
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
    ("unit", 0) ;
    ("nat", 0) ;
    ("int", 0) ;
    ("bool", 0) ;
    ("list", 1) ;
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

  let rec simpl_expression (t:Raw.expr) : ae result =
    match t with
    | EVar c ->
        if c.value = "unit"
        then ok @@ ae @@ Literal Unit
        else ok @@ ae @@ Variable c.value
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
      )
    | EConstr (ConstrApp c) ->
        let (c, args) = c.value in
        let%bind arg =
          simpl_list_expression
          @@ npseq_to_list args.value.inside in
        ok @@ ae @@ Constructor (c.value, arg)
    | EConstr _ -> simple_fail "econstr: not supported yet"
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
    | EMap _ -> simple_fail "map: not supported yet"

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
        let%bind cases =
          let aux (x : Raw.case Raw.reg) =
            let%bind i = simpl_instruction_block x.value.instr in
            ok (x.value.pattern, i) in
          bind_list
          @@ List.map aux
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
        ok @@ Record_patch (record, [], inj)
    | MapPatch _ -> simple_fail "no map patch yet"
    | SetPatch _ -> simple_fail "no set patch yet"
    | MapRemove _ -> simple_fail "no map remove yet"
    | SetRemove _ -> simple_fail "no set remove yet"

  and simpl_cases : (Raw.pattern * block) list -> matching result = fun t ->
    let open Raw in
    let get_var (t:Raw.pattern) = match t with
      | PVar v -> ok v.value
      | _ -> simple_fail "not a var"
    in
    match t with
    | [(PFalse _, f) ; (PTrue _, t)]
    | [(PTrue _, t) ; (PFalse _, f)] -> ok @@ Match_bool {match_true = t ; match_false = f}
    | [(PSome v, some) ; (PNone _, none)]
    | [(PNone _, none) ; (PSome v, some)] -> (
        let (_, v) = v.value in
        let%bind v = match v.value.inside with
        | PVar v -> ok v.value
        | _ -> simple_fail "complex patterns not supported yet" in
        ok @@ Match_option {match_none = none ; match_some = (v, some) }
      )
    | [(PCons c, cons) ; (PList (PNil _), nil)]
    | [(PList (PNil _), nil) ; (PCons c, cons)] ->
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
end

module Combinators = struct
  let t_bool      : type_expression = Type_constant ("bool", [])
  let t_string    : type_expression = Type_constant ("string", [])
  let t_bytes     : type_expression = Type_constant ("bytes", [])
  let t_int       : type_expression = Type_constant ("int", [])
  let t_unit      : type_expression = Type_constant ("unit", [])
  let t_tuple lst : type_expression = Type_tuple lst
  let t_pair a b = t_tuple [a ; b]
  let t_record m  : type_expression = (Type_record m)
  let t_ez_record (lst:(string * type_expression) list) : type_expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Type_record map

  let t_record_ez lst =
    let m = SMap.of_list lst in
    t_record m

  let t_sum m : type_expression = Type_sum m
  let make_t_ez_sum (lst:(string * type_expression) list) : type_expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Type_sum map

  let t_function param result : type_expression = Type_function (param, result)

  let annotated_expression ?type_annotation expression = {expression ; type_annotation}

  let name (s : string) : name = s

  let var (s : string) : expression = Variable s

  let unit  () : expression = Literal (Unit)
  let number n : expression = Literal (Number n)
  let bool   b : expression = Literal (Bool b)
  let string s : expression = Literal (String s)
  let bytes  b : expression = Literal (Bytes (Bytes.of_string b))

  let lambda (binder : string)
             (input_type : type_expression)
             (output_type : type_expression)
             (result : expression)
             (body : block)
      : expression =
    Lambda {
        binder = (name binder) ;
        input_type = input_type ;
        output_type = output_type ;
        result = (ae result) ;
        body ;
      }

  let tuple (lst : ae list) : expression = Tuple lst
  let ez_tuple (lst : expression list) : expression =
    tuple (List.map (fun e -> ae e) lst)

  let constructor (s : string) (e : ae) : expression = Constructor (name s, e)

  let record (lst : (string * ae) list) : expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Record map

  let ez_record  (lst : (string * expression) list) : expression =
    (* TODO: define a correct implementation of List.map
     * (an implementation that does not fail with stack overflow) *)
    record (List.map (fun (s,e) -> (s, ae e)) lst)
end
