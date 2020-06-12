open Errors_pascaligo
open Trace
open Ast_imperative

module Raw = Parser.Pascaligo.AST
module SMap = Map.String
(* module ParserLog = Parser_pascaligo.ParserLog *)

open Combinators

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let pseq_to_list = function
  None -> []
| Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

open Operators.Concrete_to_imperative.Pascaligo

let r_split = Location.r_split

(* Statements can't be simplified in isolation. [a ; b ; c] can get
   simplified either as [let x = expr in (b ; c)] if [a] is a [const x
   = expr] declaration or as [sequence(a, sequence(b, c))] for
   everything else.  Because of this, abstracting sequences depend on
   their contents. To avoid peeking in their contents, we instead
   simplify sequences elements as functions from their next elements
   to the actual result.

   For [return_let_in], if there is no follow-up element, an error is
   triggered, as you can't have [let x = expr in ...] with no [...]. A
   cleaner option might be to add a [unit] instead of failing.

   [return_statement] is used for non-let-in statements.
 *)

let return_let_in ?loc binder inline rhs = ok @@ fun expr'_opt ->
  match expr'_opt with
  | None -> ok @@ e_let_in ?loc binder inline rhs (e_skip ())
  | Some expr' -> ok @@ e_let_in ?loc binder inline rhs expr'

let return_statement expr = ok @@ fun expr'_opt ->
  match expr'_opt with
  | None -> ok @@ expr
  | Some expr' -> ok @@ e_sequence expr expr'

let get_t_string_singleton_opt = function
  | Raw.TString s -> Some s.value
  | _ -> None


let rec compile_type_expression (t:Raw.type_expr) : (type_expression , (abs_error)) result =
  match t with
    TPar x -> compile_type_expression x.value.inside
  | TVar v -> (
      let (v,loc) = r_split v in
      match type_constants v with
      | Some s -> ok @@ make_t ~loc @@ T_constant s
      | None -> ok @@ make_t ~loc @@ T_variable (Var.of_name v)
    )
  | TFun x -> (
      let (x,loc) = r_split x in
      let%bind (a , b) =
        let (a , _ , b) = x in
        bind_map_pair compile_type_expression (a , b) in
      ok @@ make_t ~loc @@ T_arrow {type1=a;type2=b}
    )
  | TApp x ->
      let (x, loc) = r_split x in
      let (name, tuple) = x in
      (match name.value with
        | "michelson_or" ->
          let lst = npseq_to_list tuple.value.inside in
          (match lst with
          | [a ; b ; c ; d ] -> (
            let%bind b' =
              trace_option (michelson_type_wrong t name.value) @@
                get_t_string_singleton_opt b in
            let%bind d' =
              trace_option (michelson_type_wrong t name.value) @@
                get_t_string_singleton_opt d in
            let%bind a' = compile_type_expression a in
            let%bind c' = compile_type_expression c in
            ok @@ t_michelson_or ~loc a' b' c' d'
            )
          | _ -> fail @@ michelson_type_wrong_arity loc name.value)
        | "michelson_pair" ->
          let lst = npseq_to_list tuple.value.inside in
          (match lst with
          | [a ; b ; c ; d ] -> (
            let%bind b' =
              trace_option (michelson_type_wrong t name.value) @@
                get_t_string_singleton_opt b in
            let%bind d' =
              trace_option (michelson_type_wrong t name.value) @@
                get_t_string_singleton_opt d in
            let%bind a' = compile_type_expression a in
            let%bind c' = compile_type_expression c in
            ok @@ t_michelson_pair ~loc a' b' c' d'
            )
          | _ -> fail @@ michelson_type_wrong_arity loc name.value)
        | _ ->
          let lst = npseq_to_list tuple.value.inside in
          let%bind lst =
            bind_list @@ List.map compile_type_expression lst in (** TODO: fix constant and operator*)
          let%bind cst =
            trace_option (unknown_predefined_type name) @@
            type_operators name.value in
          ok @@ t_operator ~loc cst lst )
  | TProd p ->
      let%bind tpl = compile_list_type_expression
    @@ npseq_to_list p.value in
      ok tpl
  | TRecord r ->
      let (r,loc ) = r_split r in
      let aux = fun (x, y) ->
        let%bind y = compile_type_expression y in
        ok (x, y)
      in
      let order = fun i (x,y) ->
        ((x,i),y)
      in
      let apply =
        fun (x:Raw.field_decl Raw.reg) -> (x.value.field_name.value, x.value.field_type) in
      let%bind lst = bind_list
        @@ List.map aux
        @@ List.mapi order
        @@ List.map apply
        @@ npseq_to_list r.ne_elements in
      let m = List.fold_left (fun m ((x,i), y) -> LMap.add (Label x) {field_type=y;field_decl_pos=i} m) LMap.empty lst in
      ok @@ make_t ~loc @@ T_record m
  | TSum s ->
      let (s,loc) = r_split s in
      let aux i (v:Raw.variant Raw.reg) =
        let args =
          match v.value.arg with
            None -> []
          | Some (_, TProd product) -> npseq_to_list product.value
          | Some (_, t_expr) -> [t_expr] in
        let%bind te = compile_list_type_expression @@ args in
        ok ((v.value.constr.value,i), te)
      in
      let%bind lst = bind_list
        @@ List.mapi aux
        @@ npseq_to_list s in
      let m = List.fold_left (fun m ((x,i), y) -> CMap.add (Constructor x) {ctor_type=y;ctor_decl_pos=i} m) CMap.empty lst in
      ok @@ make_t ~loc @@ T_sum m
  | TString _s -> fail @@ unsupported_string_singleton t

and compile_list_type_expression (lst:Raw.type_expr list) : (type_expression , (abs_error)) result =
  match lst with
  | [] -> ok @@ t_unit ()
  | [hd] -> compile_type_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map compile_type_expression lst in
      ok @@ t_tuple lst

let compile_projection : Raw.projection Region.reg -> _ = fun p ->
  let (p' , loc) = r_split p in
  let var =
    let name = Var.of_name p'.struct_name.value in
    e_variable name in
  let path = p'.field_path in
  let path' =
    let aux (s:Raw.selection) =
      match s with
      | FieldName property -> Access_record property.value
      | Component index -> (Access_tuple (snd index.value))
    in
    List.map aux @@ npseq_to_list path in
  ok @@ e_accessor ~loc var path'


let rec compile_expression (t:Raw.expr) : (expr , (abs_error)) result =
  let return x = ok x in
  match t with
  | EAnnot a -> (
      let par, loc = r_split a in
      let expr, _, type_expr = par.inside in
      let%bind expr' = compile_expression expr in
      let%bind type_expr' = compile_type_expression type_expr in
      return @@ e_annotation ~loc expr' type_expr'
    )
  | EVar c -> (
      let (c', loc) = r_split c in
      match constants c' with
      | None   -> return @@ e_variable ~loc (Var.of_name c.value)
      | Some s -> return @@ e_constant ~loc s []
    )
  | ECall x -> (
      let ((f, args), loc) = r_split x in
      let (args, args_loc) = r_split args in
      let args' = npseq_to_list args.inside in
      match f with
      | EVar name -> (
        let (f_name , f_loc) = r_split name in
        match constants f_name with
        | None ->
           let%bind arg = compile_tuple_expression ~loc:args_loc args' in
           return @@ e_application ~loc (e_variable ~loc:f_loc (Var.of_name f_name)) arg
        | Some s ->
           let%bind lst = bind_map_list compile_expression args' in
           return @@ e_constant ~loc s lst
      )
      | f -> (
        let%bind f' = compile_expression f in
        let%bind arg = compile_tuple_expression ~loc:args_loc args' in
        return @@ e_application ~loc f' arg
      )
    )
  | EPar x -> compile_expression x.value.inside
  | EUnit reg ->
    let loc = Location.lift reg in
    return @@ e_literal ~loc Literal_unit
  | EBytes x ->
    let (x' , loc) = r_split x in
    return @@ e_literal ~loc (Literal_bytes (Hex.to_bytes @@ snd x'))
  | ETuple tpl ->
      let (tpl' , loc) = r_split tpl in
      compile_tuple_expression ~loc @@ npseq_to_list tpl'.inside
  | ERecord r ->
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = compile_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assignment Raw.reg) ->
                      (x.value.field_name, x.value.field_expr))
        @@ npseq_to_list r.value.ne_elements in
      let aux prev (k, v) = SMap.add k v prev in
      return @@ e_record (List.fold_left aux SMap.empty fields)
  | EProj p -> compile_projection p
  | EUpdate u -> compile_update u
  | EConstr (ConstrApp c) -> (
      let ((c, args) , loc) = r_split c in
      match args with
        None ->
        return @@ e_constructor ~loc c.value (e_unit ())
      | Some args ->
          let args, args_loc = r_split args in
          let%bind arg =
            compile_tuple_expression ~loc:args_loc
            @@ npseq_to_list args.inside in
          return @@ e_constructor ~loc c.value arg
    )
  | EConstr (SomeApp a) ->
      let ((_, args) , loc) = r_split a in
      let (args , args_loc) = r_split args in
      let%bind arg =
        compile_tuple_expression ~loc:args_loc
        @@ npseq_to_list args.inside in
      return @@ e_constant ~loc C_SOME [arg]
  | EConstr (NoneExpr reg) -> (
      let loc = Location.lift reg in
      return @@ e_none ~loc ()
    )
  | EArith (Add c) ->
      compile_binop "ADD" c
  | EArith (Sub c) ->
      compile_binop "SUB" c
  | EArith (Mult c) ->
      compile_binop "TIMES" c
  | EArith (Div c) ->
      compile_binop "DIV" c
  | EArith (Mod c) ->
      compile_binop "MOD" c
  | EArith (Int n) -> (
      let (n , loc) = r_split n in
      let n = snd n in
      return @@ e_literal ~loc (Literal_int n)
    )
  | EArith (Nat n) -> (
      let (n , loc) = r_split n in
      let n = snd @@ n in
      return @@ e_literal ~loc (Literal_nat n)
    )
  | EArith (Mutez n) -> (
    let (n , loc) = r_split n in
    let n = snd @@ n in
    return @@ e_literal ~loc (Literal_mutez n)
  )
  | EArith (Neg e) -> compile_unop "NEG" e
  | EString (String s) ->
    let (s , loc) = r_split s in
      return @@ e_literal ~loc (Literal_string (Standard s))
  | EString (Verbatim v) ->
    let (v , loc) = r_split v in
      return @@ e_literal ~loc (Literal_string (Verbatim v))
  | EString (Cat bo) ->
    let (bo , loc) = r_split bo in
    let%bind sl = compile_expression bo.arg1 in
    let%bind sr = compile_expression bo.arg2 in
    return @@ e_string_cat ~loc sl sr
  | ELogic l -> compile_logic_expression l
  | EList l -> compile_list_expression l
  | ESet s -> compile_set_expression s
  | ECond c ->
      let (c , loc) = r_split c in
      let%bind expr = compile_expression c.test in
      let%bind match_true = compile_expression c.ifso in
      let%bind match_false = compile_expression c.ifnot in
      return @@ e_cond ~loc expr match_true match_false

  | ECase c -> (
      let (c , loc) = r_split c in
      let%bind e = compile_expression c.expr in
      let%bind lst =
        let aux (x : Raw.expr Raw.case_clause) =
          let%bind expr = compile_expression x.rhs in
          ok (x.pattern, expr) in
        bind_list
        @@ List.map aux
        @@ List.map get_value
        @@ npseq_to_list c.cases.value in
      let%bind cases = compile_cases lst in
      return @@ e_matching ~loc e cases
    )
  | EMap (MapInj mi)  -> (
      let (mi , loc) = r_split mi in
      let%bind lst =
        let lst = List.map get_value @@ pseq_to_list mi.elements in
        let aux : Raw.binding -> (expression * expression, (abs_error)) result =
          fun b ->
            let%bind src = compile_expression b.source in
            let%bind dst = compile_expression b.image in
            ok (src, dst) in
        bind_map_list aux lst in
      return @@ e_map ~loc lst
    )
  | EMap (BigMapInj mi) -> (
      let (mi , loc) = r_split mi in
      let%bind lst =
        let lst = List.map get_value @@ pseq_to_list mi.elements in
        let aux : Raw.binding -> (expression * expression, (abs_error)) result =
          fun b ->
            let%bind src = compile_expression b.source in
            let%bind dst = compile_expression b.image in
            ok (src, dst) in
        bind_map_list aux lst in
      return @@ e_big_map ~loc lst
    )
  | EMap (MapLookUp lu) -> (
      let (lu , loc) = r_split lu in
      let%bind path = match lu.path with
        | Name v -> (
            let (v , loc) = r_split v in
            return @@ e_variable ~loc (Var.of_name v)
          )
        | Path p -> compile_projection p
      in
      let%bind index = compile_expression lu.index.value.inside in
      return @@ e_accessor ~loc path [Access_map index]
    )
  | EFun f ->
    let (f , loc) = r_split f in
    let%bind (_ty_opt, f') = compile_fun_expression ~loc f
    in return @@ f'
  | ECodeInsert ci ->
    let (ci, loc) = r_split ci in
    let      language  = ci.language.value in
    let%bind code      = compile_expression ci.code in
    return @@ e_raw_code ~loc language code

and compile_update (u: Raw.update Region.reg) =
  let u, loc     = r_split u in
  let name, path = compile_path u.record in
  let var        = e_variable (Var.of_name name) in
  let record     = if path = [] then var else e_accessor var path in
  let updates    = u.updates.value.ne_elements in
  let%bind updates' =
    let aux (f: Raw.field_path_assignment Raw.reg) =
      let f, _ = r_split f in
      let%bind expr = compile_expression f.field_expr
      in ok (compile_path f.field_path, expr)
    in bind_map_list aux @@ npseq_to_list updates in
  let aux ur ((var, path), expr) =
    ok @@ e_update ~loc ur (Access_record var :: path) expr
  in bind_fold_list aux record updates'

and compile_logic_expression (t:Raw.logic_expr) : (expression , (abs_error)) result =
  match t with
  | BoolExpr (False reg) ->
      ok @@ e_bool ~loc:(Location.lift reg) false
  | BoolExpr (True reg) ->
     ok @@ e_bool ~loc:(Location.lift reg) true
  | BoolExpr (Or b) ->
      compile_binop "OR" b
  | BoolExpr (And b) ->
      compile_binop "AND" b
  | BoolExpr (Not b) ->
      compile_unop "NOT" b
  | CompExpr (Lt c) ->
      compile_binop "LT" c
  | CompExpr (Gt c) ->
      compile_binop "GT" c
  | CompExpr (Leq c) ->
      compile_binop "LE" c
  | CompExpr (Geq c) ->
      compile_binop "GE" c
  | CompExpr (Equal c) ->
      compile_binop "EQ" c
  | CompExpr (Neq c) ->
      compile_binop "NEQ" c

and compile_list_expression (t:Raw.list_expr) : (expression , (abs_error)) result =
  let return x = ok x in
  match t with
    ECons c ->
      compile_binop "CONS" c
  | EListComp lst ->
      let (lst , loc) = r_split lst in
      let%bind lst' =
        bind_map_list compile_expression @@
        pseq_to_list lst.elements in
      return @@ e_list ~loc lst'
  | ENil reg ->
      let loc = Location.lift reg in
      return @@ e_list ~loc []

and compile_set_expression (t:Raw.set_expr) : (expression , (abs_error)) result =
  match t with
  | SetMem x -> (
    let (x' , loc) = r_split x in
    let%bind set' = compile_expression x'.set in
    let%bind element' = compile_expression x'.element in
    ok @@ e_constant ~loc C_SET_MEM [ element' ; set' ]
  )
  | SetInj x -> (
    let (x' , loc) = r_split x in
    let elements = pseq_to_list x'.elements in
    let%bind elements' = bind_map_list compile_expression elements in
    ok @@ e_set ~loc elements'
  )

and compile_binop (name:string) (t:_ Raw.bin_op Region.reg) : (expression , (abs_error)) result =
  let return x = ok x in
  let (t , loc) = r_split t in
  let%bind a = compile_expression t.arg1 in
  let%bind b = compile_expression t.arg2 in
  let%bind name = trace_option (unknown_built_in name) @@ constants name in
  return @@ e_constant ~loc name [ a ; b ]

and compile_unop (name:string) (t:_ Raw.un_op Region.reg) : (expression , (abs_error)) result =
  let return x = ok x in
  let (t , loc) = r_split t in
  let%bind a = compile_expression t.arg in
  let%bind name = trace_option (unknown_built_in name) @@ constants name in
  return @@ e_constant ~loc name [ a ]

and compile_tuple_expression ?loc (lst:Raw.expr list) : (expression , (abs_error)) result =
  let return x = ok x in
  match lst with
  | [] -> return @@ e_literal Literal_unit
  | [hd] -> compile_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map compile_expression lst
      in return @@ e_tuple ?loc lst

and compile_data_declaration : Raw.data_decl -> _ result =
  fun t ->
  match t with
  | LocalVar x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = compile_type_expression x.var_type in
      let%bind expression = compile_expression x.init in
      return_let_in ~loc (Var.of_name name, Some t) false expression
  | LocalConst x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = compile_type_expression x.const_type in
      let%bind expression = compile_expression x.init in
      let inline =
        match x.attributes with
          None -> false
        | Some {value; _} ->
           npseq_to_list value.ne_elements
           |> List.exists (fun Region.{value; _} -> value = "\"inline\"")
      in return_let_in ~loc (Var.of_name name, Some t) inline expression
  | LocalFun f  ->
      let (f , loc) = r_split f in
      let%bind (binder, expr) = compile_fun_decl ~loc f in
      let inline =
        match f.attributes with
          None -> false
        | Some {value; _} ->
           npseq_to_list value.ne_elements
           |> List.exists (fun Region.{value; _} -> value = "\"inline\"")
      in return_let_in ~loc binder inline expr

and compile_param :
      Raw.param_decl -> (string * type_expression, (abs_error)) result =
  fun t ->
  match t with
  | ParamConst c ->
      let c = c.value in
      let param_name = c.var.value in
      let%bind type_expression = compile_type_expression c.param_type in
      ok (param_name , type_expression)
  | ParamVar v ->
      let c = v.value in
      let param_name = c.var.value in
      let%bind type_expression = compile_type_expression c.param_type in
      ok (param_name , type_expression)

and compile_fun_decl :
      loc:_ -> Raw.fun_decl ->
      ((expression_variable * type_expression option) * expression , (abs_error)) result =
  fun ~loc x ->
  let open! Raw in
  let {kwd_recursive;fun_name; param; ret_type; block_with;
       return; attributes} : fun_decl = x in
  let inline =
    match attributes with
      None -> false
    | Some {value; _} ->
       npseq_to_list value.ne_elements
       |> List.exists (fun Region.{value; _} -> value = "\"inline\"") in
  let statements =
    match block_with with
    | Some (block,_) -> npseq_to_list block.value.statements
    | None -> []
  in
  (match param.value.inside with
     a, [] -> (
       let%bind input = compile_param a in
       let (binder , input_type) = input in
       let%bind instructions = compile_statement_list statements in
       let%bind result = compile_expression return in
       let%bind output_type = compile_type_expression ret_type in
       let body = instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
      let binder = Var.of_name binder in
      let fun_name = Var.of_name fun_name.value in
      let fun_type = t_function input_type output_type in
      let expression : expression =
        e_lambda ~loc binder (Some input_type)(Some output_type) result in
      let%bind expression = match kwd_recursive with
        None -> ok @@ expression |
        Some _ -> ok @@ e_recursive ~loc fun_name fun_type
        @@ {binder;input_type=Some input_type; output_type= Some output_type; result}
      in
       ok ((fun_name, Some fun_type), expression)
     )
   | lst -> (
       let lst = npseq_to_list lst in
       (* TODO wrong, should be fresh? *)
       let arguments_name = Var.of_name "arguments" in
       let%bind params = bind_map_list compile_param lst in
       let (binder , input_type) =
         let type_expression = t_tuple (List.map snd params) in
         (arguments_name , type_expression) in
       let%bind tpl_declarations =
         let aux = fun i (param, type_expr) ->
           let expr =
             e_accessor (e_variable arguments_name) [Access_record (string_of_int i)] in
           let type_variable = Some type_expr in
           let ass = return_let_in (Var.of_name param , type_variable) inline expr in
           ass
         in
         bind_list @@ List.mapi aux params in
       let%bind instructions = compile_statement_list statements in
       let%bind result = compile_expression return in
       let%bind output_type = compile_type_expression ret_type in
       let body = tpl_declarations @ instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
      let fun_name = Var.of_name fun_name.value in
      let fun_type = t_function input_type output_type in
      let expression : expression =
        e_lambda ~loc binder (Some input_type)(Some output_type) result in
      let%bind expression = match kwd_recursive with
        None -> ok @@ expression |
        Some _ -> ok @@ e_recursive ~loc fun_name fun_type
        @@ {binder;input_type=Some input_type; output_type= Some output_type; result}
      in
       ok ((fun_name, Some fun_type), expression)
     )
  )

and compile_fun_expression :
  loc:_ -> Raw.fun_expr -> (type_expression option * expression , (abs_error)) result =
  fun ~loc x ->
  let open! Raw in
  let {param; ret_type; return; _} : fun_expr = x in
  let statements = [] in
  (match param.value.inside with
    a, [] -> (
        let%bind input = compile_param a in
        let (binder , input_type) = input in
        let%bind instructions = compile_statement_list statements in
        let%bind result = compile_expression return in
        let%bind output_type = compile_type_expression ret_type in

        let body = instructions in
        let%bind result =
          let aux prec cur = cur (Some prec) in
          bind_fold_right_list aux result body in
        let binder = Var.of_name binder in
        let fun_type = t_function input_type output_type in
        let expression =
          e_lambda ~loc binder (Some input_type)(Some output_type) result
        in
        ok (Some fun_type , expression)
      )
   | lst -> (
       let lst = npseq_to_list lst in
       (* TODO wrong, should be fresh? *)
       let arguments_name = Var.of_name "arguments" in
       let%bind params = bind_map_list compile_param lst in
       let (binder , input_type) =
         let type_expression = t_tuple (List.map snd params) in
         (arguments_name , type_expression) in
       let%bind tpl_declarations =
         let aux = fun i (param, param_type) ->
           let expr = e_accessor (e_variable arguments_name) [Access_tuple (Z.of_int i)] in
           let type_variable = Some param_type in
           let ass = return_let_in (Var.of_name param , type_variable) false expr in
           ass
         in
         bind_list @@ List.mapi aux params in
       let%bind instructions = compile_statement_list statements in
       let%bind result = compile_expression return in
       let%bind output_type = compile_type_expression ret_type in
       let body = tpl_declarations @ instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
      let fun_type = t_function input_type output_type in
      let expression =
        e_lambda ~loc binder (Some input_type)(Some output_type) result
      in
      ok (Some fun_type , expression)
     )
  )

and compile_statement_list statements =
  let open Raw in
  let rec hook acc = function
    [] -> acc
  | [Attr _] ->
      (* Detached attributes are erased. TODO: Warning. *)
      acc
  | Attr _ :: (Attr _ :: _ as statements) ->
      (* Detached attributes are erased. TODO: Warning. *)
      hook acc statements
  | Attr decl :: Data (LocalConst {value; region}) :: statements ->
      let new_const =
        Data (LocalConst {value = {value with attributes = Some decl}; region})
      in hook acc (new_const :: statements)
  | Attr decl :: Data (LocalFun {value; region}) :: statements ->
      let new_fun =
        Data (LocalFun {value = {value with attributes = Some decl}; region})
      in hook acc (new_fun :: statements)
  | Attr _ :: statements ->
      (* Detached attributes are erased. TODO: Warning. *)
      hook acc statements
  | Instr i :: statements ->
      hook (compile_instruction i :: acc) statements
  | Data d :: statements ->
      hook (compile_data_declaration d :: acc) statements
  in bind_list @@ hook [] (List.rev statements)

and compile_single_instruction : Raw.instruction -> ((_ -> (expression , (abs_error)) result), (abs_error)) result =
  fun t ->
  match t with
  | ProcCall x -> (
    let (f, args) , loc = r_split x in
    let args, args_loc = r_split args in
    let args' = npseq_to_list args.inside in
    match f with
    | EVar name -> (
      let (f_name , f_loc) = r_split name in
      match constants f_name with
      | None  ->
         let%bind arg = compile_tuple_expression ~loc:args_loc args' in
         return_statement @@ e_application ~loc (e_variable ~loc:f_loc (Var.of_name f_name)) arg
      | Some s ->
         let%bind lst = bind_map_list compile_expression args' in
         return_statement @@ e_constant ~loc s lst
    )
    | f -> (
      let%bind f' = compile_expression f in
      let%bind arg = compile_tuple_expression ~loc:args_loc args' in
      return_statement @@ e_application ~loc f' arg
    )
  )
  | Skip reg -> (
      let loc = Location.lift reg in
      return_statement @@ e_skip ~loc ()
    )
  | Loop (While l) ->
      let (wl, loc) = r_split l in
      let%bind condition = compile_expression wl.cond in
      let%bind body = compile_block wl.block.value in
      let%bind body = body @@ None in
      return_statement @@ e_while ~loc condition body
  | Loop (For (ForInt fi)) -> (
      let (fi,loc) = r_split fi in
      let binder = Var.of_name fi.assign.value.name.value in
      let%bind start = compile_expression fi.assign.value.expr in
      let%bind bound = compile_expression fi.bound in
      let%bind step = match fi.step with
        | None -> ok @@ e_int_z Z.one
        | Some (_, step) -> compile_expression step in
      let%bind body = compile_block fi.block.value in
      let%bind body = body @@ None in
      return_statement @@ e_for ~loc binder start bound step body
  )
  | Loop (For (ForCollect fc)) ->
      let (fc,loc) = r_split fc in
      let binder = (Var.of_name fc.var.value, Option.map (fun x -> Var.of_name (snd x:string Raw.reg).value) fc.bind_to) in
      let%bind collection = compile_expression fc.expr in
      let collection_type = match fc.collection with
        | Map _  -> Map
        | Set _  -> Set
        | List _ -> List
      in
      let%bind body = compile_block fc.block.value in
      let%bind body = body @@ None in
      return_statement @@ e_for_each ~loc binder collection collection_type body
  | Cond c -> (
      let (c , loc) = r_split c in
      let%bind expr = compile_expression c.test in
      let%bind match_true = match c.ifso with
          ClauseInstr i ->
            compile_single_instruction i
        | ClauseBlock b ->
            match b with
              LongBlock {value; _} ->
                compile_block value
            | ShortBlock {value; _} ->
                compile_statements @@ fst value.inside in
      let%bind match_false = match c.ifnot with
          ClauseInstr i ->
            compile_single_instruction i
        | ClauseBlock b ->
            match b with
              LongBlock {value; _} ->
                compile_block value
            | ShortBlock {value; _} ->
                compile_statements @@ fst value.inside in

      let%bind match_true = match_true None in
      let%bind match_false = match_false None in
      return_statement @@ e_cond ~loc expr match_true match_false
    )
  | Assign a -> (
      let (a , loc) = r_split a in
      let%bind value_expr = compile_expression a.rhs in
      match a.lhs with
        | Path path ->
            let name , path' = compile_path path in
            let name = Var.of_name name in
            return_statement @@ e_assign ~loc name path' value_expr
        | MapPath v ->
            let v' = v.value in
            let%bind (varname,map,path) = match v'.path with
              | Name name ->
                  ok (name.value ,
                      e_variable (Var.of_name name.value), [])
              | Path p ->
                  let name, p' = compile_path v'.path in
                  let%bind accessor = compile_projection p in
                  ok @@ (name, accessor, p') in
            let%bind key_expr =
              compile_expression v'.index.value.inside in
            let expr' = e_map_add key_expr value_expr map in
            let varname = Var.of_name varname in
            return_statement @@ e_assign ~loc varname path expr'
    )
  | CaseInstr c -> (
      let (c , loc) = r_split c in
      let%bind expr = compile_expression c.expr in
      let%bind cases =
        let aux (x : Raw.if_clause Raw.case_clause Raw.reg) =
          let%bind case_clause =
            match x.value.rhs with
              ClauseInstr i ->
                compile_single_instruction i
            | ClauseBlock b ->
                match b with
                  LongBlock {value; _} ->
                    compile_block value
                | ShortBlock {value; _} ->
                    compile_statements @@ fst value.inside in
          let%bind case_clause = case_clause None in
          ok (x.value.pattern, case_clause) in
        bind_list
        @@ List.map aux
        @@ npseq_to_list c.cases.value in
      let%bind m = compile_cases cases in
      return_statement @@ e_matching ~loc expr m
    )
  | RecordPatch r ->
      let reg = r.region in
      let r, loc = r_split r in
      let aux (fa: Raw.field_assignment Raw.reg) : Raw.field_path_assignment Raw.reg =
        {value = {field_path = Name fa.value.field_name;
                  assignment = fa.value.assignment;
                  field_expr = fa.value.field_expr};
         region = fa.region} in
      let update : Raw.field_path_assignment Raw.reg Raw.ne_injection Raw.reg = {
        value  = Raw.map_ne_injection aux r.record_inj.value;
        region = r.record_inj.region} in
      let u : Raw.update = {
          record   = r.path;
          kwd_with = r.kwd_with;
          updates  = update} in
      let%bind expr = compile_update {value=u;region=reg} in
      let name, access_path = compile_path r.path in
      let name = Var.of_name name in
      return_statement @@ e_assign ~loc name access_path expr
  | MapPatch patch ->
      let map_p, loc = r_split patch in
      let name, access_path = compile_path map_p.path in
      let%bind inj = bind_list
          @@ List.map (fun (x:Raw.binding Region.reg) ->
            let x = x.value in
            let (key, value) = x.source, x.image in
            let%bind key' = compile_expression key in
            let%bind value' = compile_expression value
            in ok @@ (key', value')
          )
        @@ npseq_to_list map_p.map_inj.value.ne_elements in
      (match inj with
      | [] -> return_statement @@ e_skip ~loc ()
      | _ :: _ ->
        let assigns = List.fold_right
            (fun (key, value) map ->  (e_map_add key value map))
            inj
            (e_accessor ~loc (e_variable (Var.of_name name)) access_path)
        and name = Var.of_name name in
        return_statement @@ e_assign ~loc name access_path assigns)
  | SetPatch patch -> (
      let setp, loc = r_split patch in
      let name, access_path = compile_path setp.path in
      let%bind inj =
        bind_list @@
        List.map compile_expression @@
        npseq_to_list setp.set_inj.value.ne_elements in
      match inj with
      | [] -> return_statement @@ e_skip ~loc ()
      | _ :: _ ->
        let assigns = List.fold_right
          (fun hd s -> e_constant C_SET_ADD [hd ; s])
          inj (e_accessor ~loc (e_variable (Var.of_name name)) access_path) in
        let name = Var.of_name name in
        return_statement @@ e_assign ~loc name access_path assigns
    )
  | MapRemove r ->
      let (v , loc) = r_split r in
      let key = v.key in
      let%bind (name,map,path) = match v.map with
        | Name v -> ok (v.value , e_variable (Var.of_name v.value) , [])
        | Path p ->
          let name, p' = compile_path v.map in
          let%bind accessor = compile_projection p in
          ok @@ (name , accessor , p')
      in
      let%bind key' = compile_expression key in
      let expr = e_constant ~loc C_MAP_REMOVE [key' ; map] in
      let name = Var.of_name name in
      return_statement @@ e_assign ~loc name path expr
  | SetRemove r ->
      let set_rm, loc = r_split r in
      let%bind (name, set, path) =
        match set_rm.set with
        | Name v ->
            ok (v.value, e_variable (Var.of_name v.value), [])
        | Path path ->
            let name, p' = compile_path set_rm.set in
            let%bind accessor = compile_projection path in
            ok @@ (name, accessor, p') in
      let%bind removed' = compile_expression set_rm.element in
      let expr = e_constant ~loc C_SET_REMOVE [removed' ; set] in
      let name = Var.of_name name in
      return_statement @@ e_assign ~loc name path expr

and compile_path : Raw.path -> string * access list = function
  Raw.Name v -> v.value, []
| Raw.Path {value; _} ->
    let Raw.{struct_name; field_path; _} = value in
    let var  = struct_name.value in
    let path = List.map compile_selection @@ npseq_to_list field_path
    in var, path

and compile_selection : Raw.selection -> access = function
  FieldName property -> Access_record property.value
| Component index    -> Access_tuple (snd index.value)

and compile_cases : (Raw.pattern * expression) list -> (matching_expr , (abs_error)) result = fun t ->
  let open Raw in
  let get_var (t:Raw.pattern) =
    match t with
    | PVar v -> ok v.value
    | p -> fail @@ unsupported_non_var_pattern p in
  let get_tuple (t: Raw.pattern) =
    match t with
    | PTuple v -> npseq_to_list v.value.inside
    | x -> [ x ] in
  let get_single (t: Raw.pattern) =
    let t' = get_tuple t in
    let%bind () =
      Assert.assert_list_size (unsupported_tuple_pattern t) t' 1 in
    ok (List.hd t') in
  let get_toplevel (t : Raw.pattern) =
    match t with
    | PList PCons x -> (
        let (x' , lst) = x.value in
        match lst with
        | [] -> ok x'
        | _ -> ok t
      )
    | pattern -> ok pattern in
  let get_constr (t: Raw.pattern) =
    match t with
    | PConstr (PConstrApp v) -> (
      let value = v.value in
      match value with
      | constr, None ->
         ok (constr.value, "unit")
      | _ ->
       let const, pat_opt = v.value in
        let%bind pat =
          trace_option (unsupported_cst_constr t) @@
          pat_opt in
        let%bind single_pat = get_single (PTuple pat) in
        let%bind var = get_var single_pat in
        ok (const.value , var)
      )
    | _ -> fail @@ only_constructors t in
  let%bind patterns =
    let aux (x , y) =
      let%bind x' = get_toplevel x in
      ok (x' , y)
    in bind_map_list aux t in
  match patterns with
  | [(PConstr PFalse _ , f) ; (PConstr PTrue _ , t)]
  | [(PConstr PTrue _ , t) ; (PConstr PFalse _ , f)] ->
      ok @@ Match_variant ([((Constructor "true", Var.of_name "_"), t); ((Constructor "false", Var.of_name "_"), f)])
  | [(PConstr PSomeApp v , some) ; (PConstr PNone _ , none)]
  | [(PConstr PNone _ , none) ; (PConstr PSomeApp v , some)] -> (
      let (_, v) = v.value in
      let%bind v = match v.value.inside with
        | PVar v -> ok v.value
        | p -> fail @@ unsupported_deep_some_patterns p in
      ok @@ Match_option {match_none = none ; match_some = (Var.of_name v, some) }
    )
  | [(PList PCons c, cons) ; (PList (PNil _), nil)]
  | [(PList (PNil _), nil) ; (PList PCons c, cons)] ->
      let%bind (a, b) =
        match c.value with
        | a, [(_, b)] ->
            let%bind a = get_var a in
            let%bind b = get_var b in
            ok (a, b)
        | _ -> fail @@ unsupported_deep_list_patterns c

      in
      ok @@ Match_list {match_cons = (Var.of_name a, Var.of_name b, cons) ; match_nil = nil}
  | lst ->
      let%bind constrs =
        trace_strong (unsupported_pattern_type (List.map fst lst)) @@
        let aux (x , y) =
          let%bind x' =
            get_constr x in
          ok (x' , y) in
        bind_map_list aux lst in
      ok @@ ez_match_variant constrs

and compile_instruction : Raw.instruction -> ((_ -> (expression, (abs_error)) result) , (abs_error)) result =
  fun t -> trace (abstracting_instruction_tracer t) @@ compile_single_instruction t

and compile_statements : Raw.statements -> ((_ -> (expression,(abs_error)) result) , (abs_error)) result =
  fun statements ->
    let lst = npseq_to_list statements in
    let%bind fs = compile_statement_list lst in
    let aux : _ -> (expression option -> (expression, (abs_error)) result) -> _ =
      fun prec cur ->
        let%bind res = cur prec
        in ok @@ Some res in
    ok @@ fun (expr' : _ option) ->
           let%bind ret = bind_fold_right_list aux expr' fs in
           ok @@ Option.unopt_exn ret

and compile_block : Raw.block -> ((_ -> (expression , (abs_error)) result) , (abs_error)) result =
  fun t -> compile_statements t.statements


and compile_declaration_list declarations : (declaration Location.wrap list, (abs_error)) result =
  let open Raw in
  let rec hook acc = function
    [] -> acc
  | [AttrDecl _] ->
       (* Detached attributes are erased. TODO: Warning. *)
       acc
  | AttrDecl _ :: (AttrDecl _ :: _ as declarations) ->
      (* Detached attributes are erased. TODO: Warning. *)
      hook acc declarations
  | AttrDecl decl :: ConstDecl {value; region} :: declarations ->
      let new_const =
        ConstDecl {value = {value with attributes = Some decl}; region}
      in hook acc (new_const :: declarations)
  | AttrDecl decl :: FunDecl {value; region} :: declarations ->
      let new_fun =
        FunDecl {value = {value with attributes = Some decl}; region}
      in hook acc (new_fun :: declarations)
  | AttrDecl _ :: declarations ->
      (* Detached attributes are erased. TODO: Warning. *)
      hook acc declarations
  | TypeDecl decl :: declarations ->
      let decl, loc = r_split decl in
      let {name; type_expr} : Raw.type_decl = decl in
      let%bind type_expression = compile_type_expression type_expr in
      let new_decl =
        Declaration_type (Var.of_name name.value, type_expression) in
      let res = Location.wrap ~loc new_decl in
      hook (bind_list_cons res acc) declarations
  | ConstDecl decl :: declarations ->
      let compile_const_decl =
        fun {name;const_type; init; attributes} ->
          let%bind expression = compile_expression init in
          let%bind t = compile_type_expression const_type in
          let type_annotation = Some t in
          let inline =
            match attributes with
              None -> false
            | Some {value; _} ->
                npseq_to_list value.ne_elements
               |> List.exists (fun Region.{value; _} -> value = "\"inline\"") in
          let new_decl =
            Declaration_constant
              (Var.of_name name.value, type_annotation, inline, expression)
          in ok new_decl in
      let%bind res =
        bind_map_location compile_const_decl (Location.lift_region decl)
      in hook (bind_list_cons res acc) declarations
  | FunDecl fun_decl :: declarations ->
      let decl, loc = r_split fun_decl in
      let%bind ((name, ty_opt), expr) = compile_fun_decl ~loc decl in
      let inline =
        match fun_decl.value.attributes with
          None -> false
        | Some {value; _} ->
            npseq_to_list value.ne_elements
            |> List.exists (fun Region.{value; _} -> value = "\"inline\"") in
      let new_decl =
        Declaration_constant (name, ty_opt, inline, expr) in
      let res = Location.wrap ~loc new_decl in
      hook (bind_list_cons res acc) declarations
  in hook (ok @@ []) (List.rev declarations)

let compile_program : Raw.ast -> (program , (abs_error)) result =
  fun t ->
  let declarations = nseq_to_list t.decl in
  trace (program_tracer declarations) @@
    compile_declaration_list declarations
