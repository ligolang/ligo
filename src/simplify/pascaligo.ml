open Trace
open Ast_simplified

module Raw = Parser.Pascaligo.AST
module SMap = Map.String

open Combinators

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let npseq_to_nelist (hd, tl) = hd, (List.map snd tl)
let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

module Errors = struct
  let unsupported_ass_None region =
    let title () = "assignment of None" in
    let message () =
      Format.asprintf "assignments of None are not supported yet" in
    let data = [
      ("none_expr",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ region)
    ] in
    error ~data title message

  let unsupported_entry_decl decl =
    let title () = "entry point declarations" in
    let message () =
      Format.asprintf "entry points within the contract \
                       are not supported yet" in
    let data = [
      ("declaration",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ decl.Region.region)
    ] in
    error ~data title message

  let unsupported_proc_decl decl =
    let title () = "procedure declarations" in
    let message () =
      Format.asprintf "procedures are not supported yet" in
    let data = [
      ("declaration",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ decl.Region.region)
    ] in
    error ~data title message

  let unsupported_local_proc region =
    let title () = "local procedure declarations" in
    let message () =
      Format.asprintf "local procedures are not supported yet" in
    let data = [
      ("declaration",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ region)
    ] in
    error ~data title message

  let corner_case ~loc message =
    let title () = "corner case" in
    let content () = "We don't have a good error message for this case. \
                      We are striving find ways to better report them and \
                      find the use-cases that generate them. \
                      Please report this to the developers." in
    let data = [
      ("location" , fun () -> loc) ;
      ("message" , fun () -> message) ;
    ] in
    error ~data title content

  let unknown_predefined_type name =
    let title () = "type constants" in
    let message () =
      Format.asprintf "unknown predefined type \"%s\"" name.Region.value in
    let data = [
      ("typename_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ name.Region.region)
    ] in
    error ~data title message

  let unsupported_arith_op expr =
    let title () = "arithmetic expressions" in
    let message () =
      Format.asprintf "this arithmetic operator is not supported yet" in
    let expr_loc = Raw.expr_to_region expr in
    let data = [
      ("expr_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ expr_loc)
    ] in
    error ~data title message

  let unsupported_string_catenation expr =
    let title () = "string expressions" in
    let message () =
      Format.asprintf "string concatenation is not supported yet" in
    let expr_loc = Raw.expr_to_region expr in
    let data = [
      ("expr_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ expr_loc)
    ] in
    error ~data title message

  let unsupported_set_expr expr =
    let title () = "set expressions" in
    let message () =
      Format.asprintf "the set type is not supported yet" in
     let expr_loc = Raw.expr_to_region expr in
    let data = [
      ("expr_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ expr_loc)
    ] in
    error ~data title message

  let unsupported_proc_calls call =
    let title () = "procedure calls" in
    let message () =
      Format.asprintf "procedure calls are not supported yet" in
    let data = [
      ("call_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ call.Region.region)
    ] in
    error ~data title message

  let unsupported_for_loops region =
    let title () = "bounded iterators" in
    let message () =
      Format.asprintf "for loops are not supported yet" in
    let data = [
      ("loop_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ region)
    ] in
    error ~data title message

  let unsupported_deep_map_assign v =
    let title () = "map assignments" in
    let message () =
      Format.asprintf "assignments to embedded maps are not \
                       supported yet" in
    let data = [
      ("lhs_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ v.Region.region)
    ] in
    error ~data title message

  let unsupported_empty_record_patch record_expr =
    let title () = "empty record patch" in
    let message () =
      Format.asprintf "empty record patches are not supported yet" in
    let data = [
      ("record_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ record_expr.Region.region)
    ] in
    error ~data title message

  let unsupported_map_patches patch =
    let title () = "map patches" in
    let message () =
      Format.asprintf "map patches (a.k.a. functional updates) are \
                       not supported yet" in
    let data = [
      ("patch_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ patch.Region.region)
    ] in
    error ~data title message

  let unsupported_set_patches patch =
    let title () = "set patches" in
    let message () =
      Format.asprintf "set patches (a.k.a. functional updates) are \
                       not supported yet" in
    let data = [
      ("patch_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ patch.Region.region)
    ] in
    error ~data title message

  let unsupported_deep_map_rm path =
    let title () = "binding removals" in
    let message () =
      Format.asprintf "removal of bindings from embedded maps \
                       are not supported yet" in
    let data = [
      ("path_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ path.Region.region)
    ] in
    error ~data title message

  let unsupported_set_removal remove =
    let title () = "set removals" in
    let message () =
      Format.asprintf "removal of elements in a set is not \
                       supported yet" in
    let data = [
      ("removal_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ remove.Region.region)
    ] in
    error ~data title message

  let unsupported_non_var_pattern p =
    let title () = "pattern is not a variable" in
    let message () =
      Format.asprintf "non-variable patterns in constructors \
                       are not supported yet" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("pattern_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let only_constructors p =
    let title () = "constructors in patterns" in
    let message () =
      Format.asprintf "currently, only constructors are supported in patterns" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("pattern_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_tuple_pattern p =
    let title () = "tuple pattern" in
    let message () =
      Format.asprintf "tuple patterns are not supported yet" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("pattern_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_deep_Some_patterns pattern =
    let title () = "option patterns" in
    let message () =
      Format.asprintf "currently, only variables in Some constructors \
                       in patterns are supported" in
    let pattern_loc = Raw.pattern_to_region pattern in
    let data = [
      ("pattern_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_deep_list_patterns cons =
    let title () = "lists in patterns" in
    let message () =
      Format.asprintf "currently, only empty lists and x::y \
                       are supported in patterns" in
    let data = [
      ("pattern_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ cons.Region.region)
    ] in
    error ~data title message

  let unsupported_sub_blocks b =
    let title () = "block instructions" in
    let message () =
      Format.asprintf "Sub-blocks are not supported yet" in
    let data = [
      ("block_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ b.Region.region)
    ] in
    error ~data title message

  (* Logging *)

  let simplifying_instruction t =
    let title () = "simplifiying instruction" in
    let message () = "" in
    let data = [
      ("instruction",
       fun () -> Format.asprintf "%a" PP_helpers.(printer Parser.Pascaligo.ParserLog.print_instruction) t)
    ] in
    error ~data title message
end

open Errors
open Operators.Simplify.Pascaligo

let r_split = Location.r_split

let return expr = ok @@ fun expr'_opt ->
  let expr = expr in
  match expr'_opt with
  | None -> ok @@ expr
  | Some expr' -> ok @@ e_sequence expr expr'

let return_let_in ?loc binder rhs = ok @@ fun expr'_opt ->
  match expr'_opt with
  | None -> fail @@ corner_case ~loc:__LOC__ "missing return"
  | Some expr' -> ok @@ e_let_in ?loc binder rhs expr'

let rec simpl_type_expression (t:Raw.type_expr) : type_expression result =
  match t with
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
      let%bind lst' = bind_list @@ List.map simpl_type_expression lst in
      let%bind cst =
        trace_option (unknown_predefined_type name) @@
        List.assoc_opt name.value type_constants in
      ok @@ T_constant (cst , lst')
  | TProd p ->
      let%bind tpl = simpl_list_type_expression
        @@ npseq_to_list p.value in
      ok tpl
  | TRecord r ->
      let aux = fun (x, y) -> let%bind y = simpl_type_expression y in ok (x, y) in
      let apply =
        fun (x:Raw.field_decl Raw.reg) -> (x.value.field_name.value, x.value.field_type) in
      let%bind lst = bind_list
        @@ List.map aux
        @@ List.map apply
        @@ pseq_to_list r.value.elements in
      let m = List.fold_left (fun m (x, y) -> SMap.add x y m) SMap.empty lst in
      ok @@ T_record m
  | TSum s ->
      let aux (v:Raw.variant Raw.reg) =
        let args =
          match v.value.args with
            None -> []
          | Some (_, product) ->
              npseq_to_list product.value in
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
  | [] -> ok @@ t_unit
  | [hd] -> simpl_type_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_type_expression lst in
      ok @@ T_tuple lst

let rec simpl_expression (t:Raw.expr) : expr result =
  let return x = ok x in
  let simpl_projection = fun (p : Raw.projection Region.reg) ->
    let (p' , loc) = r_split p in
    let var =
      let name = p'.struct_name.value in
      e_variable name in
    let path = p'.field_path in
    let path' =
      let aux (s:Raw.selection) =
        match s with
        | FieldName property -> Access_record property.value
        | Component index -> Access_tuple (Z.to_int (snd index.value))
      in
      List.map aux @@ npseq_to_list path in
    return @@ e_accessor ~loc var path'
  in
  match t with
  | EAnnot a -> (
      let ((expr , type_expr) , loc) = r_split a in
      let%bind expr' = simpl_expression expr in
      let%bind type_expr' = simpl_type_expression type_expr in
      return @@ e_annotation ~loc expr' type_expr'
    )
  | EVar c -> (
      let (c' , loc) = r_split c in
      match List.assoc_opt c' constants with
      | None -> return @@ e_variable ~loc c.value
      | Some s -> return @@ e_constant ~loc s []
    )
  | ECall x -> (
      let ((name, args) , loc) = r_split x in
      let (f , f_loc) = r_split name in
      let (args , args_loc) = r_split args in
      let args' = npseq_to_list args.inside in
      match List.assoc_opt f constants with
      | None ->
          let%bind arg = simpl_tuple_expression ~loc:args_loc args' in
          return @@ e_application ~loc (e_variable ~loc:f_loc f) arg
      | Some s ->
          let%bind lst = bind_map_list simpl_expression args' in
          return @@ e_constant ~loc s lst
    )
  | EPar x -> simpl_expression x.value.inside
  | EUnit reg ->
    let loc = Location.lift reg in
    return @@ e_literal ~loc Literal_unit
  | EBytes x ->
    let (x' , loc) = r_split x in
    return @@ e_literal ~loc (Literal_bytes (Bytes.of_string @@ fst x'))
  | ETuple tpl ->
      let (Raw.TupleInj tpl') = tpl in
      let (tpl' , loc) = r_split tpl' in
      simpl_tuple_expression ~loc @@ npseq_to_list tpl'.inside
  | ERecord r ->
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = simpl_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
        @@ pseq_to_list r.value.elements in
      let aux prev (k, v) = SMap.add k v prev in
      return @@ e_record (List.fold_left aux SMap.empty fields)
  | EProj p -> simpl_projection p
  | EConstr (ConstrApp c) -> (
      let ((c, args) , loc) = r_split c in
      let (args , args_loc) = r_split args in
      let%bind arg =
        simpl_tuple_expression ~loc:args_loc
        @@ npseq_to_list args.inside in
      return @@ e_constructor ~loc c.value arg
    )
  | EConstr (SomeApp a) ->
      let ((_, args) , loc) = r_split a in
      let (args , args_loc) = r_split args in
      let%bind arg =
        simpl_tuple_expression ~loc:args_loc
        @@ npseq_to_list args.inside in
      return @@ e_constant ~loc "SOME" [arg]
  | EConstr (NoneExpr reg) -> (
      let loc = Location.lift reg in
      return @@ e_none ~loc ()
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
      let n = Z.to_int @@ snd n in
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
  | EArith _ as e ->
       fail @@ unsupported_arith_op e
  | EString (String s) ->
    let (s , loc) = r_split s in
      let s' =
        (* S contains quotes *)
        String.(sub s 1 (length s - 2))
      in
      return @@ e_literal ~loc (Literal_string s')
  | EString (Cat _) as e ->
      fail @@ unsupported_string_catenation e
  | ELogic l -> simpl_logic_expression l
  | EList l -> simpl_list_expression l
  | ESet _ -> fail @@ unsupported_set_expr t
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
  | EMap (MapInj mi) -> (
      let (mi , loc) = r_split mi in
      let%bind lst =
        let lst = List.map get_value @@ pseq_to_list mi.elements in
        let aux : Raw.binding -> (expression * expression) result =
          fun b ->
            let%bind src = simpl_expression b.source in
            let%bind dst = simpl_expression b.image in
            ok (src, dst) in
        bind_map_list aux lst in
      return @@ e_map ~loc lst
    )
  | EMap (MapLookUp lu) -> (
      let (lu , loc) = r_split lu in
      let%bind path = match lu.path with
        | Name v -> (
            let (v , loc) = r_split v in
            return @@ e_variable ~loc v
          )
        | Path p -> simpl_projection p
      in
      let%bind index = simpl_expression lu.index.value.inside in
      return @@ e_look_up ~loc path index
    )

and simpl_logic_expression (t:Raw.logic_expr) : expression result =
  let return x = ok x in
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
  let return x = ok x in
  match t with
  | Cons c ->
      simpl_binop "CONS" c
  | List lst -> (
      let (lst , loc) = r_split lst in
      let%bind lst' =
        bind_map_list simpl_expression @@
        pseq_to_list lst.elements in
      return @@ e_list ~loc lst'
    )
  | Nil reg -> (
      let loc = Location.lift reg in
      return @@ e_list ~loc []
    )

and simpl_binop (name:string) (t:_ Raw.bin_op Region.reg) : expression result =
  let return x = ok x in
  let (t , loc) = r_split t in
  let%bind a = simpl_expression t.arg1 in
  let%bind b = simpl_expression t.arg2 in
  return @@ e_constant ~loc name [ a ; b ]

and simpl_unop (name:string) (t:_ Raw.un_op Region.reg) : expression result =
  let return x = ok x in
  let (t , loc) = r_split t in
  let%bind a = simpl_expression t.arg in
  return @@ e_constant ~loc name [ a ]

and simpl_tuple_expression ?loc (lst:Raw.expr list) : expression result =
  let return x = ok x in
  match lst with
  | [] -> return @@ e_literal Literal_unit
  | [hd] -> simpl_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_expression lst in
      return @@ e_tuple ?loc lst

and simpl_local_declaration : Raw.local_decl -> _ result = fun t ->
  match t with
  | LocalData d ->
      simpl_data_declaration d
  | LocalFun f  ->
      let (f , loc) = r_split f in
      let%bind (name , e) = simpl_fun_declaration ~loc f in
      return_let_in ~loc name e
  | LocalProc d ->
      fail @@ unsupported_local_proc d.Region.region
and simpl_data_declaration : Raw.data_decl -> _ result = fun t ->
  match t with
  | LocalVar x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.var_type in
      let%bind expression = simpl_expression x.init in
      return_let_in ~loc (name , Some t) expression
  | LocalConst x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.const_type in
      let%bind expression = simpl_expression x.init in
      return_let_in ~loc (name , Some t) expression

and simpl_param : Raw.param_decl -> (type_name * type_expression) result =
  fun t ->
  match t with
  | ParamConst c ->
      let c = c.value in
      let type_name = c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok (type_name , type_expression)
  | ParamVar v ->
      let c = v.value in
      let type_name = c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok (type_name , type_expression)

and simpl_fun_declaration :
  loc:_ -> Raw.fun_decl -> ((name * type_expression option) * expression) result =
  fun ~loc x ->
  let open! Raw in
  let {name;param;ret_type;local_decls;block;return} : fun_decl = x in
  (match npseq_to_list param.value.inside with
   | [] ->
       fail @@
       corner_case ~loc:__LOC__ "parameter-less function should not exist"
   | [a] -> (
       let%bind input = simpl_param a in
       let name = name.value in
       let (binder , input_type) = input in
       let%bind local_declarations =
         bind_map_list simpl_local_declaration local_decls in
       let%bind instructions = bind_list
         @@ List.map simpl_statement
         @@ npseq_to_list block.value.statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = local_declarations @ instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression : expression = e_lambda ~loc binder (Some input_type)
           (Some output_type) result in
       let type_annotation = Some (T_function (input_type, output_type)) in
       ok ((name , type_annotation) , expression)
     )
   | lst -> (
       let arguments_name = "arguments" in
       let%bind params = bind_map_list simpl_param lst in
       let (binder , input_type) =
         let type_expression = T_tuple (List.map snd params) in
         (arguments_name , type_expression) in
       let%bind tpl_declarations =
         let aux = fun i x ->
           let expr = e_accessor (e_variable arguments_name) [Access_tuple i] in
           let type_ = Some (snd x) in
           let ass = return_let_in (fst x , type_) expr in
           ass
         in
         bind_list @@ List.mapi aux params in
       let%bind local_declarations =
         bind_map_list simpl_local_declaration local_decls in
       let%bind instructions = bind_list
         @@ List.map simpl_statement
         @@ npseq_to_list block.value.statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = tpl_declarations @ local_declarations @ instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression =
         e_lambda ~loc binder (Some input_type) (Some output_type) result in
       let type_annotation = Some (T_function (input_type, output_type)) in
       ok ((name.value , type_annotation) , expression)
     )
  )
and simpl_declaration : Raw.declaration -> declaration Location.wrap result =
  fun t ->
  let open! Raw in
  match t with
  | TypeDecl x -> (
      let (x , loc) = r_split x in
      let {name;type_expr} : Raw.type_decl = x in
      let%bind type_expression = simpl_type_expression type_expr in
      ok @@ Location.wrap ~loc (Declaration_type (name.value , type_expression))
    )
  | ConstDecl x ->
      let simpl_const_decl = fun {name;const_type;init} ->
        let%bind expression = simpl_expression init in
        let%bind t = simpl_type_expression const_type in
        let type_annotation = Some t in
        ok @@ Declaration_constant (name.value , type_annotation , expression)
      in
      bind_map_location simpl_const_decl (Location.lift_region x)
  | LambdaDecl (FunDecl x) -> (
      let (x , loc) = r_split x in
      let%bind ((name , ty_opt) , expr) = simpl_fun_declaration ~loc x in
      ok @@ Location.wrap ~loc (Declaration_constant (name , ty_opt , expr))
    )
  | LambdaDecl (ProcDecl decl) ->
      fail @@ unsupported_proc_decl decl
  | LambdaDecl (EntryDecl decl) ->
      fail @@ unsupported_entry_decl decl

and simpl_statement : Raw.statement -> (_ -> expression result) result =
  fun s ->
  match s with
  | Instr i -> simpl_instruction i
  | Data d -> simpl_data_declaration d

and simpl_single_instruction : Raw.single_instr -> (_ -> expression result) result =
  fun t ->
  match t with
  | ProcCall call ->
      fail @@ unsupported_proc_calls call
  | Fail e -> (
      let%bind expr = simpl_expression e.value.fail_expr in
      return @@ e_failwith expr
    )
  | Skip reg -> (
      let loc = Location.lift reg in
      return @@ e_skip ~loc ()
    )
  | Loop (While l) ->
      let l = l.value in
      let%bind cond = simpl_expression l.cond in
      let%bind body = simpl_block l.block.value in
      let%bind body = body None in
      return @@ e_loop cond body
  | Loop (For (ForInt {region; _} | ForCollect {region; _})) ->
      fail @@ unsupported_for_loops region
  | Cond c -> (
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.test in
      let%bind match_true = match c.ifso with
        | ClauseInstr i -> simpl_instruction_block i
        | ClauseBlock b -> simpl_statements @@ fst b.value.inside in
      let%bind match_false = match c.ifnot with
        | ClauseInstr i -> simpl_instruction_block i
        | ClauseBlock b -> simpl_statements @@ fst b.value.inside in
      let%bind match_true = match_true None in
      let%bind match_false = match_false None in
      return @@ e_matching expr ~loc (Match_bool {match_true; match_false})
    )
  | Assign a -> (
      let (a , loc) = r_split a in
      let%bind value_expr = match a.rhs with
        | Expr e -> simpl_expression e
        | NoneExpr reg -> fail @@ unsupported_ass_None reg
      in
      match a.lhs with
        | Path path -> (
            let (name , path') = simpl_path path in
            return @@ e_assign ~loc name path' value_expr
          )
        | MapPath v -> (
            let v' = v.value in
            let%bind name = match v'.path with
              | Name name -> ok name
              | _ -> fail @@ unsupported_deep_map_assign v in
            let%bind key_expr = simpl_expression v'.index.value.inside in
            let old_expr = e_variable name.value in
            let expr' = e_map_add key_expr value_expr old_expr in
            return @@ e_assign ~loc name.value [] expr'
          )
    )
  | CaseInstr c -> (
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.expr in
      let%bind cases =
        let aux (x : Raw.instruction Raw.case_clause Raw.reg) =
          let%bind i = simpl_instruction_block x.value.rhs in
          let%bind i = i None in
          ok (x.value.pattern, i) in
        bind_list
        @@ List.map aux
        @@ npseq_to_list c.cases.value in
      let%bind m = simpl_cases cases in
      return @@ e_matching ~loc expr m
    )
  | RecordPatch r -> (
      let r = r.value in
      let (name , access_path) = simpl_path r.path in
      let%bind inj = bind_list
        @@ List.map (fun (x:Raw.field_assign Region.reg) ->
            let (x , loc) = r_split x in
            let%bind e = simpl_expression x.field_expr
            in ok (x.field_name.value, e , loc)
          )
        @@ pseq_to_list r.record_inj.value.elements in
      let%bind expr =
        let aux = fun (access , v , loc) ->
          e_assign ~loc name (access_path @ [ Access_record access ]) v in
        let assigns = List.map aux inj in
        match assigns with
               (* E_sequence (E_skip, E_skip) ? *)
        | [] -> fail @@ unsupported_empty_record_patch r.record_inj
        | hd :: tl -> (
            let aux acc cur = e_sequence acc cur in
            ok @@ List.fold_left aux hd tl
          )
      in
      return @@ expr
    )
  | MapPatch patch ->
      fail @@ unsupported_map_patches patch
  | SetPatch patch ->
      fail @@ unsupported_set_patches patch
  | MapRemove r -> (
      let (v , loc) = r_split r in
      let key = v.key in
      let%bind map = match v.map with
        | Name v -> ok v.value
        | Path path -> fail @@ unsupported_deep_map_rm path in
      let%bind key' = simpl_expression key in
      let expr = e_constant ~loc "MAP_REMOVE" [key' ; e_variable map] in
      return @@ e_assign ~loc map [] expr
    )
  | SetRemove r -> fail @@ unsupported_set_removal r

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
  let get_var (t:Raw.pattern) =
    match t with
    | PVar v -> ok v.value
    | p -> fail @@ unsupported_non_var_pattern p
  in
  let get_tuple (t:Raw.pattern) = match t with
    | PCons v -> npseq_to_list v.value
    | PTuple v -> npseq_to_list v.value.inside
    | x -> [ x ]
  in
  let get_single (t:Raw.pattern) =
    let t' = get_tuple t in
    let%bind () =
      trace_strong (unsupported_tuple_pattern t) @@
      Assert.assert_list_size t' 1 in
    ok (List.hd t') in
  let get_constr (t:Raw.pattern) = match t with
    | PConstr v ->
        let%bind var = get_single (snd v.value).value >>? get_var in
        ok ((fst v.value).value , var)
    | _ -> fail @@ only_constructors t
  in
  let%bind patterns =
    let aux (x , y) =
      let xs = get_tuple x in
      trace_strong (unsupported_tuple_pattern x) @@
      Assert.assert_list_size xs 1 >>? fun () ->
      ok (List.hd xs , y)
    in
    bind_map_list aux t in
  match patterns with
  | [(PFalse _ , f) ; (PTrue _ , t)]
  | [(PTrue _ , t) ; (PFalse _ , f)] ->
      ok @@ Match_bool {match_true = t ; match_false = f}
  | [(PSome v , some) ; (PNone _ , none)]
  | [(PNone _ , none) ; (PSome v , some)] -> (
      let (_, v) = v.value in
      let%bind v = match v.value.inside with
        | PVar v -> ok v.value
        | p -> fail @@ unsupported_deep_Some_patterns p in
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
        | _ -> fail @@ unsupported_deep_list_patterns c
      in
      ok @@ Match_list {match_cons = (a, b, cons) ; match_nil = nil}
  | lst ->
      trace (simple_info "currently, only booleans, options, lists and \
                         user-defined constructors are supported in patterns") @@
      let%bind constrs =
        let aux (x , y) =
          let error =
            let title () = "Pattern" in
            let content () =
              Format.asprintf "Pattern : %a" (PP_helpers.printer Parser.Pascaligo.ParserLog.print_pattern) x in
            error title content in
          let%bind x' =
            trace error @@
            get_constr x in
          ok (x' , y) in
        bind_map_list aux lst in
      ok @@ Match_variant constrs

and simpl_instruction_block : Raw.instruction -> (_ -> expression result) result =
  fun t ->
  match t with
  | Single s -> simpl_single_instruction s
  | Block b -> simpl_block b.value

and simpl_instruction : Raw.instruction -> (_ -> expression result) result =
  fun t ->
  trace (simplifying_instruction t) @@
  match t with
  | Single s -> simpl_single_instruction s
  | Block b -> fail @@ unsupported_sub_blocks b

and simpl_statements : Raw.statements -> (_ -> expression result) result =
  fun ss ->
  let lst = npseq_to_list ss in
  let%bind fs = bind_map_list simpl_statement lst in
  let aux : _ -> (expression option -> expression result) -> _ =
    fun prec cur ->
      let%bind res = cur prec in
      ok @@ Some res in
  ok @@ fun (expr' : _ option) ->
  let%bind ret = bind_fold_right_list aux expr' fs in
  ok @@ Option.unopt_exn ret

and simpl_block : Raw.block -> (_ -> expression result) result = fun t ->
  simpl_statements t.statements

let simpl_program : Raw.ast -> program result = fun t ->
  bind_list @@ List.map simpl_declaration @@ nseq_to_list t.decl
