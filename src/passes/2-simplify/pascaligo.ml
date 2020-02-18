open Trace
open Ast_simplified

module Raw = Parser.Pascaligo.AST
module SMap = Map.String
module ParserLog = Parser_pascaligo.ParserLog

open Combinators

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let pseq_to_list = function
  None -> []
| Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

and repair_mutable_variable (for_body : expression) (element_names : expression_variable list) (env : expression_variable) =
  let%bind captured_names = Self_ast_simplified.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : expression_variable list * expression_variable list) (ass_exp : expression) ->
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false;rhs;let_result} ->
          let (name,_) = let_binder in
          ok (true,(name::decl_var, free_var),e_let_in let_binder false false rhs let_result)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let (name,_) = let_binder in
          if List.mem name decl_var then 
            ok (true,(decl_var, free_var), e_let_in let_binder false false rhs let_result)
          else(
            let free_var = if (List.mem name free_var) then free_var else name::free_var in
            let expr = e_let_in (env,None) false false (e_update (e_variable env) (Var.show name) (e_variable name)) let_result in
            ok (true,(decl_var, free_var), e_let_in let_binder false  false rhs expr)
          )
        | E_variable name ->
          if List.mem name decl_var || List.mem name free_var || Var.equal name env then 
            ok (true,(decl_var, free_var), e_variable name)
          else
            ok (true, (decl_var, name::free_var), e_variable name)
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _} 
        | E_matching _ -> ok @@ (false, (decl_var,free_var),ass_exp)
      | _ -> ok (true, (decl_var, free_var),ass_exp)
    )
      (element_names,[])
      for_body in
  ok @@ captured_names

and repair_mutable_variable_for_collect (for_body : expression) (element_names : expression_variable list) (env : expression_variable) =
  let%bind captured_names = Self_ast_simplified.fold_map_expression
    (* TODO : these should use Variables sets *)
    (fun (decl_var,free_var : expression_variable list * expression_variable list) (ass_exp : expression) ->
      match ass_exp.expression_content with
        | E_let_in {let_binder;mut=false;rhs;let_result} ->
          let (name,_) = let_binder in
          ok (true,(name::decl_var, free_var),e_let_in let_binder false false rhs let_result)
        | E_let_in {let_binder;mut=true; rhs;let_result} ->
          let (name,_) = let_binder in
          if List.mem name decl_var then 
            ok (true,(decl_var, free_var), e_let_in let_binder false false rhs let_result)
          else(
            let free_var = if (List.mem name free_var) then free_var else name::free_var in
            let expr = e_let_in (env,None) false false (
              e_update (e_variable env) ("0") 
              (e_update (e_accessor (e_variable env) "0") (Var.show name) (e_variable name))
              )
              let_result in
            ok (true,(decl_var, free_var), e_let_in let_binder false  false rhs expr)
          )
        | E_variable name ->
          if List.mem name decl_var || List.mem name free_var || Var.equal name env then 
            ok (true,(decl_var, free_var), e_variable name)
          else
            ok (true,(decl_var, name::free_var), e_variable name)
        | E_constant {cons_name=C_MAP_FOLD;arguments= _}
        | E_constant {cons_name=C_SET_FOLD;arguments= _}
        | E_constant {cons_name=C_LIST_FOLD;arguments= _} 
        | E_matching _ -> ok @@ (false,(decl_var,free_var),ass_exp)
      | _ -> ok (true,(decl_var, free_var),ass_exp)
    )
      (element_names,[])
      for_body in
  ok @@ captured_names

and store_mutable_variable (free_vars : expression_variable list) =
  if (List.length free_vars == 0) then
    e_unit ()
  else
    let aux var = (Var.show var, e_variable var) in
    e_record_ez (List.map aux free_vars)
 
and restore_mutable_variable (expr : expression) (free_vars : expression_variable list) (env :expression_variable) =
  let aux (f:expression -> expression) (ev:expression_variable) =
    ok @@ fun expr -> f (e_let_in (ev,None) true false (e_accessor (e_variable env) (Var.show ev)) expr)
  in
  let%bind ef = bind_fold_list aux (fun e -> e) free_vars in
  ok @@ fun expr'_opt -> match expr'_opt with
  | None -> ok @@ e_let_in (env,None) false false expr (ef (e_skip ()))
  | Some expr' -> ok @@ e_let_in (env,None) false false expr (ef expr')
 


module Errors = struct
  let unsupported_cst_constr p =
    let title () = "" in
    let message () =
      Format.asprintf "\nConstant constructors are not supported yet.\n" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unknown_predefined_type name =
    let title () = "\nType constants" in
    let message () =
      Format.asprintf "Unknown predefined type \"%s\".\n" name.Region.value in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ name.Region.region)
    ] in
    error ~data title message

  let unsupported_non_var_pattern p =
    let title () = "" in
    let message () =
      Format.asprintf "\nNon-variable patterns in constructors \
                       are not supported yet.\n" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let only_constructors p =
    let title () = "" in
    let message () =
      Format.asprintf "\nCurrently, only constructors \
                       are supported in patterns.\n" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_tuple_pattern p =
    let title () = "" in
    let message () =
      Format.asprintf "\nTuple patterns are not supported yet.\n" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc) ;
      (** TODO: The labelled arguments should be flowing from the CLI. *)
      ("pattern",
       fun () -> ParserLog.pattern_to_string
                ~offsets:true ~mode:`Point p)
    ] in
    error ~data title message

  let unsupported_deep_Some_patterns pattern =
    let title () = "" in
    let message () =
      Format.asprintf "\nCurrently, only variables in constructors \
                       \"Some\" in patterns are supported.\n" in
    let pattern_loc = Raw.pattern_to_region pattern in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_deep_list_patterns cons =
    let title () = "" in
    let message () =
      Format.asprintf "\nCurrently, only empty lists and x::y \
                       are supported in patterns.\n" in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ cons.Region.region)
    ] in
    error ~data title message

  (* Logging *)

  let simplifying_instruction t =
    let title () = "\nSimplifiying instruction" in
    let message () = "" in
    (** TODO: The labelled arguments should be flowing from the CLI. *)
    let data = [
      ("instruction",
       fun () -> ParserLog.instruction_to_string
                ~offsets:true ~mode:`Point t)
    ] in
    error ~data title message
end

open Errors
open Operators.Simplify.Pascaligo

let r_split = Location.r_split

(* Statements can't be simplified in isolation. [a ; b ; c] can get
   simplified either as [let x = expr in (b ; c)] if [a] is a [const x
   = expr] declaration or as [sequence(a, sequence(b, c))] for
   everything else.  Because of this, simplifying sequences depend on
   their contents. To avoid peeking in their contents, we instead
   simplify sequences elements as functions from their next elements
   to the actual result.

   For [return_let_in], if there is no follow-up element, an error is
   triggered, as you can't have [let x = expr in ...] with no [...]. A
   cleaner option might be to add a [unit] instead of failing.

   [return_statement] is used for non-let-in statements.
 *)

let return_let_in ?loc binder mut inline rhs = ok @@ fun expr'_opt ->
  match expr'_opt with
  | None -> ok @@ e_let_in ?loc binder mut inline rhs (e_skip ())
  | Some expr' -> ok @@ e_let_in ?loc binder mut inline rhs expr'

let return_statement expr = ok @@ fun expr'_opt ->
  match expr'_opt with
  | None -> ok @@ expr
  | Some expr' -> ok @@ e_sequence expr expr'


let rec simpl_type_expression (t:Raw.type_expr) : type_expression result =
  match t with
    TPar x -> simpl_type_expression x.value.inside
  | TVar v -> (
      match type_constants v.value with
      | Ok (s,_) -> ok @@ make_t @@ T_constant s
      | Error _ -> ok @@ make_t @@ T_variable (Var.of_name v.value)
    )
  | TFun x -> (
      let%bind (a , b) =
        let (a , _ , b) = x.value in
        bind_map_pair simpl_type_expression (a , b) in
      ok @@ make_t @@ T_arrow {type1=a;type2=b}
    )
  | TApp x ->
      let (name, tuple) = x.value in
      let lst = npseq_to_list tuple.value.inside in
      let%bind lst =
        bind_list @@ List.map simpl_type_expression lst in (** TODO: fix constant and operator*)
      let%bind cst =
        trace (unknown_predefined_type name) @@
        type_operators name.value in
      t_operator cst lst
  | TProd p ->
      let%bind tpl = simpl_list_type_expression
    @@ npseq_to_list p.value in
      ok tpl
  | TRecord r ->
      let aux = fun (x, y) ->
        let%bind y = simpl_type_expression y in
        ok (x, y)
      in
      let apply =
        fun (x:Raw.field_decl Raw.reg) -> (x.value.field_name.value, x.value.field_type) in
      let%bind lst = bind_list
        @@ List.map aux
        @@ List.map apply
        @@ npseq_to_list r.value.ne_elements in
      let m = List.fold_left (fun m (x, y) -> LMap.add (Label x) y m) LMap.empty lst in
      ok @@ make_t @@ T_record m
  | TSum s ->
      let aux (v:Raw.variant Raw.reg) =
        let args =
          match v.value.arg with
            None -> []
          | Some (_, TProd product) -> npseq_to_list product.value
          | Some (_, t_expr) -> [t_expr] in
        let%bind te = simpl_list_type_expression @@ args in
        ok (v.value.constr.value, te)
      in
      let%bind lst = bind_list
        @@ List.map aux
        @@ npseq_to_list s.value in
      let m = List.fold_left (fun m (x, y) -> CMap.add (Constructor x) y m) CMap.empty lst in
      ok @@ make_t @@ T_sum m

and simpl_list_type_expression (lst:Raw.type_expr list) : type_expression result =
  match lst with
  | [] -> ok @@ t_unit
  | [hd] -> simpl_type_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_type_expression lst in
      ok @@ t_tuple lst

let simpl_projection : Raw.projection Region.reg -> _ = fun p ->
  let (p' , loc) = r_split p in
  let var =
    let name = Var.of_name p'.struct_name.value in
    e_variable name in
  let path = p'.field_path in
  let path' =
    let aux (s:Raw.selection) =
      match s with
      | FieldName property -> property.value
      | Component index -> (Z.to_string (snd index.value))
    in
    List.map aux @@ npseq_to_list path in
  ok @@ List.fold_left (e_accessor ~loc) var path'


let rec simpl_expression (t:Raw.expr) : expr result =
  let return x = ok x in
  match t with
  | EAnnot a -> (
      let ((expr , type_expr) , loc) = r_split a in
      let%bind expr' = simpl_expression expr in
      let%bind type_expr' = simpl_type_expression type_expr in
      return @@ e_annotation ~loc expr' type_expr'
    )
  | EVar c -> (
      let (c' , loc) = r_split c in
      match constants c' with
      | Error _   -> return @@ e_variable ~loc (Var.of_name c.value)
      | Ok (s,_)  -> return @@ e_constant ~loc s []
    )
  | ECall x -> (
      let ((f, args) , loc) = r_split x in
      let (args , args_loc) = r_split args in
      let args' = npseq_to_list args.inside in
      match f with
      | EVar name -> (
        let (f_name , f_loc) = r_split name in
        match constants f_name with
        | Error _ ->
           let%bind arg = simpl_tuple_expression ~loc:args_loc args' in
           return @@ e_application ~loc (e_variable ~loc:f_loc (Var.of_name f_name)) arg
        | Ok (s,_) ->
           let%bind lst = bind_map_list simpl_expression args' in
           return @@ e_constant ~loc s lst
      )
      | f -> (
        let%bind f' = simpl_expression f in
        let%bind arg = simpl_tuple_expression ~loc:args_loc args' in
        return @@ e_application ~loc f' arg
      )
    )
  | EPar x -> simpl_expression x.value.inside
  | EUnit reg ->
    let loc = Location.lift reg in
    return @@ e_literal ~loc Literal_unit
  | EBytes x ->
    let (x' , loc) = r_split x in
    return @@ e_literal ~loc (Literal_bytes (Hex.to_bytes @@ snd x'))
  | ETuple tpl ->
      let (tpl' , loc) = r_split tpl in
      simpl_tuple_expression ~loc @@ npseq_to_list tpl'.inside
  | ERecord r ->
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = simpl_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
        @@ npseq_to_list r.value.ne_elements in
      let aux prev (k, v) = SMap.add k v prev in
      return @@ e_record (List.fold_left aux SMap.empty fields)
  | EProj p -> simpl_projection p
  | EUpdate u -> simpl_update u
  | EConstr (ConstrApp c) -> (
      let ((c, args) , loc) = r_split c in
      match args with
        None -> simpl_tuple_expression []
      | Some args ->
          let args, args_loc = r_split args in
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
      return @@ e_constant ~loc C_SOME [arg]
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
  | EArith (Mutez n) -> (
    let (n , loc) = r_split n in
    let n = Z.to_int @@ snd @@ n in
    return @@ e_literal ~loc (Literal_mutez n)
  )
  | EArith (Neg e) -> simpl_unop "NEG" e
  | EString (String s) ->
    let (s , loc) = r_split s in
      let s' =
        (* S contains quotes *)
        String.(sub s 1 (length s - 2))
      in
      return @@ e_literal ~loc (Literal_string s')
  | EString (Cat bo) ->
    let (bo , loc) = r_split bo in
    let%bind sl = simpl_expression bo.arg1 in
    let%bind sr = simpl_expression bo.arg2 in
    return @@ e_string_cat ~loc sl sr
  | ELogic l -> simpl_logic_expression l
  | EList l -> simpl_list_expression l
  | ESet s -> simpl_set_expression s
  | ECond c ->
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.test in
      let%bind match_true = simpl_expression c.ifso in
      let%bind match_false = simpl_expression c.ifnot in
      let match_expr = e_matching expr ~loc (Match_bool {match_true; match_false}) in
      let env = Var.fresh () in
      let%bind (_, match_expr) = repair_mutable_variable match_expr [] env in
      return @@ match_expr

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
      let match_expr = e_matching ~loc e cases in
      let env = Var.fresh () in
      let%bind (_, match_expr) = repair_mutable_variable match_expr [] env in
      return @@ match_expr
    )
  | EMap (MapInj mi)  -> (
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
  | EMap (BigMapInj mi) -> (
      let (mi , loc) = r_split mi in
      let%bind lst =
        let lst = List.map get_value @@ pseq_to_list mi.elements in
        let aux : Raw.binding -> (expression * expression) result =
          fun b ->
            let%bind src = simpl_expression b.source in
            let%bind dst = simpl_expression b.image in
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
        | Path p -> simpl_projection p
      in
      let%bind index = simpl_expression lu.index.value.inside in
      return @@ e_look_up ~loc path index
    )
  | EFun f ->
    let (f , loc) = r_split f in
    let%bind (_ty_opt, f') = simpl_fun_expression ~loc f
    in return @@ f'


and simpl_update = fun (u:Raw.update Region.reg) ->
  let (u, loc) = r_split u in
  let (name, path) = simpl_path u.record in
  let record = match path with
  | [] -> e_variable (Var.of_name name)
  | _ -> e_accessor_list (e_variable (Var.of_name name)) path in 
  let updates = u.updates.value.ne_elements in
  let%bind updates' =
    let aux (f:Raw.field_path_assign Raw.reg) =
      let (f,_) = r_split f in
      let%bind expr = simpl_expression f.field_expr in
        ok ( List.map (fun (x: _ Raw.reg) -> x.value) (npseq_to_list f.field_path), expr)
    in
    bind_map_list aux @@ npseq_to_list updates
  in
  let aux ur (path, expr) = 
    let rec aux record = function
      | [] -> failwith "error in parsing"
      | hd :: [] -> ok @@ e_update ~loc record hd expr
      | hd :: tl -> 
        let%bind expr = (aux (e_accessor ~loc record hd) tl) in
        ok @@ e_update ~loc record hd expr 
    in
    aux ur path in
  bind_fold_list aux record updates'

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
    ECons c ->
      simpl_binop "CONS" c
  | EListComp lst ->
      let (lst , loc) = r_split lst in
      let%bind lst' =
        bind_map_list simpl_expression @@
        pseq_to_list lst.elements in
      return @@ e_list ~loc lst'
  | ENil reg ->
      let loc = Location.lift reg in
      return @@ e_list ~loc []

and simpl_set_expression (t:Raw.set_expr) : expression result =
  match t with
  | SetMem x -> (
    let (x' , loc) = r_split x in
    let%bind set' = simpl_expression x'.set in
    let%bind element' = simpl_expression x'.element in
    ok @@ e_constant ~loc C_SET_MEM [ element' ; set' ]
  )
  | SetInj x -> (
    let (x' , loc) = r_split x in
    let elements = pseq_to_list x'.elements in
    let%bind elements' = bind_map_list simpl_expression elements in
    ok @@ e_set ~loc elements'
  )

and simpl_binop (name:string) (t:_ Raw.bin_op Region.reg) : expression result =
  let return x = ok x in
  let (t , loc) = r_split t in
  let%bind a = simpl_expression t.arg1 in
  let%bind b = simpl_expression t.arg2 in
  let%bind name = constants name in
  return @@ e_constant ~loc name [ a ; b ]

and simpl_unop (name:string) (t:_ Raw.un_op Region.reg) : expression result =
  let return x = ok x in
  let (t , loc) = r_split t in
  let%bind a = simpl_expression t.arg in
  let%bind name = constants name in
  return @@ e_constant ~loc name [ a ]

and simpl_tuple_expression ?loc (lst:Raw.expr list) : expression result =
  let return x = ok x in
  match lst with
  | [] -> return @@ e_literal Literal_unit
  | [hd] -> simpl_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_expression lst
      in return @@ e_tuple ?loc lst

and simpl_data_declaration : Raw.data_decl -> _ result =
  fun t ->
  match t with
  | LocalVar x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.var_type in
      let%bind expression = simpl_expression x.init in
      return_let_in ~loc (Var.of_name name, Some t) false false expression
  | LocalConst x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.const_type in
      let%bind expression = simpl_expression x.init in
      let inline =
        match x.attributes with
          None -> false
        | Some {value; _} ->
           npseq_to_list value.ne_elements
           |> List.exists (fun Region.{value; _} -> value = "\"inline\"")
      in return_let_in ~loc (Var.of_name name, Some t) false inline expression
  | LocalFun f  ->
      let (f , loc) = r_split f in
      let%bind (binder, expr) = simpl_fun_decl ~loc f in
      let inline =
        match f.attributes with
          None -> false
        | Some {value; _} ->
           npseq_to_list value.ne_elements
           |> List.exists (fun Region.{value; _} -> value = "\"inline\"")
      in return_let_in ~loc binder false inline expr

and simpl_param :
      Raw.param_decl -> (string * type_expression) result =
  fun t ->
  match t with
  | ParamConst c ->
      let c = c.value in
      let param_name = c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok (param_name , type_expression)
  | ParamVar v ->
      let c = v.value in
      let param_name = c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok (param_name , type_expression)

and simpl_fun_decl :
      loc:_ -> Raw.fun_decl ->
      ((expression_variable * type_expression option) * expression) result =
  fun ~loc x ->
  let open! Raw in
  let {fun_name; param; ret_type; block_with;
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
       let%bind input = simpl_param a in
       let (binder , input_type) = input in
       let%bind instructions = simpl_statement_list statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression : expression = e_lambda ~loc (Var.of_name binder) (Some input_type)
           (Some output_type) result in
       let type_annotation =
         Some (make_t @@ T_arrow {type1=input_type;type2=output_type}) in
       ok ((Var.of_name fun_name.value, type_annotation), expression)
     )
   | lst -> (
       let lst = npseq_to_list lst in
       (* TODO wrong, should be fresh? *)
       let arguments_name = Var.of_name "arguments" in
       let%bind params = bind_map_list simpl_param lst in
       let (binder , input_type) =
         let type_expression = t_tuple (List.map snd params) in
         (arguments_name , type_expression) in
       let%bind tpl_declarations =
         let aux = fun i (param, type_expr) ->
           let expr =
             e_accessor (e_variable arguments_name) (string_of_int i) in
           let type_variable = Some type_expr in
           let ass = return_let_in (Var.of_name param , type_variable) false inline expr in
           ass
         in
         bind_list @@ List.mapi aux params in
       let%bind instructions = simpl_statement_list statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = tpl_declarations @ instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression =
         e_lambda ~loc binder (Some (input_type)) (Some output_type) result in
       let type_annotation = Some (make_t @@ T_arrow {type1=input_type; type2=output_type}) in
       ok ((Var.of_name fun_name.value, type_annotation), expression)
     )
  )

and simpl_fun_expression :
  loc:_ -> Raw.fun_expr -> (type_expression option * expression) result =
  fun ~loc x ->
  let open! Raw in
  let {param;ret_type;return;_} : fun_expr = x in
  let statements = [] in
  (match param.value.inside with
     a, [] -> (
       let%bind input = simpl_param a in
       let (binder , input_type) = input in
       let%bind instructions = simpl_statement_list statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression : expression = e_lambda ~loc (Var.of_name binder) (Some input_type)
           (Some output_type) result in
       let type_annotation = Some (make_t @@ T_arrow {type1=input_type;type2=output_type}) in
       ok (type_annotation , expression)
     )
   | lst -> (
       let lst = npseq_to_list lst in
       (* TODO wrong, should be fresh? *)
       let arguments_name = Var.of_name "arguments" in
       let%bind params = bind_map_list simpl_param lst in
       let (binder , input_type) =
         let type_expression = t_tuple (List.map snd params) in
         (arguments_name , type_expression) in
       let%bind tpl_declarations =
         let aux = fun i (param, param_type) ->
           let expr = e_accessor (e_variable arguments_name) (string_of_int i) in
           let type_variable = Some param_type in
           let ass = return_let_in (Var.of_name param , type_variable) false false expr in
           ass
         in
         bind_list @@ List.mapi aux params in
       let%bind instructions = simpl_statement_list statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = tpl_declarations @ instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression =
         e_lambda ~loc binder (Some (input_type)) (Some output_type) result in
       let type_annotation = Some (make_t @@ T_arrow {type1=input_type;type2=output_type}) in
       ok (type_annotation , expression)
     )
  )

and simpl_statement_list statements =
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
      hook (simpl_instruction i :: acc) statements
  | Data d :: statements ->
      hook (simpl_data_declaration d :: acc) statements
  in bind_list @@ hook [] (List.rev statements)

and get_case_variables (t:Raw.pattern) : expression_variable list result = 
  match t with 
    | PConstr PFalse _ 
    | PConstr PTrue _ 
    | PConstr PNone _ -> ok @@ [] 
    | PConstr PSomeApp v -> (let (_,v) = v.value in get_case_variables (v.value.inside)) 
    | PConstr PConstrApp v -> (
      match v.value with
      | constr, None -> ok @@ [ Var.of_name constr.value]
      | constr, pat_opt ->
        let%bind pat =
          trace_option (unsupported_cst_constr t) @@
          pat_opt in
        let pat = npseq_to_list pat.value.inside in
        let%bind var = bind_map_list get_case_variables pat in
        ok @@ [Var.of_name constr.value ] @ (List.concat var)
    )
    | PList PNil _  -> ok @@ []
    | PList PCons c -> (
        match c.value with
        | a, [(_, b)] ->
            let%bind a = get_case_variables a in
            let%bind b = get_case_variables b in
            ok @@ a@b
        | _ -> fail @@ unsupported_deep_list_patterns c
    )
    | PVar  v -> ok @@ [Var.of_name v.value] 
    | p -> fail @@ unsupported_cst_constr p

and simpl_single_instruction : Raw.instruction -> (_ -> expression result) result =
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
      | Error _  ->
         let%bind arg = simpl_tuple_expression ~loc:args_loc args' in
         return_statement @@ e_application ~loc (e_variable ~loc:f_loc (Var.of_name f_name)) arg
      | Ok (s,_) ->
         let%bind lst = bind_map_list simpl_expression args' in
         return_statement @@ e_constant ~loc s lst
    )
    | f -> (
      let%bind f' = simpl_expression f in
      let%bind arg = simpl_tuple_expression ~loc:args_loc args' in
      return_statement @@ e_application ~loc f' arg
    )
  )
  | Skip reg -> (
      let loc = Location.lift reg in
      return_statement @@ e_skip ~loc ()
    )
  | Loop (While l) ->
      simpl_while_loop l.value
  | Loop (For (ForInt fi)) -> (
      let%bind loop = simpl_for_int fi.value in
      ok loop
  )
  | Loop (For (ForCollect fc)) ->
      let%bind loop = simpl_for_collect fc.value in
      ok loop
  | Cond c -> (
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.test in
      let%bind match_true = match c.ifso with
          ClauseInstr i ->
            simpl_single_instruction i
        | ClauseBlock b ->
            match b with
              LongBlock {value; _} ->
                simpl_block value
            | ShortBlock {value; _} ->
                simpl_statements @@ fst value.inside in
      let%bind match_false = match c.ifnot with
          ClauseInstr i ->
            simpl_single_instruction i
        | ClauseBlock b ->
            match b with
              LongBlock {value; _} ->
                simpl_block value
            | ShortBlock {value; _} ->
                simpl_statements @@ fst value.inside in
      let env = Var.fresh () in
      
      let%bind match_true' = match_true None in
      let%bind match_false' = match_false None in
      let%bind match_true  = match_true @@ Some (e_variable env) in
      let%bind match_false = match_false @@ Some (e_variable env) in

      let%bind ((_,free_vars_true), match_true) = repair_mutable_variable match_true [] env in
      let%bind ((_,free_vars_false), match_false) = repair_mutable_variable match_false [] env in
      let free_vars = free_vars_true @ free_vars_false in
      if (List.length free_vars != 0) then 
        let match_expr  = e_matching expr ~loc (Match_bool {match_true; match_false}) in
        let return_expr = e_let_in (env,None) false false (store_mutable_variable free_vars) match_expr in
        restore_mutable_variable return_expr free_vars env
      else
        return_statement @@ e_matching expr ~loc (Match_bool {match_true=match_true'; match_false=match_false'})
    )
  | Assign a -> (
      let (a , loc) = r_split a in
      let%bind value_expr = simpl_expression a.rhs in
      match a.lhs with
        | Path path -> (
            let (name , path') = simpl_path path in
            let (let_binder, mut, rhs, inline) = e_assign_with_let ~loc name path' value_expr in
            return_let_in let_binder mut inline rhs
          )
        | MapPath v -> (
            let v' = v.value in
            let%bind (varname,map,path) = match v'.path with
              | Name name -> ok (name.value , e_variable (Var.of_name name.value), [])
              | Path p ->
                let (name,p') = simpl_path v'.path in
                let%bind accessor = simpl_projection p in
                ok @@ (name , accessor , p')
            in
            let%bind key_expr = simpl_expression v'.index.value.inside in
            let expr' = e_map_add key_expr value_expr map in
            let (let_binder, mut, rhs, inline) = e_assign_with_let ~loc varname path expr' in
            return_let_in let_binder mut inline rhs  
          )
    )
  | CaseInstr c -> (
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.expr in
      let env = Var.fresh () in
      let%bind (fv,cases) =
        let aux fv (x : Raw.if_clause Raw.case_clause Raw.reg) =
          let%bind case_clause =
            match x.value.rhs with
              ClauseInstr i ->
                simpl_single_instruction i
            | ClauseBlock b ->
                match b with
                  LongBlock {value; _} ->
                    simpl_block value
                | ShortBlock {value; _} ->
                  simpl_statements @@ fst value.inside in
          let%bind case_clause'= case_clause @@ None in
          let%bind case_clause = case_clause @@ Some(e_variable env) in
          let%bind case_vars   = get_case_variables x.value.pattern in
          let%bind ((_,free_vars), case_clause) = repair_mutable_variable case_clause case_vars env in
          ok (free_vars::fv,(x.value.pattern, case_clause, case_clause')) in
        bind_fold_map_list aux [] (npseq_to_list c.cases.value) in
      let free_vars = List.concat fv in
      if (List.length free_vars == 0) then (
        let cases = List.map (fun case -> let (a,_,b) = case in (a,b)) cases in
        let%bind m = simpl_cases cases in
        return_statement @@ e_matching ~loc expr m
      ) else (
        let cases = List.map (fun case -> let (a,b,_) = case in (a,b)) cases in
        let%bind m = simpl_cases cases in
        let match_expr = e_matching ~loc expr m in
        let return_expr = e_let_in (env,None) false false (store_mutable_variable free_vars) match_expr in
        restore_mutable_variable return_expr free_vars env
      )
    )
  | RecordPatch r -> (
      let reg = r.region in
      let (r,loc) = r_split r in
      let aux (fa :Raw.field_assign Raw.reg) : Raw.field_path_assign Raw.reg=
         {value = {field_path = (fa.value.field_name, []); equal=fa.value.equal; field_expr = fa.value.field_expr};
         region = fa.region}
      in
      let update : Raw.field_path_assign Raw.reg Raw.ne_injection Raw.reg = {
        value = Raw.map_ne_injection aux r.record_inj.value;
        region=r.record_inj.region
        } in
      let u : Raw.update = {record=r.path;kwd_with=r.kwd_with; updates=update} in
      let%bind expr = simpl_update {value=u;region=reg} in
      let (name , access_path) = simpl_path r.path in
      let loc = Some loc in
      let (binder, mut, rhs, inline) = e_assign_with_let ?loc name access_path expr in
      return_let_in binder mut inline rhs

  )
  | MapPatch patch -> (
      let (map_p, loc) = r_split patch in
      let (name, access_path) = simpl_path map_p.path in
      let%bind inj = bind_list
          @@ List.map (fun (x:Raw.binding Region.reg) ->
            let x = x.value in
            let (key, value) = x.source, x.image in
            let%bind key' = simpl_expression key in
            let%bind value' = simpl_expression value
            in ok @@ (key', value')
          )
        @@ npseq_to_list map_p.map_inj.value.ne_elements in
      match inj with
      | [] -> return_statement @@ e_skip ~loc ()
      | _ :: _ ->
        let assigns = List.fold_right
            (fun (key, value) map ->  (e_map_add key value map))
            inj
            (e_accessor_list ~loc (e_variable (Var.of_name name)) access_path)
        in 
        let (binder, mut, rhs, inline) = e_assign_with_let ~loc name access_path assigns in
        return_let_in binder mut inline rhs
    )
  | SetPatch patch -> (
      let (setp, loc) = r_split patch in
      let (name , access_path) = simpl_path setp.path in
      let%bind inj =
        bind_list @@
        List.map simpl_expression @@
        npseq_to_list setp.set_inj.value.ne_elements in
      match inj with
      | [] -> return_statement @@ e_skip ~loc ()
      | _ :: _ ->
        let assigns = List.fold_right
          (fun hd s -> e_constant C_SET_ADD [hd ; s])
          inj (e_accessor_list ~loc (e_variable (Var.of_name name)) access_path) in
        let (binder, mut, rhs, inline) = e_assign_with_let ~loc name access_path assigns in
        return_let_in binder mut inline rhs
    )
  | MapRemove r -> (
      let (v , loc) = r_split r in
      let key = v.key in
      let%bind (varname,map,path) = match v.map with
        | Name v -> ok (v.value , e_variable (Var.of_name v.value) , [])
        | Path p ->
          let (name,p') = simpl_path v.map in
          let%bind accessor = simpl_projection p in
          ok @@ (name , accessor , p')
      in
      let%bind key' = simpl_expression key in
      let expr = e_constant ~loc C_MAP_REMOVE [key' ; map] in
      let (binder, mut, rhs, inline) = e_assign_with_let ~loc varname path expr in
      return_let_in binder mut inline rhs
    )
  | SetRemove r -> (
      let (set_rm, loc) = r_split r in
      let%bind (varname, set, path) = match set_rm.set with
        | Name v -> ok (v.value, e_variable (Var.of_name v.value), [])
        | Path path ->
          let(name, p') = simpl_path set_rm.set in
          let%bind accessor = simpl_projection path in
          ok @@ (name, accessor, p')
      in
      let%bind removed' = simpl_expression set_rm.element in
      let expr = e_constant ~loc C_SET_REMOVE [removed' ; set] in
      let (binder, mut, rhs, inline) = e_assign_with_let ~loc varname path expr in
      return_let_in binder mut inline rhs
    )

and simpl_path : Raw.path -> string * string list = fun p ->
  match p with
  | Raw.Name v -> (v.value , [])
  | Raw.Path p -> (
      let p' = p.value in
      let var = p'.struct_name.value in
      let path = p'.field_path in
      let path' =
        let aux (s:Raw.selection) =
          match s with
          | FieldName property -> property.value
          | Component index -> (Z.to_string (snd index.value))
        in
        List.map aux @@ npseq_to_list path in
      (var , path')
    )

and simpl_cases : (Raw.pattern * expression) list -> matching_expr result = fun t ->
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
      trace_strong (unsupported_tuple_pattern t) @@
      Assert.assert_list_size t' 1 in
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
      ok @@ Match_bool {match_true = t ; match_false = f}
  | [(PConstr PSomeApp v , some) ; (PConstr PNone _ , none)]
  | [(PConstr PNone _ , none) ; (PConstr PSomeApp v , some)] -> (
      let (_, v) = v.value in
      let%bind v = match v.value.inside with
        | PVar v -> ok v.value
        | p -> fail @@ unsupported_deep_Some_patterns p in
      ok @@ Match_option {match_none = none ; match_some = (Var.of_name v, some, ()) }
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
      ok @@ Match_list {match_cons = (Var.of_name a, Var.of_name b, cons,()) ; match_nil = nil}
  | lst ->
      trace (simple_info "currently, only booleans, options, lists and \
                         user-defined constructors are supported in patterns") @@
      let%bind constrs =
        let aux (x , y) =
          let error =
            let title () = "Pattern" in
            (* TODO: The labelled arguments should be flowing from the CLI. *)
            let content () =
              Printf.sprintf "Pattern : %s"
                (ParserLog.pattern_to_string
                   ~offsets:true ~mode:`Point x) in
            error title content in
          let%bind x' =
            trace error @@
            get_constr x in
          ok (x' , y) in
        bind_map_list aux lst in
      ok @@ ez_match_variant constrs

and simpl_instruction : Raw.instruction -> (_ -> expression result) result =
  fun t -> trace (simplifying_instruction t) @@ simpl_single_instruction t

and simpl_statements : Raw.statements -> (_ -> expression result) result =
  fun statements ->
    let lst = npseq_to_list statements in
    let%bind fs = simpl_statement_list lst in
    let aux : _ -> (expression option -> expression result) -> _ =
      fun prec cur ->
        let%bind res = cur prec
        in ok @@ Some res in
    ok @@ fun (expr' : _ option) ->
           let%bind ret = bind_fold_right_list aux expr' fs in
           ok @@ Option.unopt_exn ret

and simpl_block : Raw.block -> (_ -> expression result) result =
  fun t -> simpl_statements t.statements

and simpl_while_loop : Raw.while_loop -> (_ -> expression result) result = fun wl ->
  let env_rec = Var.fresh () in
  let binder  = Var.fresh () in

  let%bind cond = simpl_expression wl.cond in
  let%bind for_body = simpl_block wl.block.value in

  let ctrl = 
    (e_variable binder)
    in
  let%bind for_body = for_body @@ Some( ctrl ) in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable for_body [] binder in

  let aux name expr=
    e_let_in (name,None) false false (e_accessor (e_variable binder) (Var.to_name name)) expr
  in
  let init_rec = store_mutable_variable @@ captured_name_list in
  let restore = fun expr -> List.fold_right aux captured_name_list expr in
  let continue_expr = e_constant C_CONTINUE [for_body] in
  let stop_expr = e_constant C_STOP [e_variable binder] in
  let aux_func = e_cond cond continue_expr (stop_expr) in
  let aux_func = (restore (aux_func)) in
  let aux_func = e_lambda binder None None @@ aux_func in
  let loop = e_constant C_FOLD_WHILE [aux_func; e_variable env_rec] in
  let return_expr = e_let_in (env_rec,None) false false init_rec (loop) in
  restore_mutable_variable return_expr captured_name_list env_rec 


and simpl_for_int : Raw.for_int -> (_ -> expression result) result = fun fi ->
  let env_rec = Var.fresh () in
  let binder  = Var.fresh () in
  let name = fi.assign.value.name.value in
  let it = Var.of_name name in
  let var = e_variable it in
  (*Make the cond and the step *)
  let%bind value = simpl_expression fi.assign.value.expr in
  let%bind bound = simpl_expression fi.bound in
  let cond = e_annotation (e_constant C_LE [var ; bound]) t_bool in
  let step = e_int 1 in
  let ctrl = 
    e_let_in (it,Some t_int) false false (e_constant C_ADD [ var ; step ]) 
    (e_let_in (binder, None) false false (e_update (e_variable binder) name var)
    (e_variable binder))
    in
  (* Modify the body loop*)
  let%bind for_body = simpl_block fi.block.value in
  let%bind for_body = for_body @@ Some( ctrl ) in
  let%bind ((_,captured_name_list),for_body) = repair_mutable_variable for_body [it] binder in

  let aux name expr=
    e_let_in (name,None) false false (e_accessor (e_variable binder) (Var.to_name name)) expr
  in

  (* restores the initial value of the free_var*)
  let restore = fun expr -> List.fold_right aux captured_name_list expr in

  (*Prep the lambda for the fold*)
  let continue_expr = e_constant C_CONTINUE [for_body] in
  let stop_expr = e_constant C_STOP [e_variable binder] in
  let aux_func = e_cond cond continue_expr (stop_expr) in
  let aux_func = e_let_in (it,Some t_int) false false (e_accessor (e_variable binder) name) (restore (aux_func)) in
  let aux_func = e_lambda binder None None @@ aux_func in

  (* Make the fold_while en precharge the vakye *)
  let loop = e_constant C_FOLD_WHILE [aux_func; e_variable env_rec] in
  let init_rec = store_mutable_variable @@ it::captured_name_list in
  let return_expr = e_let_in (env_rec,None) false false init_rec (loop) in
  let return_expr = e_let_in (it, Some t_int) false false value @@ return_expr in
  restore_mutable_variable return_expr captured_name_list env_rec 

and simpl_for_collect : Raw.for_collect -> (_ -> expression result) result = fun fc ->
  let _elt_name = fc.var.value in
  let binder = Var.of_name "arguments" in
  let%bind element_names = ok @@ match fc.bind_to with
    | Some v -> [Var.of_name fc.var.value;Var.of_name (snd v).value]
    | None -> [Var.of_name fc.var.value] in
  
  let env = Var.fresh () in
  let%bind for_body = simpl_block fc.block.value in
  let%bind _for_body' = for_body None in
  let%bind for_body = for_body @@ Some (e_accessor (e_variable binder) "0") in
  let%bind ((_,free_vars), for_body) = repair_mutable_variable_for_collect for_body element_names binder in

  let init_record = store_mutable_variable free_vars in
  let%bind collect = simpl_expression fc.expr in
  let aux name expr=
    e_let_in (name,None) false false (e_accessor (e_accessor (e_variable binder) "0") (Var.to_name name)) expr
  in
  let restore = fun expr -> List.fold_right aux free_vars expr in
  let restore = match fc.collection with
    | Map _ -> (match fc.bind_to with 
      | Some v -> fun expr -> restore (e_let_in (Var.of_name  fc.var.value, None) false false (e_accessor (e_accessor (e_variable binder) "1") "0") 
                                    (e_let_in (Var.of_name (snd v).value, None) false false (e_accessor (e_accessor (e_variable binder) "1") "1") expr))
      | None -> fun expr -> restore (e_let_in (Var.of_name  fc.var.value, None) false false (e_accessor (e_accessor (e_variable binder) "1") "0") expr) 
    )
    | _ -> fun expr -> restore (e_let_in (Var.of_name fc.var.value, None) false false (e_accessor (e_variable binder) "1") expr)
  in
  let lambda = e_lambda binder None None (restore for_body) in
  let op_name = match fc.collection with
   | Map _ -> C_MAP_FOLD | Set _ -> C_SET_FOLD | List _ -> C_LIST_FOLD in
  let fold = e_constant op_name [lambda; collect ; init_record] in
  restore_mutable_variable fold free_vars env

and simpl_declaration_list declarations :
  Ast_simplified.declaration Location.wrap list result =
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
      let%bind type_expression = simpl_type_expression type_expr in
      let new_decl =
        Declaration_type (Var.of_name name.value, type_expression) in
      let res = Location.wrap ~loc new_decl in
      hook (bind_list_cons res acc) declarations
  | ConstDecl decl :: declarations ->
      let simpl_const_decl =
        fun {name;const_type; init; attributes} ->
          let%bind expression = simpl_expression init in
          let%bind t = simpl_type_expression const_type in
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
        bind_map_location simpl_const_decl (Location.lift_region decl)
      in hook (bind_list_cons res acc) declarations
  | FunDecl fun_decl :: declarations ->
      let decl, loc = r_split fun_decl in
      let%bind ((name, ty_opt), expr) = simpl_fun_decl ~loc decl in
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

let simpl_program : Raw.ast -> program result =
  fun t -> simpl_declaration_list @@ nseq_to_list t.decl
