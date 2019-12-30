open Trace
open Ast_simplified

module Raw = Parser.Pascaligo.AST
module SMap = Map.String
module SSet = Set.Make (String)

open Combinators

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value
let is_compiler_generated name = String.contains (Var.to_name name) '#'

let detect_local_declarations (for_body : expression) =
  let%bind aux = Self_ast_simplified.fold_expression
    (fun (nlist, cur_loop : expression_variable list * bool) (ass_exp : expression) ->
      if cur_loop then
        match ass_exp.expression with
        | E_let_in {binder;rhs = _;result = _} ->
          let (name,_) = binder in
          ok (name::nlist, cur_loop)
        | E_constant (C_MAP_FOLD, _)
        | E_constant (C_SET_FOLD, _)
        | E_constant (C_LIST_FOLD, _) -> ok @@ (nlist, false)
        | _ -> ok (nlist, cur_loop)
      else
        ok @@ (nlist, cur_loop)
    )
    ([], true)
    for_body in
  ok @@ fst aux

let detect_free_variables (for_body : expression) (local_decl_names : expression_variable list) =
  let%bind captured_names = Self_ast_simplified.fold_expression
    (fun (prev : expression_variable list) (ass_exp : expression) ->
      match ass_exp.expression with
      | E_assign ( name , _ , _ ) ->
        if is_compiler_generated name then ok prev
        else ok (name::prev)
      | E_constant (n, [a;b])
        when n=C_OR || n=C_AND || n=C_LT || n=C_GT ||
             n=C_LE || n=C_GE  || n=C_EQ || n=C_NEQ -> (
          match (a.expression,b.expression) with
          | E_variable na , E_variable nb ->
            let ret = [] in
            let ret = if not (is_compiler_generated na) then
              na::ret else ret in
            let ret = if not (is_compiler_generated nb) then
              nb::ret else ret in
            ok (ret@prev)
          | E_variable n , _
          | _            , E_variable n ->
            if not (is_compiler_generated n) then
            ok (n::prev) else ok prev
          | _ -> ok prev)
      | _ -> ok prev )
    []
    for_body in
  let captured_names = List.map (fun (s) -> Var.to_name s) captured_names in
  let local_decl_names = List.map (fun (s) -> Var.to_name s) local_decl_names in
  ok @@ SSet.elements
     @@ SSet.diff (SSet.of_list captured_names) (SSet.of_list local_decl_names)

module Errors = struct
  let unsupported_cst_constr p =
    let title () = "constant constructor" in
    let message () =
      Format.asprintf "constant constructors are not supported yet" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let bad_bytes loc str =
    let title () = "bad bytes string" in
    let message () =
      Format.asprintf "bytes string contained non-hexadecimal chars" in
    let data = [
      ("location", fun () -> Format.asprintf "%a" Location.pp loc) ;
      ("bytes", fun () -> str) ;
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
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ name.Region.region)
    ] in
    error ~data title message

  let unsupported_non_var_pattern p =
    let title () = "pattern is not a variable" in
    let message () =
      Format.asprintf "non-variable patterns in constructors \
                       are not supported yet" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let only_constructors p =
    let title () = "constructors in patterns" in
    let message () =
      Format.asprintf "currently, only constructors are supported in patterns" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_tuple_pattern p =
    let title () = "tuple pattern" in
    let message () =
      Format.asprintf "tuple patterns are not supported yet" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc) ;
      (** TODO: The labelled arguments should be flowing from the CLI. *)
      ("pattern",
       fun () -> Parser.Pascaligo.ParserLog.pattern_to_string
                ~offsets:true ~mode:`Point p)
    ] in
    error ~data title message

  let unsupported_deep_Some_patterns pattern =
    let title () = "option patterns" in
    let message () =
      Format.asprintf "currently, only variables in Some constructors \
                       in patterns are supported" in
    let pattern_loc = Raw.pattern_to_region pattern in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_deep_list_patterns cons =
    let title () = "lists in patterns" in
    let message () =
      Format.asprintf "currently, only empty lists and x::y \
                       are supported in patterns" in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ cons.Region.region)
    ] in
    error ~data title message

  let unexpected_anonymous_function loc =
    let title () = "unexpected anonymous function" in
    let message () = "you provided a function declaration without name" in
    let data = [
        ("location" , fun () -> Format.asprintf "%a" Location.pp @@ loc)
      ] in
    error ~data title message

  let unexpected_named_function loc =
    let title () = "unexpected named function" in
    let message () = "you provided a function expression with a name (remove it)" in
    let data = [
        ("location" , fun () -> Format.asprintf "%a" Location.pp @@ loc)
      ] in
    error ~data title message

  (* Logging *)

  let simplifying_instruction t =
    let title () = "simplifiying instruction" in
    let message () = "" in
    (** TODO: The labelled arguments should be flowing from the CLI. *)
    let data = [
      ("instruction",
       fun () -> Parser.Pascaligo.ParserLog.instruction_to_string
                ~offsets:true ~mode:`Point t)
    ] in
    error ~data title message
end

open Errors
open Operators.Simplify.Pascaligo

let r_split = Location.r_split

(*
  Statements can't be simplified in isolation. `a ; b ; c` can get simplified either
  as `let x = expr in (b ; c)` if `a` is a ` const x = expr` declaration or as
  `sequence(a , sequence(b , c))` for everything else.
  Because of this, simplifying sequences depend on their contents. To avoid peeking in
  their contents, we instead simplify sequences elements as functions from their next
  elements to the actual result.

  For `return_let_in`, if there is no follow-up element, an error is triggered, as
  you can't have `let x = expr in ...` with no `...`. A cleaner option might be to add
  a `unit` instead of erroring.

  `return_statement` is used for non-let_in statements.
*)
let return_let_in ?loc binder rhs = ok @@ fun expr'_opt ->
  match expr'_opt with
  | None -> fail @@ corner_case ~loc:__LOC__ "missing return"
  | Some expr' -> ok @@ e_let_in ?loc binder rhs expr'

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
      ok @@ make_t @@ T_arrow (a , b)
    )
  | TApp x ->
      let (name, tuple) = x.value in
      let lst = npseq_to_list tuple.value.inside in
      let%bind lst = bind_list @@ List.map simpl_type_expression lst in (** TODO: fix constant and operator*)
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
      ok @@ make_t @@ T_tuple lst

let simpl_projection : Raw.projection Region.reg -> _ = fun p ->
  let (p' , loc) = r_split p in
  let var =
    let name = Var.of_name p'.struct_name.value in
    e_variable name in
  let path = p'.field_path in
  let path' =
    let aux (s:Raw.selection) =
      match s with
      | FieldName property -> Access_record property.value
      | Component index -> Access_tuple (Z.to_int (snd index.value))
    in
    List.map aux @@ npseq_to_list path in
  ok @@ e_accessor ~loc var path'


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
    return @@ e_literal ~loc (Literal_bytes (Bytes.of_string @@ fst x'))
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
      return @@ e_matching expr ~loc (Match_bool {match_true; match_false})
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
  | EFun f -> (
    let (f , loc) = r_split f in
    let%bind ((name_opt , _ty_opt) , f') = simpl_fun_expression ~loc f in
    match name_opt with
    | None -> return @@ f'
    | Some _ -> fail @@ unexpected_named_function loc
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
      let%bind lst = bind_list @@ List.map simpl_expression lst in
      return @@ e_tuple ?loc lst

and simpl_data_declaration : Raw.data_decl -> _ result = fun t ->
  match t with
  | LocalVar x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.var_type in
      let%bind expression = simpl_expression x.init in
      return_let_in ~loc (Var.of_name name , Some t) expression
  | LocalConst x ->
      let (x , loc) = r_split x in
      let name = x.name.value in
      let%bind t = simpl_type_expression x.const_type in
      let%bind expression = simpl_expression x.init in
      return_let_in ~loc (Var.of_name name , Some t) expression
  | LocalFun f  ->
      let (f , loc) = r_split f in
      let%bind ((name_opt , ty_opt) , e) = simpl_fun_expression ~loc f.fun_expr.value in
      let%bind name = trace_option (unexpected_anonymous_function loc) name_opt in
      return_let_in ~loc (name , ty_opt) e

and simpl_param : Raw.param_decl -> (expression_variable * type_expression) result =
  fun t ->
  match t with
  | ParamConst c ->
      let c = c.value in
      let type_name = Var.of_name c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok (type_name , type_expression)
  | ParamVar v ->
      let c = v.value in
      let type_name = Var.of_name c.var.value in
      let%bind type_expression = simpl_type_expression c.param_type in
      ok (type_name , type_expression)

and simpl_fun_expression :
  loc:_ -> Raw.fun_expr -> ((expression_variable option * type_expression option) * expression) result =
  fun ~loc x ->
  let open! Raw in
  let {name;param;ret_type;block_with;return} : fun_expr = x in
  let statements =
    match block_with with
    | Some (block,_) -> npseq_to_list block.value.statements
    | None -> []
  in
  (match param.value.inside with
     a, [] -> (
       let%bind input = simpl_param a in
       let name = Option.map (fun (x : _ reg) -> Var.of_name x.value) name in
       let (binder , input_type) = input in
       let%bind instructions = bind_list
         @@ List.map simpl_statement
         @@ statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression : expression = e_lambda ~loc binder (Some input_type)
           (Some output_type) result in
       let type_annotation = Some (make_t @@ T_arrow (input_type, output_type)) in
       ok ((name , type_annotation) , expression)
     )
   | lst -> (
       let lst = npseq_to_list lst in
       let arguments_name = Var.of_name "arguments" in (* TODO wrong, should be fresh? *)
       let%bind params = bind_map_list simpl_param lst in
       let (binder , input_type) =
         let type_expression = T_tuple (List.map snd params) in
         (arguments_name , type_expression) in
       let%bind tpl_declarations =
         let aux = fun i x ->
           let expr = e_accessor (e_variable arguments_name) [Access_tuple i] in
           let type_variable = Some (snd x) in
           let ass = return_let_in (fst x , type_variable) expr in
           ass
         in
         bind_list @@ List.mapi aux params in
       let%bind instructions = bind_list
         @@ List.map simpl_statement
         @@ statements in
       let%bind result = simpl_expression return in
       let%bind output_type = simpl_type_expression ret_type in
       let body = tpl_declarations @ instructions in
       let%bind result =
         let aux prec cur = cur (Some prec) in
         bind_fold_right_list aux result body in
       let expression =
         e_lambda ~loc binder (Some (make_t @@ input_type)) (Some output_type) result in
       let type_annotation = Some (make_t @@ T_arrow (make_t input_type, output_type)) in
       let name = Option.map (fun (x : _ reg) -> Var.of_name x.value) name in
       ok ((name , type_annotation) , expression)
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
      ok @@ Location.wrap ~loc (Declaration_type (Var.of_name name.value , type_expression))
    )
  | ConstDecl x ->
      let simpl_const_decl = fun {name;const_type;init} ->
        let%bind expression = simpl_expression init in
        let%bind t = simpl_type_expression const_type in
        let type_annotation = Some t in
        ok @@ Declaration_constant (Var.of_name name.value , type_annotation , expression)
      in
      bind_map_location simpl_const_decl (Location.lift_region x)
  | FunDecl x -> (
      let (x , loc) = r_split x in
      let%bind ((name_opt , ty_opt) , expr) = simpl_fun_expression ~loc x.fun_expr.value in
      let%bind name = trace_option (unexpected_anonymous_function loc) name_opt in
      ok @@ Location.wrap ~loc (Declaration_constant (name , ty_opt , expr))
    )

and simpl_statement : Raw.statement -> (_ -> expression result) result =
  fun s ->
  match s with
  | Instr i -> simpl_instruction i
  | Data d -> simpl_data_declaration d

and simpl_single_instruction : Raw.instruction -> (_ -> expression result) result =
  fun t ->
  match t with
  | ProcCall x -> (
    let ((f, args) , loc) = r_split x in
    let (args , args_loc) = r_split args in
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
      let l = l.value in
      let%bind cond = simpl_expression l.cond in
      let%bind body = simpl_block l.block.value in
      let%bind body = body None in
      return_statement @@ e_loop cond body
  | Loop (For (ForInt fi)) ->
      let%bind loop = simpl_for_int fi.value in
      let%bind loop = loop None in
      return_statement @@ loop
  | Loop (For (ForCollect fc)) ->
      let%bind loop = simpl_for_collect fc.value in
      let%bind loop = loop None in
      return_statement @@ loop
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
      let%bind match_true = match_true None in
      let%bind match_false = match_false None in
      return_statement @@ e_matching expr ~loc (Match_bool {match_true; match_false})
    )
  | Assign a -> (
      let (a , loc) = r_split a in
      let%bind value_expr = simpl_expression a.rhs in
      match a.lhs with
        | Path path -> (
            let (name , path') = simpl_path path in
            return_statement @@ e_assign ~loc name path' value_expr
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
            return_statement @@ e_assign ~loc varname path expr'
          )
    )
  | CaseInstr c -> (
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.expr in
      let%bind cases =
        let aux (x : Raw.if_clause Raw.case_clause Raw.reg) =
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
          let%bind case_clause = case_clause None in
          ok (x.value.pattern, case_clause) in
        bind_list
        @@ List.map aux
        @@ npseq_to_list c.cases.value in
      let%bind m = simpl_cases cases in
      return_statement @@ e_matching ~loc expr m
    )
  | RecordPatch r -> (
      let r = r.value in
      let (name , access_path) = simpl_path r.path in

      let head, tail = r.record_inj.value.ne_elements in

      let%bind tail' = bind_list
        @@ List.map (fun (x: Raw.field_assign Region.reg) ->
            let (x , loc) = r_split x in
            let%bind e = simpl_expression x.field_expr
            in ok (x.field_name.value, e , loc)
          )
        @@ List.map snd tail in

      let%bind head' =
        let (x , loc) = r_split head in
        let%bind e = simpl_expression x.field_expr
        in ok (x.field_name.value, e , loc) in

      let%bind expr =
        let aux = fun (access , v , loc) ->
          e_assign ~loc name (access_path @ [Access_record access]) v in

        let hd, tl = aux head', List.map aux tail' in
        let aux acc cur = e_sequence acc cur in
        ok @@ List.fold_left aux hd tl
      in
      return_statement @@ expr
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
      let expr =
        match inj with
        | [] -> e_skip ~loc ()
        | _ :: _ ->
          let assigns = List.fold_right
              (fun (key, value) map -> (e_map_add key value map))
              inj
              (e_accessor ~loc (e_variable (Var.of_name name)) access_path)
          in e_assign ~loc name access_path assigns
      in return_statement @@ expr
    )
  | SetPatch patch -> (
      let (setp, loc) = r_split patch in
      let (name , access_path) = simpl_path setp.path in
      let%bind inj =
        bind_list @@
        List.map simpl_expression @@
        npseq_to_list setp.set_inj.value.ne_elements in
      let expr =
        match inj with
        | [] -> e_skip ~loc ()
        | _ :: _ ->
          let assigns = List.fold_right
            (fun hd s -> e_constant C_SET_ADD [hd ; s])
            inj (e_accessor ~loc (e_variable (Var.of_name name)) access_path) in
          e_assign ~loc name access_path assigns in
      return_statement @@ expr
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
      return_statement @@ e_assign ~loc varname path expr
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
      return_statement @@ e_assign ~loc varname path expr
    )

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

and simpl_cases : type a . (Raw.pattern * a) list -> (a, unit) matching result = fun t ->
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
            (** TODO: The labelled arguments should be flowing from the CLI. *)
            let content () =
              Printf.sprintf "Pattern : %s"
                (Parser.Pascaligo.ParserLog.pattern_to_string
                   ~offsets:true ~mode:`Point x) in
            error title content in
          let%bind x' =
            trace error @@
            get_constr x in
          ok (x' , y) in
        bind_map_list aux lst in
      ok @@ ez_match_variant constrs

and simpl_instruction : Raw.instruction -> (_ -> expression result) result =
  fun t ->
  trace (simplifying_instruction t) @@ simpl_single_instruction t

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

and simpl_for_int : Raw.for_int -> (_ -> expression result) result = fun fi ->
  (* cond part *)
  let var = e_variable (Var.of_name fi.assign.value.name.value) in
  let%bind value = simpl_expression fi.assign.value.expr in
  let%bind bound = simpl_expression fi.bound in
  let comp = e_annotation (e_constant C_LE [var ; bound]) t_bool
  in
  (* body part *)
  let%bind body = simpl_block fi.block.value in
  let%bind body = body None in
  let step = e_int 1 in
  let ctrl = e_assign
    fi.assign.value.name.value [] (e_constant C_ADD [ var ; step ]) in
  let rec add_to_seq expr = match expr.expression with
    | E_sequence (_,a) -> add_to_seq a
    | _ -> e_sequence body ctrl in
  let body' = add_to_seq body in
  let loop = e_loop comp body' in
  return_statement @@ e_let_in (Var.of_name fi.assign.value.name.value, Some t_int) value loop

(** simpl_for_collect
  For loops over collections, like

  ``` concrete syntax :
  for x : int in set myset
  begin
    myint := myint + x ;
    myst := myst ^ "to" ;
  end
  ```

  are implemented using a MAP_FOLD, LIST_FOLD or SET_FOLD:

  ``` pseudo Ast_simplified
  let #COMPILER#folded_record = list_fold(  mylist ,
                                  record st = st; acc = acc; end;
                                  lamby = fun arguments -> (
                                      let #COMPILER#acc = arguments.0 in
                                      let #COMPILER#elt_x = arguments.1 in
                                      #COMPILER#acc.myint := #COMPILER#acc.myint + #COMPILER#elt_x ;
                                      #COMPILER#acc.myst  := #COMPILER#acc.myst ^ "to" ;
                                      #COMPILER#acc
                                  )
                                ) in
  {
    myst  := #COMPILER#folded_record.myst ;
    myint := #COMPILER#folded_record.myint ;
  }
  ```

  We are performing the following steps:
    1) Simplifying the for body using ̀simpl_block`

    2) Detect the free variables and build a list of their names
       (myint and myst in the previous example)
       Free variables are simply variables being assigned but not defined
       locally.
       Note:  In the case of a nested loops, assignements to a compiler
              generated value (#COMPILER#acc) correspond to variables
              that were already renamed in the inner loop.
              e.g :
              ```
              #COMPILER#acc.myint := #COMPILER#acc.myint + #COMPILER#elt_x ;
              #COMPILER#acc.myst  := #COMPILER#acc.myst ^ "to" ;
              ```
              They must not be considered as free variables

    3) Build the initial record (later passed as 2nd argument of
      `MAP/SET/LIST_FOLD`) capturing the environment using the
      free variables list of (2)

    4) In the filtered body of (1), replace occurences:
        - free variable of name X as rhs ==> accessor `#COMPILER#acc.X`
        - free variable of name X as lhs ==> accessor `#COMPILER#acc.X`
       And, in the case of a map:
            - references to the iterated key   ==> variable `#COMPILER#elt_K`
            - references to the iterated value ==> variable `#COMPILER#elt_V`
            in the case of a set/list:
            - references to the iterated value ==> variable `#COMPILER#elt_X`
       Note: In the case of an inner loop capturing variable from an outer loop
             the free variable name can be `#COMPILER#acc.Y` and because we do not
             capture the accumulator record in the inner loop, we don't want to
             generate `#COMPILER#acc.#COMPILER#acc.Y` but `#COMPILER#acc.Y`

    5) Append the return value to the body

    6) Prepend the declaration of the lambda arguments to the body which
       is a serie of `let .. in`'s
       Note that the parameter of the lambda ̀arguments` is a tree of
       tuple holding:
        * In the case of `list` or ̀set`:
          ( folding record , current list/set element ) as
          ( #COMPILER#acc  , #COMPILER#elt_X          )
        * In the case of `map`:
          ( folding record , current map key ,   current map value   ) as
          ( #COMPILER#acc  , #COMPILER#elt_K ,   #COMPILER#elt_V     )
       Note: X , K and V above have to be replaced with their given name

    7) Build the lambda using the final body of (6)

    8) Build a sequence of assignments for all the captured variables
       to their new value, namely an access to the folded record
       (#COMPILER#folded_record)

    9) Attach the sequence of 8 to the ̀let .. in` declaration
       of #COMPILER#folded_record

**)
and simpl_for_collect : Raw.for_collect -> (_ -> expression result) result = fun fc ->
  let elt_name = "#COMPILER#elt_"^fc.var.value in
  let elt_v_name = match fc.bind_to with
    | Some v -> "#COMPILER#elt_"^(snd v).value
    | None -> "#COMPILER#elt_unused" in
  let element_names = ok @@ match fc.bind_to with
    | Some v -> [Var.of_name fc.var.value;Var.of_name (snd v).value]
    | None -> [Var.of_name fc.var.value] in
  (* STEP 1 *)
  let%bind for_body = simpl_block fc.block.value in
  let%bind for_body = for_body None in
  (* STEP 2 *)
  let%bind local_decl_name_list = bind_concat (detect_local_declarations for_body) element_names in
  let%bind captured_name_list = detect_free_variables for_body local_decl_name_list in
  (* STEP 3 *)
  let add_to_record (prev: expression SMap.t) (captured_name: string) =
    SMap.add captured_name (e_variable (Var.of_name captured_name)) prev in
  let init_record = e_record (List.fold_left add_to_record SMap.empty captured_name_list) in
  (* STEP 4 *)
  let replace exp =
    match exp.expression with
    (* replace references to fold accumulator as lhs *)
    | E_assign ( name , path , expr ) -> (
      if (List.mem name local_decl_name_list ) then
        ok @@ exp
      else
      let name = Var.to_name name in
        let path' = List.filter
          ( fun el ->
            match el with
            | Access_record name -> not @@ is_compiler_generated (Var.of_name name)
            | _ -> true )
          ((Access_record name)::path) in
        ok @@ e_assign "#COMPILER#acc" path' expr )
    | E_variable name -> (
      let name = Var.to_name name in
      if (List.mem name captured_name_list) then
        (* replace references to fold accumulator as rhs *)
        ok @@ e_accessor (e_variable (Var.of_name "#COMPILER#acc")) [Access_record name] (* TODO fresh *)
      else match fc.collection with
      (* loop on map *)
      | Map _ ->
        let k' = e_variable (Var.of_name elt_name) in
        if ( name = fc.var.value ) then
          ok @@ k' (* replace references to the the key *)
        else (
          match fc.bind_to with
          | Some (_,v) ->
            let v' = e_variable (Var.of_name elt_v_name) in
            if ( name = v.value ) then
              ok @@ v' (* replace references to the the value *)
            else ok @@ exp
          | None -> ok @@ exp
      )
      (* loop on set or list *)
      | (Set _ | List _) ->
        if (name = fc.var.value ) then
          (* replace references to the collection element *)
          ok @@ (e_variable (Var.of_name elt_name))
        else ok @@ exp
    )
    | _ -> ok @@ exp in
  let%bind for_body = Self_ast_simplified.map_expression replace for_body in
  (* STEP 5 *)
  let rec add_return (expr : expression) = match expr.expression with
    | E_sequence (a,b) -> e_sequence a (add_return b)
    | _  -> e_sequence expr (e_variable (Var.of_name "#COMPILER#acc")) in (* TODO fresh *)
  let for_body = add_return for_body in
  (* STEP 6 *)
  let for_body =
    let ( arg_access: Types.access_path -> expression ) = e_accessor (e_variable (Var.of_name "arguments")) in (* TODO fresh *)
    ( match fc.collection with
      | Map _ ->
        let acc          = arg_access [Access_tuple 0 ] in
        let collec_elt_v = arg_access [Access_tuple 1 ; Access_tuple 0] in
        let collec_elt_k = arg_access [Access_tuple 1 ; Access_tuple 1] in
        e_let_in (Var.of_name "#COMPILER#acc", None) acc @@ (* TODO fresh *)
        e_let_in (Var.of_name elt_name, None) collec_elt_v @@
        e_let_in (Var.of_name elt_v_name, None) collec_elt_k (for_body)
      | _ ->
        let acc        = arg_access [Access_tuple 0] in
        let collec_elt = arg_access [Access_tuple 1] in
        e_let_in (Var.of_name "#COMPILER#acc", None) acc @@ (* TODO fresh *)
        e_let_in (Var.of_name elt_name, None) collec_elt (for_body)
    ) in
  (* STEP 7 *)
  let%bind collect = simpl_expression fc.expr in
  let lambda = e_lambda (Var.of_name "arguments") None None for_body in
  let op_name = match fc.collection with
   | Map _ -> C_MAP_FOLD | Set _ -> C_SET_FOLD | List _ -> C_LIST_FOLD in
  let fold = e_constant op_name [lambda; collect ; init_record] in
  (* STEP 8 *)
  let assign_back (prev : expression option) (captured_varname : string) : expression option =
    let access = e_accessor (e_variable (Var.of_name "#COMPILER#folded_record")) (* TODO fresh *)
      [Access_record captured_varname] in
    let assign = e_assign captured_varname [] access in
    match prev with
    | None -> Some assign
    | Some p -> Some (e_sequence p assign) in
  let reassign_sequence = List.fold_left assign_back None captured_name_list in
  (* STEP 9 *)
  let final_sequence = match reassign_sequence with
    (* None case means that no variables were captured *)
    | None -> e_skip ()
    | Some seq -> e_let_in (Var.of_name "#COMPILER#folded_record", None) fold seq in (* TODO fresh *)
  return_statement @@ final_sequence

let simpl_program : Raw.ast -> program result = fun t ->
  bind_list @@ List.map simpl_declaration @@ nseq_to_list t.decl
