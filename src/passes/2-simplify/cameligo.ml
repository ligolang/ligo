[@@@warning "-45"]

open Trace
open Ast_simplified

module Raw = Parser.Cameligo.AST
module SMap = Map.String
module Option = Simple_utils.Option
(* TODO: move 1-parser/shared/Utils.ml{i} to Simple_utils/ *)

open Combinators

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map snd tl)
let npseq_to_nelist (hd, tl) = hd, (List.map snd tl)
let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

module Errors = struct
  let wrong_pattern expected_name actual =
    let title () = "wrong pattern" in
    let message () =
      match actual with
      | Raw.PVar v -> v.value
      | Raw.PTuple _ -> "tuple"
      | Raw.PRecord _ -> "record"
      | Raw.PList _ -> "list"
      | Raw.PBytes _ -> "bytes"
      | _ -> "other"
    in
    let data = [
      ("expected", fun () -> expected_name);
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@
                Raw.pattern_to_region actual)]
    in error ~data title message

  let unsupported_let_in_function (patterns : Raw.pattern list) =
    let title () = "" in
    let message () = "\nDefining functions with \"let ... in\" \
                      is not supported yet.\n" in
    let patterns_loc =
      List.fold_left (fun a p -> Region.cover a (Raw.pattern_to_region p))
        Region.ghost patterns in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ patterns_loc)]
    in error ~data title message

  let unknown_predefined_type name =
    let title () = "Type constants" in
    let message () =
      Format.asprintf "Unknown predefined type \"%s\".\n"
                      name.Region.value in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ name.Region.region)]
    in error ~data title message

  let untyped_fun_param var =
    let title () = "" in
    let message () =
      Format.asprintf "\nUntyped function parameters \
                       are not supported yet.\n" in
    let param_loc = var.Region.region in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ param_loc)]
    in error ~data title message

  let unsupported_tuple_pattern p =
    let title () = "" in
    let message () =
      Format.asprintf "\nTuple patterns are not supported yet.\n" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_cst_constr p =
    let title () = "" in
    let message () =
      Format.asprintf "\nConstant constructors are not supported yet.\n" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)]
    in error ~data title message

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

  let simplifying_expr t =
    let title () = "Simplifying expression" in
    let message () = "" in
    let data = [
        ("expression" ,
         (** TODO: The labelled arguments should be flowing from the CLI. *)
       thunk @@ Parser.Cameligo.ParserLog.expr_to_string
                 ~offsets:true ~mode:`Point t)]
    in error ~data title message

  let only_constructors p =
    let title () = "" in
    let message () =
      Format.asprintf "\nCurrently, only constructors are \
                       supported in patterns.\n" in
    let pattern_loc = Raw.pattern_to_region p in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ pattern_loc)
    ] in
    error ~data title message

  let unsupported_sugared_lists region =
    let title () = "" in
    let message () =
      Format.asprintf "\nCurrently, only empty lists and \
                       constructors (::) \
                       are supported in patterns.\n" in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
    in error ~data title message

  let corner_case description =
    let title () = "Corner case" in
    let message () = description in
    error title message

end

open Errors

open Operators.Simplify.Cameligo

let r_split = Location.r_split

let rec pattern_to_var : Raw.pattern -> _ = fun p ->
  match p with
  | Raw.PPar p -> pattern_to_var p.value.inside
  | Raw.PVar v -> ok v
  | Raw.PWild r -> ok @@ ({ region = r ; value = "_" } : Raw.variable)
  | _ -> fail @@ wrong_pattern "single var" p

let rec tuple_pattern_to_vars : Raw.pattern -> _ = fun pattern ->
  match pattern with
  | Raw.PPar pp -> tuple_pattern_to_vars pp.value.inside
  | Raw.PTuple pt -> bind_map_list pattern_to_var (npseq_to_list pt.value)
  | Raw.PVar _ | Raw.PWild _-> bind_list [pattern_to_var pattern]
  | other -> (fail @@ wrong_pattern "parenthetical, tuple, or variable" other)


let rec pattern_to_typed_var : Raw.pattern -> _ = fun p ->
  match p with
  | Raw.PPar p -> pattern_to_typed_var p.value.inside
  | Raw.PTyped tp -> (
      let tp = tp.value in
      let%bind v = pattern_to_var tp.pattern in
      ok (v , Some tp.type_expr)
    )
  | Raw.PVar v -> ok (v , None)
  | Raw.PWild r -> ok (({ region = r ; value = "_" } : Raw.variable) , None)
  | _ -> fail @@ wrong_pattern "single typed variable" p

let rec expr_to_typed_expr : Raw.expr -> _ = function
  EPar e -> expr_to_typed_expr e.value.inside
| EAnnot {value={inside=e,_,t; _}; _} -> ok (e, Some t)
| e -> ok (e , None)

let rec tuple_pattern_to_typed_vars : Raw.pattern -> _ = fun pattern ->
  match pattern with
  | Raw.PPar pp -> tuple_pattern_to_typed_vars pp.value.inside
  | Raw.PTuple pt -> bind_map_list pattern_to_typed_var (npseq_to_list pt.value)
  | Raw.PVar _ -> bind_list [pattern_to_typed_var pattern]
  | other -> (fail @@ wrong_pattern "parenthetical, tuple, or variable" other)

let rec typed_pattern_to_typed_vars : Raw.pattern -> _ = fun pattern ->
  match pattern with
  | Raw.PPar pp -> typed_pattern_to_typed_vars pp.value.inside
  | Raw.PTyped pt ->
     let (p,t) = pt.value.pattern,pt.value.type_expr in
     let%bind p = tuple_pattern_to_vars p in 
     let%bind t = simpl_type_expression t in
     ok @@ (p,t)
  | other -> (fail @@ wrong_pattern "parenthetical or type annotation" other)

and unpar_pattern : Raw.pattern -> Raw.pattern = function
  | PPar p -> unpar_pattern p.value.inside
  | _ as p -> p

and simpl_type_expression : Raw.type_expr -> type_expression result = fun te ->
  trace (simple_info "simplifying this type expression...") @@
  match te with
    TPar x -> simpl_type_expression x.value.inside
  | TVar v -> (
      match type_constants v.value with
      | Ok (s,_) -> ok @@ make_t @@ T_constant s
      | Error _ -> ok @@ make_t @@ T_variable (Var.of_name v.value)
    )
  | TFun x -> (
      let%bind (type1 , type2) =
        let (a , _ , b) = x.value in
        let%bind a = simpl_type_expression a in
        let%bind b = simpl_type_expression b in
        ok (a , b)
      in
      ok @@ make_t @@ T_arrow {type1;type2}
    )
  | TApp x -> (
      let (name, tuple) = x.value in
      let lst = npseq_to_list tuple.value.inside in
      let%bind lst' = bind_map_list simpl_type_expression lst in
      let%bind cst =
        trace (unknown_predefined_type name) @@
        type_operators name.value in
      t_operator cst lst'
    )
  | TProd p -> (
      let%bind tpl = simpl_list_type_expression  @@ npseq_to_list p.value in
      ok tpl
    )
  | TRecord r ->
      let aux = fun (x, y) -> let%bind y = simpl_type_expression y in ok (x, y) in
      let apply (x:Raw.field_decl Raw.reg) =
        (x.value.field_name.value, x.value.field_type) in
      let%bind lst =
        bind_list
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
        ok (v.value.constr.value, te) in
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
      let%bind lst = bind_map_list simpl_type_expression lst in
      ok @@ t_tuple lst

let rec simpl_expression :
  Raw.expr -> expr result = fun t ->
  let return x = ok x in
  let simpl_projection = fun (p:Raw.projection Region.reg) ->
    let (p , loc) = r_split p in
    let var =
      let name = Var.of_name p.struct_name.value in
      e_variable name in
    let path = p.field_path in
    let path' =
      let aux (s:Raw.selection) =
        match s with
          FieldName property -> property.value
        | Component index -> Z.to_string (snd index.value)
      in
      List.map aux @@ npseq_to_list path in
    return @@ List.fold_left (e_accessor ~loc ) var path'
  in
  let simpl_path : Raw.path -> string * label list = fun p ->
    match p with
    | Raw.Name v -> (v.value , [])
    | Raw.Path p -> (
        let p' = p.value in
        let var = p'.struct_name.value in
        let path = p'.field_path in
        let path' =
          let aux (s:Raw.selection) =
            match s with
            | FieldName property -> Label property.value
            | Component index -> Label (Z.to_string (snd index.value))
          in
          List.map aux @@ npseq_to_list path in
        (var , path')
      )
  in
  let simpl_update = fun (u:Raw.update Region.reg) ->
    let (u, loc) = r_split u in
    let (name, path) = simpl_path u.record in
    let record = match path with
    | [] -> e_variable (Var.of_name name)
    | _ ->
      let aux expr (Label l) = e_accessor expr l in
      List.fold_left aux (e_variable (Var.of_name name)) path in 
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
  in

  trace (simplifying_expr t) @@
  match t with
    Raw.ELetIn e ->
      let Raw.{binding; body; attributes; _} = e.value in
      let inline = List.exists (fun (a: Raw.attribute) -> a.value = "inline") attributes in
      let Raw.{binders; lhs_type; let_rhs; _} = binding in
      begin match binders with
      | (p, []) ->
      let%bind variables = tuple_pattern_to_typed_vars p in
      let%bind ty_opt =
        bind_map_option (fun (_,te) -> simpl_type_expression te) lhs_type in
      let%bind rhs = simpl_expression let_rhs in
      let rhs_b = Var.fresh ~name: "rhs" () in
      let rhs',rhs_b_expr =
        match ty_opt with
          None -> rhs, e_variable rhs_b
        | Some ty -> (e_annotation rhs ty), e_annotation (e_variable rhs_b) ty in
      let%bind body = simpl_expression body in
      let prepare_variable (ty_var: Raw.variable * Raw.type_expr option) =
        let variable, ty_opt = ty_var in
        let var_expr = Var.of_name variable.value in
        let%bind ty_expr_opt =
          match ty_opt with
          | Some ty -> bind_map_option simpl_type_expression (Some ty)
          | None -> ok None
        in ok (var_expr, ty_expr_opt)
      in
      let%bind prep_vars = bind_list (List.map prepare_variable variables) in
      let%bind () =
        if (List.length prep_vars) = 0
        then fail @@ corner_case "let ... in without variables passed parsing stage"
        else ok ()
      in
      let rhs_b_expr = (* We only want to evaluate the rhs first if multi-bind *)
        if List.length prep_vars = 1
        then rhs' else rhs_b_expr
      in
      let rec chain_let_in variables body : expression =
        match variables with
        | hd :: [] ->
          if (List.length prep_vars = 1)
          then e_let_in hd false inline rhs_b_expr body
          else e_let_in hd false inline (e_accessor rhs_b_expr (string_of_int ((List.length prep_vars) - 1))) body
        | hd :: tl ->
          e_let_in hd
          false
          inline
          (e_accessor rhs_b_expr (string_of_int ((List.length prep_vars) - (List.length tl) - 1)))
          (chain_let_in tl body)
        | [] -> body (* Precluded by corner case assertion above *)
      in
      if List.length prep_vars = 1
      then ok (chain_let_in prep_vars body)
      (* Bind the right hand side so we only evaluate it once *)
      else ok (e_let_in (rhs_b, ty_opt) false inline rhs' (chain_let_in prep_vars body))

      (* let f p1 ps... = rhs in body *)
      | (f, p1 :: ps) ->
        fail @@ unsupported_let_in_function (f :: p1 :: ps)
      end
  | Raw.EAnnot a ->
      let Raw.{inside=expr, _, type_expr; _}, loc = r_split a in
      let%bind expr' = simpl_expression expr in
      let%bind type_expr' = simpl_type_expression type_expr in
      return @@ e_annotation ~loc expr' type_expr'
  | EVar c ->
      let (c',loc) = r_split c in
      (match constants c' with
       | Error _  -> return @@ e_variable ~loc (Var.of_name c.value)
       | Ok (s,_) -> return @@ e_constant s [])
  | ECall x -> (
      let ((e1 , e2) , loc) = r_split x in
      let%bind args = bind_map_list simpl_expression (nseq_to_list e2) in
      let rec chain_application (f: expression) (args: expression list) =
        match args with
        | hd :: tl ->  chain_application (e_application ~loc f hd) tl
        | [] -> f
      in
      match e1 with
      | EVar f -> (
          let (f , f_loc) = r_split f in
          match constants f with
          | Error _ -> return @@ chain_application (e_variable ~loc:f_loc (Var.of_name f)) args
          | Ok (s, _) -> return @@ e_constant ~loc s args
              )
      | e1 ->
          let%bind e1' = simpl_expression e1 in
          return @@ chain_application e1' args
  )
  | EPar x -> simpl_expression x.value.inside
  | EUnit reg ->
      let (_ , loc) = r_split reg in
      return @@ e_literal ~loc Literal_unit
  | EBytes x ->
      let (x , loc) = r_split x in
      return @@ e_literal ~loc (Literal_bytes (Hex.to_bytes @@ snd x))
  | ETuple tpl -> simpl_tuple_expression @@ (npseq_to_list tpl.value)
  | ERecord r ->
      let (r , loc) = r_split r in
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = simpl_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
        @@ npseq_to_list r.ne_elements in
      return @@ e_record_ez ~loc fields
  | EProj p -> simpl_projection p
  | EUpdate u -> simpl_update u
  | EConstr (ESomeApp a) ->
      let (_, args), loc = r_split a in
      let%bind arg = simpl_expression args in
      return @@ e_constant ~loc C_SOME [arg]
  | EConstr (ENone reg) ->
      let loc = Location.lift reg in
      return @@ e_none ~loc ()
  | EConstr (EConstrApp c) ->
      let (c_name, args), loc = r_split c in
      let c_name, _c_loc = r_split c_name in
      let args =
        match args with
          None -> []
        | Some arg -> [arg] in
      let%bind arg = simpl_tuple_expression @@ args
      in return @@ e_constructor ~loc c_name arg
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
  | EArith (Mutez n) -> (
      let (n , loc) = r_split n in
      let n = Z.to_int @@ snd @@ n in
      return @@ e_literal ~loc (Literal_mutez n)
    )
  | EArith (Neg e) -> simpl_unop "NEG" e
  | EString (String s) -> (
      let (s , loc) = r_split s in
      let s' =
        let s = s in
        String.(sub s 1 ((length s) - 2))
      in
      return @@ e_literal ~loc (Literal_string s')
    )
  | EString (Cat c) ->
    let (c, loc) = r_split c in
    let%bind string_left = simpl_expression c.arg1 in
    let%bind string_right = simpl_expression c.arg2 in
    return @@ e_string_cat ~loc string_left string_right
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
      let default_action () =
        let%bind cases = simpl_cases lst in
        return @@ e_matching ~loc e  cases in
      (* Hack to take care of patterns introduced by `parser/cameligo/Parser.mly` in "norm_fun_expr". TODO: Still needed? *)
      match lst with
      | [ (pattern , rhs) ] -> (
          match pattern with
          | Raw.PPar p -> (
              let p' = p.value.inside in
              match p' with
              | Raw.PTyped x -> (
                  let x' = x.value in
                  match x'.pattern with
                  | Raw.PVar y ->
                    let var_name = Var.of_name y.value in
                    let%bind type_expr = simpl_type_expression x'.type_expr in
                    return @@ e_let_in (var_name , Some type_expr) false false e rhs
                  | _ -> default_action ()
                )
              | _ -> default_action ()
            )
          | _ -> default_action ()
        )
      | _ -> default_action ()
    )
  | EFun lamb -> simpl_fun lamb
  | ESeq s -> (
      let (s , loc) = r_split s in
      let items : Raw.expr list = pseq_to_list s.elements in
      (match items with
         [] -> return @@ e_skip ~loc ()
       | expr::more ->
          let expr' = simpl_expression expr in
          let apply (e1: Raw.expr) (e2: expression Trace.result) =
            let%bind a = simpl_expression e1 in
            let%bind e2' = e2 in
            return @@ e_sequence a e2'
          in List.fold_right apply more expr')
    )
  | ECond c -> (
      let (c , loc) = r_split c in
      let%bind expr = simpl_expression c.test in
      let%bind match_true = simpl_expression c.ifso in
      let%bind match_false = simpl_expression c.ifnot in
      return @@ e_matching ~loc expr (Match_bool {match_true; match_false})
    )

and simpl_fun lamb' : expr result =
  let return x = ok x in
  let (lamb , loc) = r_split lamb' in
  let%bind params' =
    let params = nseq_to_list lamb.binders in
    let params = (* Handle case where we have tuple destructure in params *)
      (* So basically the transformation we're doing is:

         let sum (result, i: int * int) : int = result + i

         TO:

         let sum (#P: int * int) : int =
           let result, i = #P in result + i

         In this first section we replace `result, i` with `#P`. *)
      match lamb.binders with
      (* TODO: currently works only if there is one param *)
      | (Raw.PPar pp, []) ->
        let pt = pp.value.inside in
        (match pt with
        | Raw.PTyped pt ->
          begin
          let pt_pattern = unpar_pattern pt.value.pattern in
          match pt_pattern with
          | Raw.PVar _ -> params
          | Raw.PTuple _ ->
            [Raw.PTyped
               {region=Region.ghost;
                value=
                  { pt.value with pattern=
                                    Raw.PVar {region=Region.ghost;
                                              value="#P"}}}]
          | _ -> params
        end
        | _ -> params)
      | _ -> params
    in
    let%bind p_params = bind_map_list pattern_to_typed_var params in
    let aux ((var : Raw.variable) , ty_opt) =
      match var.value , ty_opt with
      | "storage" , None ->
        ok (var , t_variable "storage")
      | _ , None ->
          fail @@ untyped_fun_param var
      | _ , Some ty -> (
        let%bind ty' = simpl_type_expression ty in
        ok (var , ty')
      )
    in
    bind_map_list aux p_params
  in
  let%bind body =
    if (List.length params' > 1) then ok lamb.body
    else
    let original_params = nseq_to_list lamb.binders in
    let%bind destruct =
      match original_params with
      | hd :: _ -> ok @@ hd
      | [] -> fail @@ corner_case "Somehow have no parameters in function during tuple param destructure"
    in
    match destruct with (* Handle tuple parameter destructuring *)
    (* In this section we create a let ... in that binds the original parameters *)
    | Raw.PPar pp ->
      (match unpar_pattern pp.value.inside with
       | Raw.PTyped pt ->
         let vars = pt.value in
         (match unpar_pattern vars.pattern with
          | PTuple vars ->
            let let_in_binding: Raw.let_binding =
              {binders = (PTuple vars, []) ;
               lhs_type=None;
               eq=Region.ghost;
               let_rhs=(Raw.EVar {region=Region.ghost; value="#P"});
              }
            in
            let let_in: Raw.let_in =
              {kwd_let= Region.ghost;
               binding= let_in_binding;
               kwd_in= Region.ghost;
               body= lamb.body;
               attributes = []
              }
            in
            ok (Raw.ELetIn
                  {
                    region=Region.ghost;
                    value=let_in
                  })
          | Raw.PVar _ -> ok lamb.body
          | _ ->  ok lamb.body)
       | _ ->  ok lamb.body)
    | _ ->  ok lamb.body
  in
  let%bind (body , body_type) = expr_to_typed_expr body in
  let%bind output_type =
    bind_map_option simpl_type_expression body_type in
  let%bind body = simpl_expression body in
  let rec layer_arguments (arguments: (Raw.variable * type_expression) list) =
    match arguments with
    | hd :: tl ->
      let (binder , input_type) =
        (Var.of_name (fst hd).value , snd hd) in
      e_lambda ~loc (binder) (Some input_type) output_type (layer_arguments tl)
    | [] -> body
  in
  return @@ layer_arguments params'


and simpl_logic_expression ?te_annot (t:Raw.logic_expr) : expr result =
  let return x = ok @@ make_option_typed x te_annot in
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
    ECons c -> simpl_binop "CONS" c
  | EListComp lst -> (
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
  let%bind name = constants name in
  return @@ e_constant ~loc name [ a ; b ]

and simpl_unop (name:string) (t:_ Raw.un_op Region.reg) : expression result =
  let return x = ok @@ x in
  let (t , loc) = r_split t in
  let%bind a = simpl_expression t.arg in
  let%bind name = constants name in
  return @@ e_constant ~loc name [ a ]

and simpl_tuple_expression ?loc (lst:Raw.expr list) : expression result =
  let return x = ok @@ x in
  match lst with
  | [] -> return @@ e_literal ?loc Literal_unit
  | [hd] -> simpl_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map simpl_expression lst in
      return @@ e_tuple ?loc lst

and simpl_declaration : Raw.declaration -> declaration Location.wrap list result =
  fun t ->
  let open! Raw in
  let loc : 'a . 'a Raw.reg -> _ -> _ =
    fun x v -> Location.wrap ~loc:(File x.region) v in
  match t with
  | TypeDecl x ->
      let {name;type_expr} : Raw.type_decl = x.value in
      let%bind type_expression = simpl_type_expression type_expr in
      ok @@ [loc x @@ Declaration_type (Var.of_name name.value , type_expression)]
  | Let x -> (
      let (_, let_binding, attributes), _ = r_split x in
      let inline = List.exists (fun (a: Raw.attribute) -> a.value = "inline") attributes in
      let binding = let_binding in
      let {binders; lhs_type; let_rhs} = binding in
      let%bind (hd, _) =
        let (hd, tl) = binders in ok (hd, tl) in
        match hd with
        | PTuple pt ->
          let process_variable (var_pair: pattern * Raw.expr) :
            Ast_simplified.declaration Location.wrap result =
           (let (par_var, rhs_expr) = var_pair in
            let%bind (v, v_type) = pattern_to_typed_var par_var in
            let%bind v_type_expression =
              match v_type with
              | Some v_type -> ok (to_option (simpl_type_expression v_type))
              | None -> ok None
            in
            let%bind simpl_rhs_expr = simpl_expression rhs_expr in
              ok @@ loc x @@ Declaration_constant (Var.of_name v.value, v_type_expression, inline, simpl_rhs_expr) )
          in let%bind variables = ok @@ npseq_to_list pt.value
          in let%bind expr_bind_lst =
               match let_rhs with
               | ETuple et -> ok @@ npseq_to_list et.value
               | EVar v -> (* Handle variable bound to tuple *)
                 let name = v.value in
                 let rec gen_access_tuple ?(i: int = 0)
                     ?(accesses: Raw.expr list = []) (name: string)
                   : Raw.expr list =
                   let build_access_expr : Raw.expr = EProj
                     {region = v.region;
                      value =
                        { struct_name = v;
                          selector = Region.ghost ;
                          field_path =
                            (
                              (Component
                                 {region = Region.ghost;
                                  value = name, Z.of_int i;} : Raw.selection)
                              , []);
                        }
                     }
                   in
                   if i = (List.length variables) then accesses
                   else
                     let accesses =
                       build_access_expr :: accesses
                     in
                     gen_access_tuple name ~i: (i + 1) ~accesses
                 in ok (gen_access_tuple name)
               (* TODO: Improve this error message *)
               | other -> fail @@ simplifying_expr other
          in let%bind decls =
               (* TODO: Rewrite the gen_access_tuple so there's no List.rev *)
               bind_map_list process_variable (List.combine variables (List.rev expr_bind_lst))
          in ok @@ decls
        | PPar {region = _ ; value = { lpar = _ ; inside = pt; rpar = _; } } ->
          (* Extract parenthetical multi-bind *)
          let (wild, _, attributes) = fst @@ r_split x in
          simpl_declaration
            (Let {
                region = x.region;
                value = (wild, {binders = (pt, []);
                                lhs_type = lhs_type;
                                eq = Region.ghost ;
                                let_rhs = let_rhs}, attributes)}
            : Raw.declaration)
        | _ ->
      let%bind (var, args) =
        let%bind (hd, tl) =
          let hd, tl = binders in ok (hd, tl) in
        let%bind var = pattern_to_var hd in
        ok (var , tl)
      in
      let%bind lhs_type' = bind_map_option (fun x -> simpl_type_expression (snd x)) lhs_type in
      let let_rhs = match args with
      | [] -> let_rhs
      | param1::others ->
          let fun_ = {
            kwd_fun = Region.ghost;
            binders = param1, others;
            lhs_type;
            arrow   = Region.ghost;
            body    = let_rhs
          } in
          Raw.EFun {region=Region.ghost ; value=fun_} 
      in
      let f_args = (match let_rhs with 
      | Raw.EFun f -> nseq_to_list f.value.binders
      | _ -> []
      )
      in
      let%bind rhs' = simpl_expression let_rhs in
      let%bind ty = bind_map_list typed_pattern_to_typed_vars f_args in
      let aux acc ty = Option.map (t_function (snd ty)) acc in
      let func_type = List.fold_right' aux lhs_type' ty in
      ok @@ [loc x @@ (Declaration_constant (Var.of_name var.value , func_type , inline, rhs'))]
    )

and simpl_cases : type a . (Raw.pattern * a) list -> (a, unit) matching_content result =
  fun t ->
  let open Raw in
  let rec get_var (t:Raw.pattern) =
    match t with
    | PVar v -> ok v.value
    | PPar p -> get_var p.value.inside
    | _ -> fail @@ unsupported_non_var_pattern t in
  let rec get_tuple (t:Raw.pattern) =
    match t with
    | PTuple v -> npseq_to_list v.value
    | PPar p -> get_tuple p.value.inside
    | x -> [ x ] in
  let get_single (t:Raw.pattern) =
    let t' = get_tuple t in
    let%bind () =
      trace_strong (unsupported_tuple_pattern t) @@
      Assert.assert_list_size t' 1 in
    ok (List.hd t') in
  let rec get_constr (t:Raw.pattern) =
    match t with
      PPar p -> get_constr p.value.inside
    | PConstr v ->
       let const, pat_opt =
         match v with
           PConstrApp {value; _} ->
           (match value with
           | constr, None ->
              constr, Some (PVar {value = "unit"; region = Region.ghost})
           | _ -> value)
         | PSomeApp {value=region,pat; _} ->
            {value="Some"; region}, Some pat
         | PNone region ->
            {value="None"; region}, None in
        let%bind pat =
          trace_option (unsupported_cst_constr t) @@ pat_opt in
        let%bind single_pat = get_single pat in
        let%bind var = get_var single_pat in
        ok (const.value, var)
    | _ -> fail @@ only_constructors t in
  let rec get_constr_opt (t:Raw.pattern) =
    match t with
      PPar p -> get_constr_opt p.value.inside
    | PConstr v ->
       let const, pat_opt =
         match v with
           PConstrApp {value; _} -> value
         | PSomeApp {value=region,pat; _} ->
            {value="Some"; region}, Some pat
         | PNone region ->
            {value="None"; region}, None in
        let%bind var_opt =
          match pat_opt with
          | None -> ok None
          | Some pat ->
              let%bind single_pat = get_single pat in
              let%bind var = get_var single_pat in
              ok (Some var)
        in ok (const.value , var_opt)
    | _ -> fail @@ only_constructors t in
  let%bind patterns =
    let aux (x , y) =
      let xs = get_tuple x in
      trace_strong (unsupported_tuple_pattern x) @@
      Assert.assert_list_size xs 1 >>? fun () ->
      ok (List.hd xs , y)
    in
    bind_map_list aux t in
  match patterns with
  | [(PFalse _, f) ; (PTrue _, t)]
  | [(PTrue _, t) ; (PFalse _, f)] ->
      ok @@ Match_bool {match_true = t ; match_false = f}
  | [(PList (PCons c), cons); (PList (PListComp sugar_nil), nil)]
  | [(PList (PListComp sugar_nil), nil); (PList (PCons c), cons)] ->
      let%bind () =
        trace_strong (unsupported_sugared_lists sugar_nil.region)
        @@ Assert.assert_list_empty
        @@ pseq_to_list
        @@ sugar_nil.value.elements in
      let%bind (a, b) =
        let a, _, b = c.value in
        let%bind a = get_var a in
        let%bind b = get_var b in
        ok (a, b) in
      ok @@ Match_list {match_cons=(Var.of_name a, Var.of_name b, cons, ()); match_nil=nil}
  | lst ->
      let error x =
        let title () = "Pattern" in
         (** TODO: The labelled arguments should be flowing from the CLI. *)
        let content () =
          Printf.sprintf "Pattern : %s"
                         (Parser.Cameligo.ParserLog.pattern_to_string
                            ~offsets:true ~mode:`Point x) in
        error title content
      in
      let as_variant () =
        trace (simple_info "currently, only booleans, lists, options, and constructors \
                            are supported in patterns") @@
        let%bind constrs =
          let aux (x, y) =
            let%bind x' = trace (error x) @@ get_constr x
            in ok (x', y)
          in bind_map_list aux lst
        in ok @@ ez_match_variant constrs in
      let as_option () =
        let aux (x, y) =
          let%bind x' = trace (error x) @@ get_constr_opt x
          in ok (x', y) in
        let%bind constrs = bind_map_list aux lst in
        match constrs with
        | [ (("Some", Some some_var), some_expr);
            (("None" , None) , none_expr) ]
        | [ (("None", None), none_expr);
            (("Some", Some some_var), some_expr) ] ->
           ok @@ Match_option {
                    match_some = (Var.of_name some_var, some_expr, ());
                    match_none = none_expr }
        | _ -> simple_fail "bad option pattern"
      in bind_or (as_option () , as_variant ())

let simpl_program : Raw.ast -> program result = fun t ->
  let%bind decls = bind_map_list simpl_declaration @@ nseq_to_list t.decl in
  ok @@ List.concat @@ decls
