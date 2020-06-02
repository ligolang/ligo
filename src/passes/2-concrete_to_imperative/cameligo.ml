[@@@warning "-45"]

open Trace
open Ast_imperative

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

  let unsupported_let_in_function (region : Region.t) (patterns : Raw.pattern list) =
    let title () = "" in
    let message () = "\nDefining functions with \"let ... in\" \
                      is not supported yet.\n" in
    let patterns_loc =
      List.fold_left (fun a p -> Region.cover a (Raw.pattern_to_region p))
        region patterns in
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

  let untyped_recursive_function var =
    let title () = "" in
    let message () =
      Format.asprintf "\nUntyped recursive functions \
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

  let abstracting_expr t =
    let title () = "abstracting expression" in
    let message () = "" in
    let data = [
        ("expression" ,
         (** TODO: The labelled arguments should be flowing from the CLI. *)
       thunk @@ Parser_cameligo.ParserLog.expr_to_string
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

  let unknown_built_in name =
    let title () = "\n Unknown built-in function" in
    let message () = "" in
    let data = [
      ("built-in", fun  () -> name);
    ] in
    error ~data title message

end

open Errors

open Operators.Concrete_to_imperative.Cameligo

let r_split = Location.r_split

let get_t_string_singleton_opt = function
  | Raw.TString s -> Some s.value
  | _ -> None

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
     let%bind t = compile_type_expression t in
     let l = Location.lift pt.region in
     ok @@ (p,t,l)
  | other -> (fail @@ wrong_pattern "parenthetical or type annotation" other)

and unpar_pattern : Raw.pattern -> Raw.pattern = function
  | PPar p -> unpar_pattern p.value.inside
  | _ as p -> p

and compile_type_expression : Raw.type_expr -> type_expression result = fun te ->
  trace (simple_info "abstracting this type expression...") @@
  match te with
    TPar x -> compile_type_expression x.value.inside
  | TVar v -> (
      let (v, loc) = r_split v in
      match type_constants v with
      | Some s -> ok @@ make_t ~loc @@ T_constant s
      | None   -> ok @@ make_t ~loc @@ T_variable (Var.of_name v)
    )
  | TFun x -> (
      let (x,loc) = r_split x in
      let%bind (type1 , type2) =
        let (a , _ , b) = x in
        let%bind a = compile_type_expression a in
        let%bind b = compile_type_expression b in
        ok (a , b)
      in
      ok @@ make_t ~loc @@ T_arrow {type1;type2}
    )
  | TApp x -> (
      let (x,loc) = r_split x in
      let (name, tuple) = x in
      ( match name.value with
        | "michelson_or" ->
          let lst = npseq_to_list tuple.value.inside in
          (match lst with
          | [a ; b ; c ; d ] -> (
            let%bind b' =
              trace_option (simple_error "second argument of michelson_or must be a string singleton") @@
                get_t_string_singleton_opt b in
            let%bind d' =
              trace_option (simple_error "fourth argument of michelson_or must be a string singleton") @@
                get_t_string_singleton_opt d in
            let%bind a' = compile_type_expression a in
            let%bind c' = compile_type_expression c in
            ok @@ t_michelson_or ~loc a' b' c' d'
            )
          | _ -> simple_fail "michelson_or does not have the right number of argument")
        | "michelson_pair" ->
          let lst = npseq_to_list tuple.value.inside in
          (match lst with
          | [a ; b ; c ; d ] -> (
            let%bind b' =
              trace_option (simple_error "second argument of michelson_pair must be a string singleton") @@
                get_t_string_singleton_opt b in
            let%bind d' =
              trace_option (simple_error "fourth argument of michelson_pair must be a string singleton") @@
                get_t_string_singleton_opt d in
            let%bind a' = compile_type_expression a in
            let%bind c' = compile_type_expression c in
            ok @@ t_michelson_pair ~loc a' b' c' d'
            )
          | _ -> simple_fail "michelson_pair does not have the right number of argument")
        | _ ->
          let lst = npseq_to_list tuple.value.inside in
          let%bind lst' = bind_map_list compile_type_expression lst in
          let%bind cst =
          trace_option (unknown_predefined_type name) @@
          type_operators name.value in
          ok @@ t_operator ~loc cst lst' )
    )
  | TProd p -> (
      let%bind tpl = compile_list_type_expression  @@ npseq_to_list p.value in
      ok tpl
    )
  | TRecord r ->
      let (r, loc) = r_split r in
      let aux = fun (x, y) -> let%bind y = compile_type_expression y in ok (x, y) in
      let order = fun i (x,y) -> ((x,i),y) in
      let apply (x:Raw.field_decl Raw.reg) =
        (x.value.field_name.value, x.value.field_type) in
      let%bind lst =
        bind_list
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
        ok ((v.value.constr.value,i), te) in
      let%bind lst = bind_list
        @@ List.mapi aux
        @@ npseq_to_list s in
      let m = List.fold_left (fun m ((x,i), y) -> CMap.add (Constructor x) {ctor_type=y;ctor_decl_pos=i} m) CMap.empty lst in
      ok @@ make_t ~loc @@ T_sum m
  | TString _s -> simple_fail "we don't support singleton string type"

and compile_list_type_expression (lst:Raw.type_expr list) : type_expression result =
  match lst with
  | [] -> ok @@ t_unit ()
  | [hd] -> compile_type_expression hd
  | lst ->
      let%bind lst = bind_map_list compile_type_expression lst in
      ok @@ t_tuple lst

let rec compile_expression :
  Raw.expr -> expr result = fun t ->
  let return x = ok x in
  let compile_projection = fun (p:Raw.projection Region.reg) ->
    let (p , loc) = r_split p in
    let var =
      let name = Var.of_name p.struct_name.value in
      e_variable ~loc name in
    let path = p.field_path in
    let path' =
      let aux (s:Raw.selection) =
        match s with
          FieldName property -> property.value
        | Component index -> Z.to_string (snd index.value)
      in
      List.map aux @@ npseq_to_list path in
    return @@ List.fold_left (e_record_accessor ~loc ) var path'
  in
  let compile_path : Raw.path -> string * label list = fun p ->
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
  let compile_update = fun (u:Raw.update Region.reg) ->
    let (u, loc) = r_split u in
    let (name, path) = compile_path u.record in
    let record = match path with
    | [] -> e_variable (Var.of_name name)
    | _ ->
      let aux expr (Label l) = e_record_accessor expr l in
      List.fold_left aux (e_variable (Var.of_name name)) path in
    let updates = u.updates.value.ne_elements in
    let%bind updates' =
      let aux (f:Raw.field_path_assign Raw.reg) =
        let (f,_) = r_split f in
        let%bind expr = compile_expression f.field_expr in
        ok ( List.map (fun (x: _ Raw.reg) -> x.value) (npseq_to_list f.field_path), expr)
      in
      bind_map_list aux @@ npseq_to_list updates
    in
    let aux ur (path, expr) =
      let rec aux record = function
        | [] -> failwith "error in parsing"
        | hd :: [] -> ok @@ e_record_update ~loc record hd expr
        | hd :: tl ->
          let%bind expr = (aux (e_record_accessor ~loc record hd) tl) in
          ok @@ e_record_update ~loc record hd expr
      in
      aux ur path in
    bind_fold_list aux record updates'
  in

  trace (abstracting_expr t) @@
  match t with
    Raw.ELetIn e ->
      let Raw.{kwd_rec; binding; body; attributes; _} = e.value in
      let region = e.region in
      let loc = Location.lift region in
      let inline = List.exists (fun (a: Raw.attribute) -> a.value = "inline") attributes in
      let Raw.{binders; lhs_type; let_rhs; _} = binding in
      begin match binders with
      | (p, []) ->
      let%bind variables = tuple_pattern_to_typed_vars p in
      let%bind ty_opt =
        bind_map_option (fun (re,te) -> let%bind te = compile_type_expression te in ok(Location.lift re,te)) lhs_type in
      let%bind rhs = compile_expression let_rhs in
      let rhs_b = Var.fresh ~name: "rhs" () in
      let rhs',rhs_b_expr =
        match ty_opt with
          None -> rhs, e_variable ~loc rhs_b
        | Some (lt,ty) -> (e_annotation ~loc:lt rhs ty), e_annotation ~loc:lt (e_variable ~loc rhs_b) ty in
      let%bind body = compile_expression body in
      let prepare_variable (ty_var: Raw.variable * Raw.type_expr option) =
        let variable, ty_opt = ty_var in
        let var_expr = Var.of_name variable.value in
        let%bind ty_expr_opt =
          match ty_opt with
          | Some ty -> bind_map_option compile_type_expression (Some ty)
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
          then e_let_in ~loc hd inline rhs_b_expr body
          else e_let_in ~loc hd inline (e_record_accessor ~loc rhs_b_expr (string_of_int ((List.length prep_vars) - 1))) body
        | hd :: tl ->
          e_let_in ~loc hd
          inline
          (e_record_accessor ~loc rhs_b_expr (string_of_int ((List.length prep_vars) - (List.length tl) - 1)))
          (chain_let_in tl body)
        | [] -> body (* Precluded by corner case assertion above *)
      in
      let%bind ty_opt = match ty_opt with
      | None -> (match let_rhs with
        | EFun {value={binders;lhs_type}} ->
          let f_args = nseq_to_list (binders) in
          let%bind lhs_type' = bind_map_option (fun x -> compile_type_expression (snd x)) lhs_type in
          let%bind ty = bind_map_list typed_pattern_to_typed_vars f_args in
          let aux acc (_,ty,loc) = Option.map (t_function ~loc ty) acc in
          ok @@ (List.fold_right' aux lhs_type' ty)
        | _ -> ok None
        )
      | Some (_,t) -> ok @@ Some t
      in
      let%bind ret_expr = if List.length prep_vars = 1
        then ok (chain_let_in prep_vars body)
        (* Bind the right hand side so we only evaluate it once *)
        else ok (e_let_in (rhs_b, ty_opt) inline rhs' (chain_let_in prep_vars body))
      in
      let%bind ret_expr = match kwd_rec with
        | None -> ok @@ ret_expr
        | Some _ ->
          match ret_expr.expression_content with
            | E_let_in li -> (
              let%bind lambda =
                let rec aux rhs = match rhs.expression_content with
                  | E_lambda l -> ok @@ l
                  | E_ascription a -> aux a.anno_expr
                  | _ -> fail @@ corner_case "recursive only supported for lambda"
                in
                aux rhs'
              in
              let fun_name = fst @@ List.hd prep_vars in
              let%bind fun_type = match ty_opt with
                | Some t -> ok @@ t
                | None -> match rhs'.expression_content with
                      | E_ascription a -> ok a.type_annotation
                      | _ -> fail @@ untyped_recursive_function e
              in
              let expression_content = E_recursive {fun_name;fun_type;lambda} in
              let expression_content = E_let_in {li with rhs = {li.rhs with  expression_content}} in
              ok @@ {ret_expr with expression_content}
          )
          | _ -> fail @@ corner_case "impossible"
      in
      ok ret_expr

      (* let f p1 ps... = rhs in body *)
      | (f, p1 :: ps) ->
        fail @@ unsupported_let_in_function e.region (f :: p1 :: ps)
      end
  | Raw.EAnnot a ->
      let Raw.{inside=expr, _, type_expr; _}, loc = r_split a in
      let%bind expr' = compile_expression expr in
      let%bind type_expr' = compile_type_expression type_expr in
      return @@ e_annotation ~loc expr' type_expr'
  | EVar c ->
      let (c',loc) = r_split c in
      (match constants c' with
       | None   -> return @@ e_variable ~loc (Var.of_name c.value)
       | Some s -> return @@ e_constant s [])
  | ECall x -> (
      let ((e1 , e2) , loc) = r_split x in
      let%bind args = bind_map_list compile_expression (nseq_to_list e2) in
      let rec chain_application (f: expression) (args: expression list) =
        match args with
        | hd :: tl ->  chain_application (e_application ~loc f hd) tl
        | [] -> f
      in
      match e1 with
      | EVar f -> (
          let (f , f_loc) = r_split f in
          match constants f with
          | None   -> return @@ chain_application (e_variable ~loc:f_loc (Var.of_name f)) args
          | Some s -> return @@ e_constant ~loc s args
              )
      | e1 ->
          let%bind e1' = compile_expression e1 in
          return @@ chain_application e1' args
  )
  | EPar x -> compile_expression x.value.inside
  | EUnit reg ->
      let (_ , loc) = r_split reg in
      return @@ e_literal ~loc Literal_unit
  | EBytes x ->
      let (x , loc) = r_split x in
      return @@ e_literal ~loc (Literal_bytes (Hex.to_bytes @@ snd x))
  | ETuple tpl -> compile_tuple_expression ~loc:(Location.lift tpl.region) @@ (npseq_to_list tpl.value)
  | ERecord r ->
      let (r , loc) = r_split r in
      let%bind fields = bind_list
        @@ List.map (fun ((k : _ Raw.reg), v) -> let%bind v = compile_expression v in ok (k.value, v))
        @@ List.map (fun (x:Raw.field_assign Raw.reg) -> (x.value.field_name, x.value.field_expr))
        @@ npseq_to_list r.ne_elements in
      return @@ e_record_ez ~loc fields
  | EProj p -> compile_projection p
  | EUpdate u -> compile_update u
  | EConstr (ESomeApp a) ->
      let (_, args), loc = r_split a in
      let%bind arg = compile_expression args in
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
      let%bind arg = compile_tuple_expression @@ args
      in return @@ e_constructor ~loc c_name arg
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
      let n = snd @@ n in
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
  | EString (String s) -> (
      let (s , loc) = r_split s in
      return @@ e_literal ~loc (Literal_string (Standard s))
    )
  | EString (Verbatim v) -> (
      let (v , loc) = r_split v in
      return @@ e_literal ~loc (Literal_string (Verbatim v))
    )
  | EString (Cat c) ->
    let (c, loc) = r_split c in
    let%bind string_left = compile_expression c.arg1 in
    let%bind string_right = compile_expression c.arg2 in
    return @@ e_string_cat ~loc string_left string_right
  | ELogic l -> compile_logic_expression l
  | EList l -> compile_list_expression l
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
      let default_action () =
        let%bind cases = compile_cases lst in
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
                    let%bind type_expr = compile_type_expression x'.type_expr in
                    return @@ e_let_in (var_name , Some type_expr) false e rhs
                  | _ -> default_action ()
                )
              | _ -> default_action ()
            )
          | _ -> default_action ()
        )
      | _ -> default_action ()
    )
  | EFun lamb -> compile_fun lamb
  | ESeq s -> (
      let (s , loc) = r_split s in
      let items : Raw.expr list = pseq_to_list s.elements in
      (match List.rev items with
         [] -> return @@ e_skip ~loc ()
       | expr::more ->
          let expr' = compile_expression expr in
          let apply e1 e2 =
            let%bind a = compile_expression e2 in
            let%bind e1' = e1 in
            return @@ e_sequence a e1'
          in List.fold_left apply expr' more)
    )
  | ECond c -> (
      let (c , loc) = r_split c in
      let%bind expr = compile_expression c.test in
      let%bind match_true = compile_expression c.ifso in
      let%bind match_false = compile_expression c.ifnot in
      return @@ e_cond ~loc expr match_true match_false
    )

and compile_fun lamb' : expr result =
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
          | Raw.PTuple t ->
            [Raw.PTyped
               {region=t.region;
                value=
                  { pt.value with pattern=
                                    Raw.PVar {region=pt.region;
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
        let%bind ty' = compile_type_expression ty in
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
               let_rhs=(Raw.EVar {region=pt.region; value="#P"});
              }
            in
            let let_in: Raw.let_in =
              {kwd_let= Region.ghost;
               kwd_rec= None;
               binding= let_in_binding;
               kwd_in= Region.ghost;
               body= lamb.body;
               attributes = []
              }
            in
            ok (Raw.ELetIn
                  {
                    region=pt.region;
                    value=let_in
                  })
          | Raw.PVar _ -> ok lamb.body
          | _ ->  ok lamb.body)
       | _ ->  ok lamb.body)
    | _ ->  ok lamb.body
  in
  let%bind (body , body_type) = expr_to_typed_expr body in
  let%bind output_type =
    bind_map_option compile_type_expression body_type in
  let%bind body = compile_expression body in
  let rec layer_arguments (arguments: (Raw.variable * type_expression) list) =
    match arguments with
    | hd :: tl ->
      let (binder , input_type) =
        (Var.of_name (fst hd).value , snd hd) in
      e_lambda ~loc (binder) (Some input_type) output_type (layer_arguments tl)
    | [] -> body
  in
  let ret_lamb = layer_arguments params' in
  return @@ ret_lamb


and compile_logic_expression ?te_annot (t:Raw.logic_expr) : expr result =
  let return x = ok @@ make_option_typed x te_annot in
  match t with
  | BoolExpr (False reg) -> (
      let loc = Location.lift reg in
      return @@ e_bool ~loc false
    )
  | BoolExpr (True reg) -> (
      let loc = Location.lift reg in
      return @@ e_bool ~loc true
    )
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

and compile_list_expression (t:Raw.list_expr) : expression result =
  let return x = ok @@ x in
  match t with
    ECons c -> compile_binop "CONS" c
  | EListComp lst -> (
      let (lst , loc) = r_split lst in
      let%bind lst' =
        bind_map_list compile_expression @@
        pseq_to_list lst.elements in
      return @@ e_list ~loc lst'
    )

and compile_binop (name:string) (t:_ Raw.bin_op Region.reg) : expression result =
  let return x = ok @@ x in
  let (args , loc) = r_split t in
  let%bind a = compile_expression args.arg1 in
  let%bind b = compile_expression args.arg2 in
  let%bind name = trace_option (unknown_built_in name) @@ constants name in
  return @@ e_constant ~loc name [ a ; b ]

and compile_unop (name:string) (t:_ Raw.un_op Region.reg) : expression result =
  let return x = ok @@ x in
  let (t , loc) = r_split t in
  let%bind a = compile_expression t.arg in
  let%bind name = trace_option (unknown_built_in name) @@ constants name in
  return @@ e_constant ~loc name [ a ]

and compile_tuple_expression ?loc (lst:Raw.expr list) : expression result =
  let return x = ok @@ x in
  match lst with
  | [] -> return @@ e_literal ?loc Literal_unit
  | [hd] -> compile_expression hd
  | lst ->
      let%bind lst = bind_list @@ List.map compile_expression lst in
      return @@ e_tuple ?loc lst

and compile_declaration : Raw.declaration -> declaration Location.wrap list result =
  fun t ->
  let open! Raw in
  let loc : 'a . 'a Raw.reg -> _ -> _ =
    fun x v -> Location.wrap ~loc:(File x.region) v in
  match t with
  | TypeDecl x ->
      let {name;type_expr} : Raw.type_decl = x.value in
      let%bind type_expression = compile_type_expression type_expr in
      ok @@ [loc x @@ Declaration_type (Var.of_name name.value , type_expression)]
  | Let x -> (
      let (region, recursive, let_binding, attributes), _ = r_split x in
      let inline = List.exists (fun (a: Raw.attribute) -> a.value = "inline") attributes in
      let binding = let_binding in
      let {binders; lhs_type; let_rhs} = binding in
      let (hd, _) = binders in
        match hd with
        | PTuple pt ->
          let process_variable (var_pair: pattern * Raw.expr) =
           (let (par_var, rhs_expr) = var_pair in
            let%bind (v, v_type) = pattern_to_typed_var par_var in
            let%bind v_type_expression =
              match v_type with
              | Some v_type -> ok (to_option (compile_type_expression v_type))
              | None -> ok None
            in
            let%bind compile_rhs_expr = compile_expression rhs_expr in
              ok @@ loc x @@ Declaration_constant (Var.of_name v.value, v_type_expression, inline, compile_rhs_expr) )
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
                                 {region = v.region;
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
               | other -> fail @@ abstracting_expr other
          in let%bind decls =
               (* TODO: Rewrite the gen_access_tuple so there's no List.rev *)
               bind_map_list process_variable (List.combine variables (List.rev expr_bind_lst))
          in ok @@ decls
        | PPar {region = _ ; value = { lpar = _ ; inside = pt; rpar = _; } } ->
          (* Extract parenthetical multi-bind *)
          let (wild, recursive, _, attributes) = fst @@ r_split x in
          compile_declaration
            (Let {
                region = x.region;
                value = (wild, recursive, {binders = (pt, []);
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
      let%bind lhs_type' = bind_map_option (fun x -> compile_type_expression (snd x)) lhs_type in
      let%bind let_rhs,lhs_type = match args with
      | [] -> ok (let_rhs, lhs_type')
      | param1::others ->
          let fun_ = {
            kwd_fun = Region.ghost;
            binders = param1, others;
            lhs_type;
            arrow   = Region.ghost;
            body    = let_rhs
          } in
          let f_args = nseq_to_list (param1,others) in
          let%bind ty = bind_map_list typed_pattern_to_typed_vars f_args in
          let aux acc (_,ty,loc) = Option.map (t_function ~loc ty) acc in
          ok (Raw.EFun {region; value=fun_},List.fold_right' aux lhs_type' ty)
      in
      let%bind rhs' = compile_expression let_rhs in
      let%bind lhs_type = match lhs_type with
      | None -> (match let_rhs with
        | EFun {value={binders;lhs_type}} ->
          let f_args = nseq_to_list (binders) in
          let%bind lhs_type' = bind_map_option (fun x -> compile_type_expression (snd x)) lhs_type in
          let%bind ty = bind_map_list typed_pattern_to_typed_vars f_args in
          let aux acc (_,ty,loc) = Option.map (t_function ~loc ty) acc in
          ok @@ (List.fold_right' aux lhs_type' ty)
        | _ -> ok None
        )
      | Some t -> ok @@ Some t
      in
      let binder = Var.of_name var.value in
      let%bind rhs' = match recursive with
        None -> ok @@ rhs'
        | Some _ -> match rhs'.expression_content with
          E_lambda lambda ->
            (match lhs_type with
              None -> fail @@ untyped_recursive_function var
              | Some (lhs_type) ->
              let expression_content = E_recursive {fun_name=binder;fun_type=lhs_type;lambda} in
              ok @@ {rhs' with expression_content})
          | _ -> ok @@ rhs'
      in
      ok @@ [loc x @@ (Declaration_constant (Var.of_name var.value , lhs_type , inline, rhs'))]
    )

and compile_cases : type a . (Raw.pattern * a) list -> (a, unit) matching_content result =
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
           PConstrApp {value; region} ->
           (match value with
           | constr, None ->
              constr, Some (PVar {value = "unit"; region})
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
      ok @@ Match_variant ([((Constructor "true", Var.of_name "_"), t); ((Constructor "false", Var.of_name "_"), f)], ())
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
                         (Parser_cameligo.ParserLog.pattern_to_string
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

let compile_program : Raw.ast -> program result = fun t ->
  let%bind decls = bind_map_list compile_declaration @@ nseq_to_list t.decl in
  ok @@ List.concat @@ decls
