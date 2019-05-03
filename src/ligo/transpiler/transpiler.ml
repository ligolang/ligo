open! Trace
open Mini_c
open Combinators

module AST = Ast_typed
open AST.Combinators

let temp_unwrap_loc = Location.unwrap
let temp_unwrap_loc_list = List.map Location.unwrap

let list_of_map m = List.rev @@ Map.String.fold (fun _ v prev -> v :: prev) m []
let kv_list_of_map m = List.rev @@ Map.String.fold (fun k v prev -> (k, v) :: prev) m []
let map_of_kv_list lst =
  let open AST.SMap in
  List.fold_left (fun prev (k, v) -> add k v prev) empty lst

let rec translate_type (t:AST.type_value) : type_value result =
  match t.type_value' with
  | T_constant ("bool", []) -> ok (T_base Base_bool)
  | T_constant ("int", []) -> ok (T_base Base_int)
  | T_constant ("nat", []) -> ok (T_base Base_nat)
  | T_constant ("tez", []) -> ok (T_base Base_tez)
  | T_constant ("string", []) -> ok (T_base Base_string)
  | T_constant ("address", []) -> ok (T_base Base_address)
  | T_constant ("unit", []) -> ok (T_base Base_unit)
  | T_constant ("operation", []) -> ok (T_base Base_operation)
  | T_constant ("map", [key;value]) ->
      let%bind kv' = bind_map_pair translate_type (key, value) in
      ok (T_map kv')
  | T_constant ("list", [t]) ->
      let%bind t' = translate_type t in
      ok (T_list t')
  | T_constant ("option", [o]) ->
      let%bind o' = translate_type o in
      ok (T_option o')
  | T_constant (name, _) -> fail (fun () -> error (thunk "unrecognized type constant") (fun () -> name) ())
  | T_sum m ->
      let node = Append_tree.of_list @@ list_of_map m in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (T_or (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | T_record m ->
      let node = Append_tree.of_list @@ list_of_map m in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (T_pair (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | T_tuple lst ->
      let node = Append_tree.of_list lst in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (T_pair (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | T_function (param, result) ->
      let%bind param' = translate_type param in
      let%bind result' = translate_type result in
      ok (T_function (param', result'))

let tuple_access_to_lr : type_value -> type_value list -> int -> (type_value * [`Left | `Right]) list result = fun ty tys ind ->
  let node_tv = Append_tree.of_list @@ List.mapi (fun i a -> (i, a)) tys in
  let%bind path =
    let aux (i , _) = i = ind in
    trace_option (simple_error "no leaf with given index") @@
    Append_tree.exists_path aux node_tv in
  let lr_path = List.map (fun b -> if b then `Right else `Left) path in
  let%bind (_ , lst) =
    let aux = fun (ty , acc) cur ->
      let%bind (a , b) = Mini_c.get_t_pair ty in
      match cur with
      | `Left -> ok (a , (a , `Left) :: acc)
      | `Right -> ok (b , (b , `Right) :: acc) in
    bind_fold_list aux (ty , []) lr_path in
  ok lst

let record_access_to_lr : type_value -> type_value AST.type_name_map -> string -> (type_value * (type_value * [`Left | `Right]) list) result = fun ty tym ind ->
  let tys = kv_list_of_map tym in
  let node_tv = Append_tree.of_list tys in
  let leaf (i, _) : (type_value * (type_value * [`Left | `Right]) list) result =
    if i = ind then (
      ok (ty, [])
    ) else (
      simple_fail "bad leaf"
    ) in
  let node a b : (type_value * (type_value * [`Left | `Right]) list) result =
    match%bind bind_lr (a, b) with
    | `Left (t, acc) ->
        let%bind (a, _) = Mini_c.get_t_pair t in
        ok @@ (t, (a, `Left) :: acc)
    | `Right (t, acc) -> (
        let%bind (_, b) = Mini_c.get_t_pair t in
        ok @@ (t, (b, `Right) :: acc)
      ) in
  let error_content () =
    let aux ppf (name, ty) = Format.fprintf ppf "%s -> %a" name PP.type_ ty in
    Format.asprintf "(%a).%s" (PP.list_sep_d aux) tys ind in
  trace_strong (fun () -> error (thunk "bad index in record (shouldn't happen here)") error_content ()) @@
  Append_tree.fold_ne leaf node node_tv

let rec translate_block env (b:AST.block) : block result =
  let aux = fun (precs, env) instruction ->
    let%bind lst = translate_instruction env instruction in
    let env' = List.fold_left (fun _ i -> (snd i).post_environment) env lst in (* Get last environment *)
    ok (precs @ lst, env') in
  let%bind (instructions, env') = bind_fold_list aux ([], env) b in
  ok (instructions, environment_wrap env env')

and translate_instruction (env:Environment.t) (i:AST.instruction) : statement list result =
  let return ?(env' = env) x : statement list result = ok ([x, environment_wrap env env']) in
  match i with
  | I_declaration {name;annotated_expression} ->
      let%bind expression = translate_annotated_expression env annotated_expression in
      let env' = Environment.add (name, (Combinators.Expression.get_type expression)) env in
      return ~env' (S_declaration (name, expression))
  | I_assignment {name;annotated_expression} ->
      let%bind expression = translate_annotated_expression env annotated_expression in
      return (S_assignment (name, expression))
  | I_patch (r, s, v) -> (
      let ty = r.type_value in
      let aux : ((AST.type_value * [`Left | `Right] list) as 'a) -> AST.access -> 'a result =
        fun (prev, acc) cur ->
          let%bind ty' = translate_type prev in
          match cur with
          | Access_tuple ind ->
              let%bind ty_lst = AST.Combinators.get_t_tuple prev in
              let%bind ty'_lst = bind_map_list translate_type ty_lst in
              let%bind path = tuple_access_to_lr ty' ty'_lst ind in
              let path' = List.map snd path in
              ok (List.nth ty_lst ind, path' @ acc)
          | Access_record prop ->
              let%bind ty_map =
                let error =
                  let title () = "accessing property on not a record" in
                let content () = Format.asprintf "%s on %a in %a"
                    prop Ast_typed.PP.type_value prev Ast_typed.PP.instruction i in
                  error title content
                in
                trace error @@
                AST.Combinators.get_t_record prev in
              let%bind ty'_map = bind_map_smap translate_type ty_map in
              let%bind (_, path) = record_access_to_lr ty' ty'_map prop in
              let path' = List.map snd path in
              ok (Map.String.find prop ty_map, path' @ acc)
          | Access_map _k -> simple_fail "no patch for map yet"
      in
      let%bind (_, path) = bind_fold_list aux (ty, []) s in
      let%bind v' = translate_annotated_expression env v in
      return (S_patch (r.type_name, path, v'))
    )
  | I_matching (expr, m) -> (
      let%bind expr' = translate_annotated_expression env expr in
      let env' = Environment.extend env in
      let extend s =
        let pre = Combinators.statement S_environment_extend env in
        ok [ pre ; (s, environment_wrap env env) ] in
      let restrict : block -> block = fun b -> Combinators.append_statement' b S_environment_restrict in
      match m with
      | Match_bool {match_true ; match_false} -> (
          let%bind true_branch = translate_block env' match_true in
          let%bind false_branch = translate_block env' match_false in
          extend @@ S_cond (expr', restrict true_branch, restrict false_branch)
        )
      | Match_option {match_none ; match_some = ((name, t), sm)} -> (
          let%bind none_branch = translate_block env' match_none in
          let%bind t' = translate_type t in
          let%bind some_branch =
            let env'' = Environment.add (name, t') env' in
            translate_block env'' sm
          in
          extend (S_if_none (expr', restrict none_branch, ((name, t'), restrict some_branch)))
        )
      | _ -> simple_fail "todo : match"
    )
  | I_loop (expr, body) ->
      let%bind expr' = translate_annotated_expression env expr in
      let env' = Environment.extend env in
      let%bind body' = translate_block env' body in
      return (S_while (expr', body'))
  | I_skip -> ok []
  | I_do ae -> (
      let%bind ae' = translate_annotated_expression env ae in
      return @@ S_do ae'
    )

and translate_literal : AST.literal -> value = fun l -> match l with
  | Literal_bool b -> D_bool b
  | Literal_int n -> D_int n
  | Literal_nat n -> D_nat n
  | Literal_tez n -> D_tez n
  | Literal_bytes s -> D_bytes s
  | Literal_string s -> D_string s
  | Literal_address s -> D_string s
  | Literal_unit -> D_unit

and transpile_small_environment : AST.small_environment -> Environment.Small.t result = fun x ->
  let x' = AST.Environment.Small.get_environment x in
  let aux prec (name , (ele : AST.environment_element)) =
    let%bind tv' = translate_type ele.type_value in
    ok @@ Environment.Small.append (name , tv') prec
  in
  trace (simple_error "transpiling small environment") @@
  bind_fold_right_list aux Append_tree.Empty x'

and transpile_environment : AST.full_environment -> Environment.t result = fun x ->
  let%bind nlst = bind_map_ne_list transpile_small_environment x in
  ok @@ List.Ne.to_list nlst

and tree_of_sum : AST.type_value -> (type_name * AST.type_value) Append_tree.t result = fun t ->
  let%bind map_tv = get_t_sum t in
  ok @@ Append_tree.of_list @@ kv_list_of_map map_tv

and translate_annotated_expression (env:Environment.t) (ae:AST.annotated_expression) : expression result =
  let%bind tv = translate_type ae.type_annotation in
  let return ?(tv = tv) ?(env = env) expr =
    (* let%bind env' = transpile_environment ae.environment in *)
    ok @@ Combinators.Expression.make_tpl (expr, tv, env) in
  let f = translate_annotated_expression env in
  match ae.expression with
  | E_failwith ae -> (
      let%bind ae' = translate_annotated_expression env ae in
      return @@ E_constant ("FAILWITH" , [ae'])
    )
  | E_literal l -> return @@ E_literal (translate_literal l)
  | E_variable name ->
      let%bind tv =
        trace_option (simple_error "transpiler: variable not in env") @@
        Environment.get_opt env name in
      return ~tv @@ E_variable name
  | E_application (a, b) ->
      let%bind a = translate_annotated_expression env a in
      let%bind b = translate_annotated_expression env b in
      return @@ E_application (a, b)
  | E_constructor (m, param) ->
      let%bind param' = translate_annotated_expression env param in
      let (param'_expr , param'_tv) = Combinators.Expression.(get_content param' , get_type param') in
      let%bind node_tv = tree_of_sum ae.type_annotation in
      let leaf (k, tv) : (expression' option * type_value) result =
        if k = m then (
          let%bind _ =
            trace (simple_error "constructor parameter doesn't have expected type (shouldn't happen here)")
            @@ AST.assert_type_value_eq (tv, param.type_annotation) in
          ok (Some (param'_expr), param'_tv)
        ) else (
          let%bind tv = translate_type tv in
          ok (None, tv)
        ) in
      let node a b : (expression' option * type_value) result =
        let%bind a = a in
        let%bind b = b in
        match (a, b) with
        | (None, a), (None, b) -> ok (None, T_or (a, b))
        | (Some _, _), (Some _, _) -> simple_fail "several identical constructors in the same variant (shouldn't happen here)"
        | (Some v, a), (None, b) -> ok (Some (E_constant ("LEFT", [Combinators.Expression.make_tpl (v, a, env)])), T_or (a, b))
        | (None, a), (Some v, b) -> ok (Some (E_constant ("RIGHT", [Combinators.Expression.make_tpl (v, b, env)])), T_or (a, b))
      in
      let%bind (ae_opt, tv) = Append_tree.fold_ne leaf node node_tv in
      let%bind ae =
        trace_option (simple_error "constructor doesn't exist in claimed type (shouldn't happen here)")
          ae_opt in
      return ~tv ae
  | E_tuple lst ->
      let node = Append_tree.of_list lst in
      let aux (a:expression result) (b:expression result) : expression result =
        let%bind a = a in
        let%bind b = b in
        let a_ty = Combinators.Expression.get_type a in
        let b_ty = Combinators.Expression.get_type b in
        let tv = T_pair (a_ty , b_ty) in
        return ~tv @@ E_constant ("PAIR", [a; b])
      in
      Append_tree.fold_ne (translate_annotated_expression env) aux node
  | E_tuple_accessor (tpl, ind) ->
      let%bind ty' = translate_type tpl.type_annotation in
      let%bind ty_lst = get_t_tuple tpl.type_annotation in
      let%bind ty'_lst = bind_map_list translate_type ty_lst in
      let%bind path = tuple_access_to_lr ty' ty'_lst ind in
      let aux = fun pred (ty, lr) ->
        let c = match lr with
          | `Left -> "CAR"
          | `Right -> "CDR" in
        Combinators.Expression.make_tpl (E_constant (c, [pred]) , ty , env) in
      let%bind tpl' = translate_annotated_expression env tpl in
      let expr = List.fold_right' aux tpl' path in
      ok expr
  | E_record m ->
      let node = Append_tree.of_list @@ list_of_map m in
      let aux a b : expression result =
        let%bind a = a in
        let%bind b = b in
        let a_ty = Combinators.Expression.get_type a in
        let b_ty = Combinators.Expression.get_type b in
        let tv = T_pair (a_ty , b_ty) in
        return ~tv @@ E_constant ("PAIR", [a; b])
      in
      Append_tree.fold_ne (translate_annotated_expression env) aux node
  | E_record_accessor (record, property) ->
      let%bind translation = translate_annotated_expression env record in
      let%bind record_type_map =
        let error =
          let title () =
            Format.asprintf "Accessing field of %a, that has type %a, which isn't a record"
              AST.PP.annotated_expression record AST.PP.type_value record.type_annotation in
          let content () = "" in
          error title content in
        trace error @@
        get_t_record record.type_annotation in
      let node_tv = Append_tree.of_list @@ kv_list_of_map record_type_map in
      let leaf (key, _) : expression result =
        if property = key then (
          ok translation
        ) else (
          simple_fail "bad leaf"
        ) in
      let node (a:expression result) b : expression result =
        match%bind bind_lr (a, b) with
        | `Left expr -> (
            let%bind (tv, _) = Mini_c.get_t_pair @@ Expression.get_type expr in
            return ~tv @@ E_constant ("CAR", [expr])
          )
        | `Right expr -> (
            let%bind (_, tv) = Mini_c.get_t_pair @@ Expression.get_type expr in
            return ~tv @@ E_constant ("CDR", [expr])
          ) in
      let%bind expr =
        trace_strong (simple_error "bad key in record (shouldn't happen here)") @@
        Append_tree.fold_ne leaf node node_tv in
      ok expr
  | E_constant (name, lst) ->
      let%bind lst' = bind_list @@ List.map (translate_annotated_expression env) lst in (
        match name, lst with
        | "NONE", [] ->
            let%bind o = Mini_c.Combinators.get_t_option tv in
            return @@ E_make_none o
        | _ -> return @@ E_constant (name, lst')
      )
  | E_lambda l -> translate_lambda env l
  | E_list lst ->
      let%bind t = Mini_c.Combinators.get_t_list tv in
      let%bind lst' = bind_map_list (translate_annotated_expression env) lst in
      let aux : expression -> expression -> expression result = fun prev cur ->
        return @@ E_constant ("CONS", [cur ; prev]) in
      let%bind (init : expression) = return @@ E_empty_list t in
      bind_fold_list aux init lst'
  | E_map m ->
      let%bind (src, dst) = Mini_c.Combinators.get_t_map tv in
      let aux : expression result -> (AST.ae * AST.ae) -> expression result = fun prev (k, v) ->
        let%bind prev' = prev in
        let%bind (k', v') =
          let v' = e_a_some v ae.environment in
          bind_map_pair (translate_annotated_expression env) (k, v') in
        return @@ E_constant ("UPDATE", [k' ; v' ; prev'])
      in
      let init = return @@ E_empty_map (src, dst) in
      List.fold_left aux init m
  | E_look_up dsi ->
      let%bind (ds', i') = bind_map_pair f dsi in
      return @@ E_constant ("GET", [i' ; ds'])
  | E_matching (expr, m) -> (
      let%bind expr' = translate_annotated_expression env expr in
      match m with
      | Match_bool {match_true ; match_false} ->
          let%bind (t , f) = bind_map_pair (translate_annotated_expression env) (match_true, match_false) in
          return @@ E_Cond (expr', t, f)
      | Match_option { match_none; match_some = ((name, tv), s) } ->
          let%bind n = translate_annotated_expression env match_none in
          let%bind (tv' , s') =
            let%bind tv' = translate_type tv in
            let env' = Environment.(add (name , tv') @@ extend env) in
            let%bind s' = translate_annotated_expression env' s in
            ok (tv' , s') in
          return @@ E_if_none (expr' , n , ((name , tv') , s'))
      | Match_variant (lst , variant) -> (
          let%bind tree = tree_of_sum variant in
          let%bind tree' = match tree with
            | Empty -> simple_fail "match empty variant"
            | Full x -> ok x in
          let%bind tree'' =
            let rec aux t =
              match (t : _ Append_tree.t') with
              | Leaf (name , tv) ->
                  let%bind tv' = translate_type tv in
                  ok (`Leaf name , tv')
              | Node {a ; b} ->
                  let%bind a' = aux a in
                  let%bind b' = aux b in
                  let tv' = Mini_c.t_union (snd a') (snd b') in
                  ok (`Node (a' , b') , tv')
            in aux tree'
          in

          let rec aux (acc , env) t =
            let top =
              match acc with
              | None -> expr'
              | Some x -> x in
            match t with
            | ((`Leaf constructor_name) , tv) -> (
                let%bind ((_ , name) , body) =
                  trace_option (simple_error "not supposed to happen here: missing match clause") @@
                  List.find_opt (fun ((constructor_name' , _) , _) -> constructor_name' = constructor_name) lst in
                let env' = Environment.(add (name , tv) @@ extend env) in
                let%bind body' = translate_annotated_expression env' body in
                return ~env @@ E_let_in ((name , tv) , top , body')
              )
            | ((`Node (a , b)) , tv) ->
                let%bind a' =
                  let%bind a_ty = get_t_left tv in
                  let a_var = "left" , a_ty in
                  let env' = Environment.(add a_var @@ extend env) in
                  let%bind e = aux ((Some (Expression.make (E_variable "left") a_ty env')) , env') a in
                  ok (a_var , e)
                in
                let%bind b' =
                  let%bind b_ty = get_t_right tv in
                  let b_var = "right" , b_ty in
                  let env' = Environment.(add b_var @@ extend env) in
                  let%bind e = aux ((Some (Expression.make (E_variable "right") b_ty env')) , env') b in
                  ok (b_var , e)
                in
                return ~env @@ E_if_left (top , a' , b')
          in
          aux (None , env) tree''
        )
      | AST.Match_list _ | AST.Match_tuple (_, _) ->
          simple_fail "only match bool and option exprs are translated yet"
    )


and translate_lambda_shallow : Mini_c.Environment.t -> AST.lambda -> Mini_c.expression result = fun env l ->
  let { binder ; input_type ; output_type ; body ; result } : AST.lambda = l in
  (* Shallow capture. Capture the whole environment. Extend it with a new scope. Append it the input. *)
  let env' = Environment.extend env in
  let%bind input_type' = translate_type input_type in
  let new_env = Environment.add (binder, input_type') env' in
  let%bind (_, e) as body = translate_block new_env body in
  let%bind result = translate_annotated_expression e.post_environment result in
  let%bind output_type' = translate_type output_type in
  let tv = Combinators.t_shallow_closure env input_type' output_type' in
  let capture_type = Shallow_capture env' in
  let content = {binder;input=input_type';output=output_type';body;result;capture_type} in
  ok @@ Combinators.Expression.make_tpl (E_function content, tv, env)

and translate_lambda_deep : Mini_c.Environment.t -> AST.lambda -> Mini_c.expression result = fun env l ->
  let { binder ; input_type ; output_type ; body ; result } : AST.lambda = l in
  (* Deep capture. Capture the relevant part of the environment. Extend it with a new scope. Append it the input. *)
  let%bind input_type' = translate_type input_type in
  let%bind small_env =
    let env' = Environment.extend env in
    let new_env = Environment.add (binder, input_type') env' in
    let free_variables = Ast_typed.Misc.Free_variables.lambda [] l in
    let%bind elements =
      let aux x =
        let not_found_error =
          let title () = "translate deep shallow (type checker didn't do its job)" in
          let content () = Format.asprintf "%s in %a" x Mini_c.PP.environment new_env  in
          error title content in
        trace_option not_found_error @@
        Environment.get_opt new_env x in
      bind_map_list aux free_variables in
    let kvs = List.combine free_variables elements in
    let small_env = Environment.Small.of_list kvs in
    ok small_env
  in
  let new_env = Environment.(add (binder , input_type') @@ extend @@ of_small small_env) in
  let%bind (_, e) as body = translate_block new_env body in
  let%bind result = translate_annotated_expression e.post_environment result in
  let%bind output_type' = translate_type output_type in
  let tv = Combinators.t_deep_closure small_env input_type' output_type' in
  let capture_type = Deep_capture small_env in
  let content = {binder;input=input_type';output=output_type';body;result;capture_type} in
  ok @@ Combinators.Expression.make_tpl (E_function content, tv, env)

and translate_lambda env l =
  let { binder ; input_type ; output_type ; body ; result } : AST.lambda = l in
  (* Try to translate it in an empty env, if it succeeds, transpiles it as a quote value, else, as a closure expression. *)
  let ((_body_bounds , body_fvs) , result_fvs) = AST.Free_variables.(
      let bindings = singleton binder in
      let ((body_bounds , _) as b) = block' bindings body in
      b , annotated_expression body_bounds result
    ) in
  match (body_fvs, result_fvs) with
  | [] , [] -> (
      let%bind empty_env =
        let%bind input = translate_type input_type in
        ok Environment.(add (binder, input) empty) in
      let%bind ((_, e) as body') = translate_block empty_env body in
      let%bind result' = translate_annotated_expression e.post_environment result in
      trace (simple_error "translate quote") @@
      let capture_type = No_capture in
      let%bind input = translate_type input_type in
      let%bind output = translate_type output_type in
      let tv = Combinators.t_function input output in
      let content = {binder;input;output;body=body';result=result';capture_type} in
      ok @@ Combinators.Expression.make_tpl (E_literal (D_function {capture=None;content}), tv, env)
    )
  | _ -> (
      trace (simple_error "translate lambda deep") @@
      translate_lambda_deep env l
    )

let translate_declaration env (d:AST.declaration) : toplevel_statement result =
  match d with
  | Declaration_constant ({name;annotated_expression} , _) ->
      let%bind expression = translate_annotated_expression env annotated_expression in
      let tv = Combinators.Expression.get_type expression in
      let env' = Environment.add (name, tv) env in
      ok @@ ((name, expression), environment_wrap env env')

let translate_program (lst:AST.program) : program result =
  let aux (prev:(toplevel_statement list * Environment.t) result) cur =
    let%bind (tl, env) = prev in
    let%bind ((_, env') as cur') = translate_declaration env cur in
    ok (cur' :: tl, env'.post_environment)
  in
  let%bind (statements, _) = List.fold_left aux (ok ([], Environment.empty)) (temp_unwrap_loc_list lst) in
  ok statements

let translate_main (l:AST.lambda) (_t:AST.type_value) : anon_function result =
  let%bind expr = translate_lambda Environment.empty l in
  match Combinators.Expression.get_content expr with
  | E_literal (D_function f) -> ok f
  | _ -> simple_fail "main is not a function"

(* From a non-functional expression [expr], build the functional expression [fun () -> expr] *)
let functionalize (e:AST.annotated_expression) : AST.lambda * AST.type_value =
  let t = e.type_annotation in
  let open! AST in
  {
    binder = "_" ;
    input_type = Combinators.t_unit () ;
    output_type = t ;
    result = e ;
    body = [I_skip]
  }, Combinators.(t_function (t_unit ()) t ())

let translate_entry (lst:AST.program) (name:string) : anon_function result =
  let%bind (lst', l, tv) =
    let rec aux acc (lst:AST.program) =
      match lst with
      | [] -> None
      | hd :: tl -> (
          let (AST.Declaration_constant (an , _)) = temp_unwrap_loc hd in
          match an.name = name with
          | true -> (
              match an.annotated_expression.expression with
              | E_lambda l -> Some (acc, l, an.annotated_expression.type_annotation)
              | _ ->
                  let (a, b) = functionalize an.annotated_expression in
                  Some (acc, a, b)
            )
          | false -> aux (acc @ [AST.I_declaration an]) tl
        )
    in
    let%bind (lst', l, tv) =
      trace_option (simple_error "no entry-point with given name")
      @@ aux [] lst in
    ok (lst', l, tv) in
  let l' = {l with body = lst' @ l.body} in
  let r =
    trace (simple_error "translating entry") @@
    translate_main l' tv in
  r

open Combinators

let rec exp x n =
  if n = 0
  then 1
  else
    let exp' = exp (x * x) (n / 2) in
    let m = if n mod 2 = 0 then 1 else x in
    m * exp'

let exp2 = exp 2

let extract_constructor (v : value) (tree : _ Append_tree.t') : (string * value * AST.type_value) result =
  let open Append_tree in
  let rec aux tv : (string * value * AST.type_value) result=
    match tv with
    | Leaf (k, t), v -> ok (k, v, t)
    | Node {a}, D_left v -> aux (a, v)
    | Node {b}, D_right v -> aux (b, v)
    | _ -> simple_fail "bad constructor path"
  in
  let%bind (s, v, t) = aux (tree, v) in
  ok (s, v, t)

let extract_tuple (v : value) (tree : AST.type_value Append_tree.t') : ((value * AST.type_value) list) result =
  let open Append_tree in
  let rec aux tv : ((value * AST.type_value) list) result =
    match tv with
    | Leaf t, v -> ok @@ [v, t]
    | Node {a;b}, D_pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> simple_fail "bad tuple path"
  in
  aux (tree, v)

let extract_record (v : value) (tree : _ Append_tree.t') : (_ list) result =
  let open Append_tree in
  let rec aux tv : ((string * (value * AST.type_value)) list) result =
    match tv with
    | Leaf (s, t), v -> ok @@ [s, (v, t)]
    | Node {a;b}, D_pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> simple_fail "bad record path"
  in
  aux (tree, v)

let rec untranspile (v : value) (t : AST.type_value) : AST.annotated_expression result =
  let open! AST in
  let return e = ok (make_a_e_empty e t) in
  match t.type_value' with
  | T_constant ("unit", []) ->
      let%bind () = get_unit v in
      return (E_literal Literal_unit)
  | T_constant ("bool", []) ->
      let%bind b = get_bool v in
      return (E_literal (Literal_bool b))
  | T_constant ("int", []) ->
      let%bind n = get_int v in
      return (E_literal (Literal_int n))
  | T_constant ("nat", []) ->
      let%bind n = get_nat v in
      return (E_literal (Literal_nat n))
  | T_constant ("tez", []) ->
      let%bind n = get_nat v in
      return (E_literal (Literal_tez n))
  | T_constant ("string", []) ->
      let%bind n = get_string v in
      return (E_literal (Literal_string n))
  | T_constant ("address", []) ->
      let%bind n = get_string v in
      return (E_literal (Literal_address n))
  | T_constant ("option", [o]) -> (
      match%bind get_option v with
      | None -> ok (e_a_empty_none o)
      | Some s ->
          let%bind s' = untranspile s o in
          ok (e_a_empty_some s')
    )
  | T_constant ("map", [k_ty;v_ty]) -> (
      let%bind lst = get_map v in
      let%bind lst' =
        let aux = fun (k, v) ->
          let%bind k' = untranspile k k_ty in
          let%bind v' = untranspile v v_ty in
          ok (k', v') in
        bind_map_list aux lst in
      return (E_map lst')
    )
  | T_constant ("list", [ty]) -> (
      let%bind lst = get_list v in
      let%bind lst' =
        let aux = fun e -> untranspile e ty in
        bind_map_list aux lst in
      return (E_list lst')
    )
  | T_constant _ ->
      simple_fail "unknown type_constant"
  | T_sum m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> simple_fail "empty sum type"
        | Full t -> ok t
      in
      let%bind (name, v, tv) = extract_constructor v node in
      let%bind sub = untranspile v tv in
      return (E_constructor (name, sub))
  | T_tuple lst ->
      let%bind node = match Append_tree.of_list lst with
        | Empty -> simple_fail "empty tuple"
        | Full t -> ok t in
      let%bind tpl = extract_tuple v node in
      let%bind tpl' = bind_list
        @@ List.map (fun (x, y) -> untranspile x y) tpl in
      return (E_tuple tpl')
  | T_record m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> simple_fail "empty record"
        | Full t -> ok t in
      let%bind lst = extract_record v node in
      let%bind lst = bind_list
        @@ List.map (fun (x, (y, z)) -> let%bind yz = untranspile y z in ok (x, yz)) lst in
      let m' = map_of_kv_list lst in
      return (E_record m')
  | T_function _ -> simple_fail "no untranspilation for functions yet"
