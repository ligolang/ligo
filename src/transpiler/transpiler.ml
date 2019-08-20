open! Trace

module AST = Ast_typed
module Append_tree = Tree.Append
open AST.Combinators
open Mini_c
open Combinators

let temp_unwrap_loc = Location.unwrap
let temp_unwrap_loc_list = List.map Location.unwrap

let list_of_map m = List.rev @@ Map.String.fold (fun _ v prev -> v :: prev) m []
let kv_list_of_map m = List.rev @@ Map.String.fold (fun k v prev -> (k, v) :: prev) m []
let map_of_kv_list lst =
  let open AST.SMap in
  List.fold_left (fun prev (k, v) -> add k v prev) empty lst

module Errors = struct
  let corner_case ~loc message =
    let title () = "corner case" in
    let content () = "we don't have a good error message for this case. we are
striving find ways to better report them and find the use-cases that generate
them. please report this to the developers." in
    let data = [
      ("location" , fun () -> loc) ;
      ("message" , fun () -> message) ;
    ] in
    error ~data title content

  let unrecognized_type_constant name =
    let title () = "unrecognized type constant" in
    let content () = name in
    error title content

  let row_loc l = ("location" , fun () -> Format.asprintf "%a" Location.pp l)

  let unsupported_pattern_matching kind location =
    let title () = "unsupported pattern-matching" in
    let content () = Format.asprintf "%s patterns aren't supported yet" kind in
    let data = [
        row_loc location ;
      ] in
    error ~data title content

  let unsupported_iterator location =
    let title () = "unsupported iterator" in
    let content () = "only lambda are supported as iterators" in
    let data = [
        row_loc location ;
      ] in
    error ~data title content

  let not_functional_main location =
    let title () = "not functional main" in
    let content () = "main should be a function" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp location) ;
    ] in
    error ~data title content

  let missing_entry_point name =
    let title () = "missing entry point" in
    let content () = "no entry point with the given name" in
    let data = [
      ("name" , fun () -> name) ;
    ] in
    error ~data title content

  let wrong_mini_c_value expected_type actual =
    let title () = "illed typed intermediary value" in
    let content () = "type of intermediary value doesn't match what was expected" in
    let data = [
      ("expected_type" , fun () -> expected_type) ;
      ("actual" , fun () -> Format.asprintf "%a" Mini_c.PP.value actual ) ;
    ] in
    error ~data title content

  let bad_untranspile bad_type value =
    let title () = "untranspiling bad value" in
    let content () = Format.asprintf "can not untranspile %s" bad_type in
    let data = [
      ("bad_type" , fun () -> bad_type) ;
      ("value" , fun () -> Format.asprintf "%a" Mini_c.PP.value value) ;
    ] in
    error ~data title content

  let unknown_untranspile unknown_type value =
    let title () = "untranspiling unknown value" in
    let content () = Format.asprintf "can not untranspile %s" unknown_type in
    let data = [
      ("unknown_type" , fun () -> unknown_type) ;
      ("value" , fun () -> Format.asprintf "%a" Mini_c.PP.value value) ;
    ] in
    error ~data title content
end
open Errors

let rec translate_type (t:AST.type_value) : type_value result =
  match t.type_value' with
  | T_constant ("bool", []) -> ok (T_base Base_bool)
  | T_constant ("int", []) -> ok (T_base Base_int)
  | T_constant ("nat", []) -> ok (T_base Base_nat)
  | T_constant ("tez", []) -> ok (T_base Base_tez)
  | T_constant ("string", []) -> ok (T_base Base_string)
  | T_constant ("address", []) -> ok (T_base Base_address)
  | T_constant ("timestamp", []) -> ok (T_base Base_timestamp)
  | T_constant ("unit", []) -> ok (T_base Base_unit)
  | T_constant ("operation", []) -> ok (T_base Base_operation)
  | T_constant ("contract", [x]) ->
      let%bind x' = translate_type x in
      ok (T_contract x')
  | T_constant ("map", [key;value]) ->
      let%bind kv' = bind_map_pair translate_type (key, value) in
      ok (T_map kv')
  | T_constant ("list", [t]) ->
      let%bind t' = translate_type t in
      ok (T_list t')
  | T_constant ("set", [t]) ->
      let%bind t' = translate_type t in
      ok (T_set t')
  | T_constant ("option", [o]) ->
      let%bind o' = translate_type o in
      ok (T_option o')
  | T_constant (name , _lst) -> fail @@ unrecognized_type_constant name
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
  | T_function (param, result) -> (
      let%bind param' = translate_type param in
      let%bind result' = translate_type result in
      ok (T_function (param', result'))
    )

let tuple_access_to_lr : type_value -> type_value list -> int -> (type_value * [`Left | `Right]) list result = fun ty tys ind ->
  let node_tv = Append_tree.of_list @@ List.mapi (fun i a -> (i, a)) tys in
  let%bind path =
    let aux (i , _) = i = ind in
    trace_option (corner_case ~loc:__LOC__ "tuple access leaf") @@
    Append_tree.exists_path aux node_tv in
  let lr_path = List.map (fun b -> if b then `Right else `Left) path in
  let%bind (_ , lst) =
    let aux = fun (ty' , acc) cur ->
      let%bind (a , b) =
        trace_strong (corner_case ~loc:__LOC__ "tuple access pair") @@
        Mini_c.get_t_pair ty' in
      match cur with
      | `Left -> ok (a , acc @ [(a , `Left)])
      | `Right -> ok (b , acc @ [(b , `Right)])
    in
    bind_fold_list aux (ty , []) lr_path in
  ok lst

let record_access_to_lr : type_value -> type_value AST.type_name_map -> string -> (type_value * [`Left | `Right]) list result = fun ty tym ind ->
  let tys = kv_list_of_map tym in
  let node_tv = Append_tree.of_list tys in
  let%bind path =
    let aux (i , _) = i = ind in
    trace_option (corner_case ~loc:__LOC__ "record access leaf") @@
    Append_tree.exists_path aux node_tv in
  let lr_path = List.map (fun b -> if b then `Right else `Left) path in
  let%bind (_ , lst) =
    let aux = fun (ty , acc) cur ->
      let%bind (a , b) =
        trace_strong (corner_case ~loc:__LOC__ "recard access pair") @@
        Mini_c.get_t_pair ty in
      match cur with
      | `Left -> ok (a , acc @ [(a , `Left)])
      | `Right -> ok (b , acc @ [(b , `Right)] ) in
    bind_fold_list aux (ty , []) lr_path in
  ok lst

let rec translate_literal : AST.literal -> value = fun l -> match l with
  | Literal_bool b -> D_bool b
  | Literal_int n -> D_int n
  | Literal_nat n -> D_nat n
  | Literal_timestamp n -> D_timestamp n
  | Literal_tez n -> D_tez n
  | Literal_bytes s -> D_bytes s
  | Literal_string s -> D_string s
  | Literal_address s -> D_string s
  | Literal_operation op -> D_operation op
  | Literal_unit -> D_unit

and transpile_environment_element_type : AST.environment_element -> type_value result = fun ele ->
  match (AST.get_type' ele.type_value , ele.definition) with
  | (AST.T_function (f , arg) , ED_declaration (ae , ((_ :: _) as captured_variables)) ) ->
    let%bind f' = translate_type f in
    let%bind arg' = translate_type arg in
    let%bind env' = transpile_environment ae.environment in
    let sub_env = Mini_c.Environment.select captured_variables env' in
    ok @@ Combinators.t_deep_closure sub_env f' arg'
  | _ -> translate_type ele.type_value

and transpile_small_environment : AST.small_environment -> Environment.t result = fun x ->
  let x' = AST.Environment.Small.get_environment x in
  let aux prec (name , (ele : AST.environment_element)) =
    let%bind tv' = transpile_environment_element_type ele in
    ok @@ Environment.add (name , tv') prec
  in
  let%bind result =
    bind_fold_right_list aux Environment.empty x' in
  ok result

and transpile_environment : AST.full_environment -> Environment.t result = fun x ->
  let%bind nlst = bind_map_ne_list transpile_small_environment x in
  ok @@ Environment.concat @@ List.Ne.to_list nlst

and tree_of_sum : AST.type_value -> (type_name * AST.type_value) Append_tree.t result = fun t ->
  let%bind map_tv = get_t_sum t in
  ok @@ Append_tree.of_list @@ kv_list_of_map map_tv

and translate_annotated_expression (ae:AST.annotated_expression) : expression result =
  let%bind tv = translate_type ae.type_annotation in
  let return ?(tv = tv) expr = ok @@ Combinators.Expression.make_tpl (expr, tv) in
  let f = translate_annotated_expression in
  let info =
    let title () = "translating expression" in
    let content () = Format.asprintf "%a" Location.pp ae.location in
    info title content in
  trace info @@
  match ae.expression with
  | E_let_in {binder; rhs; result} ->
    let%bind rhs' = translate_annotated_expression rhs in
    let%bind result' = translate_annotated_expression result in
    return (E_let_in ((binder, rhs'.type_value), rhs', result'))
  | E_failwith ae -> (
      let%bind ae' = translate_annotated_expression ae in
      return @@ E_constant ("FAILWITH" , [ae'])
    )
  | E_literal l -> return @@ E_literal (translate_literal l)
  | E_variable name -> (
      let%bind ele =
        trace_option (corner_case ~loc:__LOC__ "name not in environment") @@
        AST.Environment.get_opt name ae.environment in
      let%bind tv = transpile_environment_element_type ele in
      return ~tv @@ E_variable name
    )
  | E_application (a, b) ->
      let%bind a = translate_annotated_expression a in
      let%bind b = translate_annotated_expression b in
      return @@ E_application (a, b)
  | E_constructor (m, param) -> (
      let%bind param' = translate_annotated_expression param in
      let (param'_expr , param'_tv) = Combinators.Expression.(get_content param' , get_type param') in
      let%bind node_tv =
        trace_strong (corner_case ~loc:__LOC__ "getting lr tree") @@
        tree_of_sum ae.type_annotation in
      let leaf (k, tv) : (expression' option * type_value) result =
        if k = m then (
          let%bind _ =
            trace_strong (corner_case ~loc:__LOC__ "wrong type for constructor parameter")
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
        | (Some _, _), (Some _, _) -> fail @@ corner_case ~loc:__LOC__ "multiple identical constructors in the same variant"
        | (Some v, a), (None, b) -> ok (Some (E_constant ("LEFT", [Combinators.Expression.make_tpl (v, a)])), T_or (a, b))
        | (None, a), (Some v, b) -> ok (Some (E_constant ("RIGHT", [Combinators.Expression.make_tpl (v, b)])), T_or (a, b))
      in
      let%bind (ae_opt, tv) = Append_tree.fold_ne leaf node node_tv in
      let%bind ae =
        trace_option (corner_case ~loc:__LOC__ "inexistant constructor")
          ae_opt in
      return ~tv ae
    )
  | E_tuple lst -> (
      let node = Append_tree.of_list lst in
      let aux (a:expression result) (b:expression result) : expression result =
        let%bind a = a in
        let%bind b = b in
        let a_ty = Combinators.Expression.get_type a in
        let b_ty = Combinators.Expression.get_type b in
        let tv = T_pair (a_ty , b_ty) in
        return ~tv @@ E_constant ("PAIR", [a; b])
      in
      Append_tree.fold_ne (translate_annotated_expression) aux node
    )
  | E_tuple_accessor (tpl, ind) -> (
      let%bind ty' = translate_type tpl.type_annotation in
      let%bind ty_lst =
        trace_strong (corner_case ~loc:__LOC__ "not a tuple") @@
        get_t_tuple tpl.type_annotation in
      let%bind ty'_lst = bind_map_list translate_type ty_lst in
      let%bind path =
        trace_strong (corner_case ~loc:__LOC__ "tuple access") @@
        tuple_access_to_lr ty' ty'_lst ind in
      let aux = fun pred (ty, lr) ->
        let c = match lr with
          | `Left -> "CAR"
          | `Right -> "CDR" in
        Combinators.Expression.make_tpl (E_constant (c, [pred]) , ty) in
      let%bind tpl' = translate_annotated_expression tpl in
      let expr = List.fold_left aux tpl' path in
      ok expr
    )
  | E_record m -> (
      let node = Append_tree.of_list @@ list_of_map m in
      let aux a b : expression result =
        let%bind a = a in
        let%bind b = b in
        let a_ty = Combinators.Expression.get_type a in
        let b_ty = Combinators.Expression.get_type b in
        let tv = T_pair (a_ty , b_ty) in
        return ~tv @@ E_constant ("PAIR", [a; b])
      in
      trace_strong (corner_case ~loc:__LOC__ "record build") @@
      Append_tree.fold_ne (translate_annotated_expression) aux node
    )
  | E_record_accessor (record, property) ->
      let%bind ty' = translate_type (get_type_annotation record) in
      let%bind ty_smap =
        trace_strong (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record (get_type_annotation record) in
      let%bind ty'_smap = bind_map_smap translate_type ty_smap in
      let%bind path =
        trace_strong (corner_case ~loc:__LOC__ "record access") @@
        record_access_to_lr ty' ty'_smap property in
      let aux = fun pred (ty, lr) ->
        let c = match lr with
          | `Left -> "CAR"
          | `Right -> "CDR" in
        Combinators.Expression.make_tpl (E_constant (c, [pred]) , ty) in
      let%bind record' = translate_annotated_expression record in
      let expr = List.fold_left aux record' path in
      ok expr
  | E_constant (name , lst) -> (
      let (iter , map) =
        let iterator name = fun (lst : AST.annotated_expression list) -> match lst with
          | [i ; f] -> (
              let%bind f' = match f.expression with
                | E_lambda l -> (
                    let%bind body' = translate_annotated_expression l.result in
                    let%bind input' = translate_type l.input_type in
                    ok ((l.binder , input') , body')
                  )
                | E_variable v -> (
                    let%bind elt =
                      trace_option (corner_case ~loc:__LOC__ "missing var") @@
                      AST.Environment.get_opt v f.environment in
                    match elt.definition with
                    | ED_declaration (f , _) -> (
                        match f.expression with
                        | E_lambda l -> (
                            let%bind body' = translate_annotated_expression l.result in
                            let%bind input' = translate_type l.input_type in
                            ok ((l.binder , input') , body')
                          )
                        | _ -> fail @@ unsupported_iterator f.location
                      )
                    | _ -> fail @@ unsupported_iterator f.location
                  )
                | _ -> fail @@ unsupported_iterator f.location
              in
              let%bind i' = translate_annotated_expression i in
              return @@ E_iterator (name , f' , i')
            )
          | _ -> fail @@ corner_case ~loc:__LOC__ "bad iterator arity"
        in
        iterator "ITER" , iterator "MAP" in
      match (name , lst) with
      | ("SET_ITER" , lst) -> iter lst
      | ("LIST_ITER" , lst) -> iter lst
      | ("MAP_ITER" , lst) -> iter lst
      | ("LIST_MAP" , lst) -> map lst
      | ("MAP_MAP" , lst) -> map lst
      | _ -> (
          let%bind lst' = bind_map_list (translate_annotated_expression) lst in
          return @@ E_constant (name , lst')
        )
    )
  | E_lambda l ->
    let%bind env =
      trace_strong (corner_case ~loc:__LOC__ "environment") @@
      transpile_environment ae.environment in
    translate_lambda env l
  | E_list lst -> (
      let%bind t =
        trace_strong (corner_case ~loc:__LOC__ "not a list") @@
        Mini_c.Combinators.get_t_list tv in
      let%bind lst' = bind_map_list (translate_annotated_expression) lst in
      let aux : expression -> expression -> expression result = fun prev cur ->
        return @@ E_constant ("CONS", [cur ; prev]) in
      let%bind (init : expression) = return @@ E_make_empty_list t in
      bind_fold_right_list aux init lst'
    )
  | E_set lst -> (
      let%bind t =
        trace_strong (corner_case ~loc:__LOC__ "not a set") @@
        Mini_c.Combinators.get_t_set tv in
      let%bind lst' = bind_map_list (translate_annotated_expression) lst in
      let aux : expression -> expression -> expression result = fun prev cur ->
        return @@ E_constant ("SET_ADD", [cur ; prev]) in
      let%bind (init : expression) = return @@ E_make_empty_set t in
      bind_fold_list aux init lst'
    )
  | E_map m -> (
      let%bind (src, dst) =
        trace_strong (corner_case ~loc:__LOC__ "not a map") @@
        Mini_c.Combinators.get_t_map tv in
      let aux : expression result -> (AST.ae * AST.ae) -> expression result = fun prev (k, v) ->
        let%bind prev' = prev in
        let%bind (k', v') =
          let v' = e_a_some v ae.environment in
          bind_map_pair (translate_annotated_expression) (k , v') in
        return @@ E_constant ("UPDATE", [k' ; v' ; prev'])
      in
      let init = return @@ E_make_empty_map (src, dst) in
      List.fold_left aux init m
    )
  | E_look_up dsi -> (
      let%bind (ds', i') = bind_map_pair f dsi in
      return @@ E_constant ("MAP_GET", [i' ; ds'])
    )
  | E_sequence (a , b) -> (
      let%bind a' = translate_annotated_expression a in
      let%bind b' = translate_annotated_expression b in
      return @@ E_sequence (a' , b')
    )
  | E_loop (expr , body) -> (
      let%bind expr' = translate_annotated_expression expr in
      let%bind body' = translate_annotated_expression body in
      return @@ E_while (expr' , body')
    )
  | E_assign (typed_name , path , expr) -> (
      let ty = typed_name.type_value in
      let aux : ((AST.type_value * [`Left | `Right] list) as 'a) -> AST.access -> 'a result =
        fun (prev, acc) cur ->
          let%bind ty' = translate_type prev in
          match cur with
          | Access_tuple ind -> (
              let%bind ty_lst =
                trace_strong (corner_case ~loc:__LOC__ "not a tuple") @@
                AST.Combinators.get_t_tuple prev in
              let%bind ty'_lst = bind_map_list translate_type ty_lst in
              let%bind path = tuple_access_to_lr ty' ty'_lst ind in
              let path' = List.map snd path in
              ok (List.nth ty_lst ind, acc @ path')
            )
          | Access_record prop -> (
            let%bind ty_map =
                trace_strong (corner_case ~loc:__LOC__ "not a record") @@
                AST.Combinators.get_t_record prev in
              let%bind ty'_map = bind_map_smap translate_type ty_map in
              let%bind path = record_access_to_lr ty' ty'_map prop in
              let path' = List.map snd path in
            ok (Map.String.find prop ty_map, acc @ path')
          )
          | Access_map _k -> fail (corner_case ~loc:__LOC__ "no patch for map yet")
      in
      let%bind (_, path) = bind_fold_right_list aux (ty, []) path in
      let%bind expr' = translate_annotated_expression expr in
      return (E_assignment (typed_name.type_name, path, expr'))
    )
  | E_matching (expr, m) -> (
      let%bind expr' = translate_annotated_expression expr in
      match m with
      | Match_bool {match_true ; match_false} ->
          let%bind (t , f) = bind_map_pair (translate_annotated_expression) (match_true, match_false) in
          return @@ E_if_bool (expr', t, f)
      | Match_option { match_none; match_some = ((name, tv), s) } ->
          let%bind n = translate_annotated_expression match_none in
          let%bind (tv' , s') =
            let%bind tv' = translate_type tv in
            let%bind s' = translate_annotated_expression s in
            ok (tv' , s') in
          return @@ E_if_none (expr' , n , ((name , tv') , s'))
      | Match_variant (lst , variant) -> (
          let%bind tree =
            trace_strong (corner_case ~loc:__LOC__ "getting lr tree") @@
            tree_of_sum variant in
          let%bind tree' = match tree with
            | Empty -> fail (corner_case ~loc:__LOC__ "match empty variant")
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

          let rec aux top t =
            match t with
            | ((`Leaf constructor_name) , tv) -> (
                let%bind ((_ , name) , body) =
                  trace_option (corner_case ~loc:__LOC__ "missing match clause") @@
                  List.find_opt (fun ((constructor_name' , _) , _) -> constructor_name' = constructor_name) lst in
                let%bind body' = translate_annotated_expression body in
                return @@ E_let_in ((name , tv) , top , body')
              )
            | ((`Node (a , b)) , tv) ->
                let%bind a' =
                  let%bind a_ty = get_t_left tv in
                  let a_var = "left" , a_ty in
                  let%bind e = aux (((Expression.make (E_variable "left") a_ty))) a in
                  ok (a_var , e)
                in
                let%bind b' =
                  let%bind b_ty = get_t_right tv in
                  let b_var = "right" , b_ty in
                  let%bind e = aux (((Expression.make (E_variable "right") b_ty))) b in
                  ok (b_var , e)
                in
                return @@ E_if_left (top , a' , b')
          in
          trace_strong (corner_case ~loc:__LOC__ "building constructor") @@
          aux expr' tree''
        )
      | AST.Match_list _ -> fail @@ unsupported_pattern_matching "list" ae.location
      | AST.Match_tuple _ -> fail @@ unsupported_pattern_matching "tuple" ae.location
    )

and translate_lambda_deep : Mini_c.Environment.t -> AST.lambda -> Mini_c.expression result = fun env l ->
  let { binder ; input_type ; output_type ; result } : AST.lambda = l in
  (* Deep capture. Capture the relevant part of the environment. *)
  let%bind c_env =
    let free_variables = Ast_typed.Free_variables.lambda [] l in
    let sub_env = Mini_c.Environment.select free_variables env in
    ok sub_env in
  let%bind (f_expr , input_tv , output_tv) =
    let%bind raw_input = translate_type input_type in
    let%bind output = translate_type output_type in
    let%bind result = translate_annotated_expression result in
    let f_literal = D_function { binder ; result } in
    let expr' = E_literal f_literal in
    ok (expr' , raw_input , output) in
  let tv = Mini_c.t_deep_closure c_env input_tv output_tv in
  ok @@ Expression.make_tpl (f_expr , tv)

and translate_lambda env l =
  let { binder ; input_type ; output_type ; result } : AST.lambda = l in
  (* Try to translate it in an empty env, if it succeeds, transpiles it as a quote value, else, as a closure expression. *)
  let fvs = AST.Free_variables.(annotated_expression (singleton binder) result) in
  let%bind result =
    match fvs with
    | [] -> (
        let%bind result' = translate_annotated_expression result in
        let result' = ez_e_return result' in
        let%bind input = translate_type input_type in
        let%bind output = translate_type output_type in
        let tv = Combinators.t_function input output in
        let content = D_function {binder;result=result'} in
        ok @@ Combinators.Expression.make_tpl (E_literal content , tv)
      )
    | _ -> (
        translate_lambda_deep env l
      ) in
  ok result

let translate_declaration env (d:AST.declaration) : toplevel_statement result =
  match d with
  | Declaration_constant ({name;annotated_expression} , _) ->
      let%bind expression = translate_annotated_expression annotated_expression in
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

let translate_main (l:AST.lambda) loc : (anon_function * _) result =
  let%bind expr = translate_lambda Environment.empty l in
  match expr.content , expr.type_value with
  | E_literal (D_function f) , T_function ty -> ok (f , ty)
  | _ -> fail @@ not_functional_main loc

(* From an expression [expr], build the expression [fun () -> expr] *)
let functionalize (e:AST.annotated_expression) : AST.lambda * AST.type_value =
  let t = e.type_annotation in
  let open! AST in
  {
    binder = "_" ;
    input_type = Combinators.t_unit () ;
    output_type = t ;
    result = e ;
  }, Combinators.(t_function (t_unit ()) t ())

let translate_entry (lst:AST.program) (name:string) : (anon_function * _) result =
  let rec aux acc (lst:AST.program) =
    let%bind acc = acc in
    match lst with
    | [] -> fail @@ missing_entry_point name
    | hd :: tl -> (
        let (AST.Declaration_constant (an , (pre_env , _))) = temp_unwrap_loc hd in
        match an.name = name with
        | false -> (
            let next = fun expr ->
              let cur = e_a_let_in an.name an.annotated_expression expr pre_env in
              acc cur in
            aux (ok next) tl
          )
        | true -> (
            match an.annotated_expression.expression with
            | E_lambda l ->
              let l' = { l with result = acc l.result } in
              translate_main l' an.annotated_expression.location
            | _ ->
              let (l , _) = functionalize an.annotated_expression in
              let l' = { l with result = acc l.result } in
              translate_main l' an.annotated_expression.location
          )
      )
  in
  let%bind l = aux (ok (fun x -> x)) lst in
  ok l

open Combinators

let extract_constructor (v : value) (tree : _ Append_tree.t') : (string * value * AST.type_value) result =
  let open Append_tree in
  let rec aux tv : (string * value * AST.type_value) result=
    match tv with
    | Leaf (k, t), v -> ok (k, v, t)
    | Node {a}, D_left v -> aux (a, v)
    | Node {b}, D_right v -> aux (b, v)
    | _ -> fail @@ internal_assertion_failure "bad constructor path"
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
    | _ -> fail @@ internal_assertion_failure "bad tuple path"
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
    | _ -> fail @@ internal_assertion_failure "bad record path"
  in
  aux (tree, v)

let rec untranspile (v : value) (t : AST.type_value) : AST.annotated_expression result =
  let open! AST in
  let return e = ok (make_a_e_empty e t) in
  match t.type_value' with
  | T_constant ("unit", []) -> (
      let%bind () =
        trace_strong (wrong_mini_c_value "unit" v) @@
        get_unit v in
      return (E_literal Literal_unit)
    )
  | T_constant ("bool", []) -> (
      let%bind b =
        trace_strong (wrong_mini_c_value "bool" v) @@
        get_bool v in
      return (E_literal (Literal_bool b))
    )
  | T_constant ("int", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "int" v) @@
        get_int v in
      return (E_literal (Literal_int n))
    )
  | T_constant ("nat", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "nat" v) @@
        get_nat v in
      return (E_literal (Literal_nat n))
    )
  | T_constant ("timestamp", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "timestamp" v) @@
        get_timestamp v in      
      return (E_literal (Literal_timestamp n))
    )
  | T_constant ("tez", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "tez" v) @@
        get_nat v in
      return (E_literal (Literal_tez n))
    )
  | T_constant ("string", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "string" v) @@
        get_string v in
      return (E_literal (Literal_string n))
    )
  | T_constant ("address", []) -> (
      let%bind n =
        trace_strong (wrong_mini_c_value "address" v) @@
        get_string v in
      return (E_literal (Literal_address n))
    )
  | T_constant ("option", [o]) -> (
      let%bind opt =
        trace_strong (wrong_mini_c_value "option" v) @@
        get_option v in
      match opt with
      | None -> ok (e_a_empty_none o)
      | Some s ->
          let%bind s' = untranspile s o in
          ok (e_a_empty_some s')
    )
  | T_constant ("map", [k_ty;v_ty]) -> (
      let%bind lst =
        trace_strong (wrong_mini_c_value "map" v) @@
        get_map v in
      let%bind lst' =
        let aux = fun (k, v) ->
          let%bind k' = untranspile k k_ty in
          let%bind v' = untranspile v v_ty in
          ok (k', v') in
        bind_map_list aux lst in
      return (E_map lst')
    )
  | T_constant ("list", [ty]) -> (
      let%bind lst =
        trace_strong (wrong_mini_c_value "list" v) @@
        get_list v in
      let%bind lst' =
        let aux = fun e -> untranspile e ty in
        bind_map_list aux lst in
      return (E_list lst')
    )
  | T_constant ("set", [ty]) -> (
      let%bind lst =
        trace_strong (wrong_mini_c_value "set" v) @@
        get_set v in
      let%bind lst' =
        let aux = fun e -> untranspile e ty in
        bind_map_list aux lst in
      return (E_set lst')
    )
  | T_constant ("contract" , [_ty]) ->
    fail @@ bad_untranspile "contract" v
  | T_constant ("operation" , []) -> (
      let%bind op =
        trace_strong (wrong_mini_c_value "operation" v) @@
        get_operation v in
      return (E_literal (Literal_operation op))
    )
  | T_constant (name , _lst) ->
    fail @@ unknown_untranspile name v
  | T_sum m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty sum type"
        | Full t -> ok t
      in
      let%bind (name, v, tv) =
        trace_strong (corner_case ~loc:__LOC__ "sum extract constructor") @@
        extract_constructor v node in
      let%bind sub = untranspile v tv in
      return (E_constructor (name, sub))
  | T_tuple lst ->
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty tuple"
        | Full t -> ok t in
      let%bind tpl =
        trace_strong (corner_case ~loc:__LOC__ "tuple extract") @@
        extract_tuple v node in
      let%bind tpl' = bind_list
        @@ List.map (fun (x, y) -> untranspile x y) tpl in
      return (E_tuple tpl')
  | T_record m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> fail @@ corner_case ~loc:__LOC__ "empty record"
        | Full t -> ok t in
      let%bind lst =
        trace_strong (corner_case ~loc:__LOC__ "record extract") @@
        extract_record v node in
      let%bind lst = bind_list
        @@ List.map (fun (x, (y, z)) -> let%bind yz = untranspile y z in ok (x, yz)) lst in
      let m' = map_of_kv_list lst in
      return (E_record m')
  | T_function _ -> fail @@ bad_untranspile "function" v
