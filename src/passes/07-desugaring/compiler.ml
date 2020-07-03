module I = Ast_sugar
module O = Ast_core
open Trace

open Errors

let rec compile_type_expression : I.type_expression -> (O.type_expression , desugaring_error) result =
  fun te ->
  let return tc = ok @@ O.make_t ~loc:te.location ~sugar:te tc in
  match te.type_content with
    | I.T_sum sum -> 
      let sum = I.CMap.to_kv_list sum in
      let%bind sum = 
        bind_map_list (fun (k,v) ->
          let {ctor_type ; michelson_annotation ; ctor_decl_pos} : I.ctor_content = v in
          let%bind ctor_type = compile_type_expression ctor_type in
          let v' : O.ctor_content = {ctor_type ; michelson_annotation ; ctor_decl_pos} in
          ok @@ (k,v')
        ) sum
      in
      return @@ O.T_sum (O.CMap.of_list sum)
    | I.T_record record -> 
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let {field_type ; michelson_annotation ; field_decl_pos} : I.field_content = v in
          let%bind field_type = compile_type_expression field_type in
          let v' : O.field_content = {field_type ; field_annotation=michelson_annotation ; field_decl_pos} in
          ok @@ (k,v')
        ) record
      in
      return @@ O.T_record (O.LMap.of_list record)
    | I.T_tuple tuple ->
      let aux (i,acc) el = 
        let%bind el = compile_type_expression el in
        ok @@ (i+1,(O.Label (string_of_int i), ({field_type=el;field_annotation=None;field_decl_pos=0}:O.field_content))::acc) in
      let%bind (_, lst ) = bind_fold_list aux (0,[]) tuple in
      let record = O.LMap.of_list lst in
      return @@ O.T_record record
    | I.T_arrow {type1;type2} ->
      let%bind type1 = compile_type_expression type1 in
      let%bind type2 = compile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | I.T_variable type_variable -> return @@ T_variable type_variable 
    | I.T_constant type_constant -> return @@ T_constant type_constant
    | I.T_operator (type_operator, lst) ->
      let%bind lst = bind_map_list compile_type_expression lst in
      return @@ T_operator (type_operator, lst)

let rec compile_expression : I.expression -> (O.expression , desugaring_error) result =
  fun sugar ->
  let return expr = ok @@ O.make_e ~loc:sugar.location ~sugar expr in
  match sugar.expression_content with
    | I.E_literal literal   -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} -> 
      let%bind arguments = bind_map_list compile_expression arguments in
      return @@ O.E_constant {cons_name;arguments}
    | I.E_variable name     -> return @@ O.E_variable name
    | I.E_application {lamb;args} -> 
      let%bind lamb = compile_expression lamb in
      let%bind args = compile_expression args in
      return @@ O.E_application {lamb; args}
    | I.E_lambda lambda ->
      let%bind lambda = compile_lambda lambda in
      return @@ O.E_lambda lambda
    | I.E_recursive {fun_name;fun_type;lambda} ->
      let%bind fun_type = compile_type_expression fun_type in
      let%bind lambda = compile_lambda lambda in
      return @@ O.E_recursive {fun_name;fun_type;lambda}
    | I.E_let_in {let_binder;inline;rhs;let_result} ->
      let (binder,ty_opt) = let_binder in
      let%bind ty_opt = bind_map_option compile_type_expression ty_opt in
      let%bind rhs = compile_expression rhs in
      let%bind let_result = compile_expression let_result in
      return @@ O.E_let_in {let_binder=(binder,ty_opt);inline;rhs;let_result}
    | I.E_raw_code {language;code} ->
      let%bind code = compile_expression code in
      return @@ O.E_raw_code {language;code} 
    | I.E_constructor {constructor;element} ->
      let%bind element = compile_expression element in
      return @@ O.E_constructor {constructor;element}
    | I.E_matching {matchee; cases} ->
      let%bind matchee = compile_expression matchee in
      compile_matching sugar matchee cases
    | I.E_record record ->
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let%bind v =compile_expression v in
          ok @@ (k,v)
        ) record
      in
      return @@ O.E_record (O.LMap.of_list record)
    | I.E_accessor {record;path} ->
      let%bind record = compile_expression record in
      let accessor ?loc expr a =
        match a with 
          I.Access_tuple  i -> ok @@ O.e_record_accessor ?loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ?loc expr (Label a)
        | I.Access_map k -> 
          let%bind k = compile_expression k in
          ok @@ O.e_constant ?loc C_MAP_FIND_OPT [k;expr]
      in
      bind_fold_list accessor record path
    | I.E_update {record;path;update} ->
      let%bind record = compile_expression record in
      let%bind update = compile_expression update in
      let accessor ?loc expr a =
        match a with 
          I.Access_tuple  i -> ok @@ O.e_record_accessor ?loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ?loc expr (Label a)
        | I.Access_map k -> 
          let%bind k = compile_expression k in
          ok @@ O.e_constant ?loc C_MAP_FIND_OPT [k;expr]
      in
      let updator ?loc (s:O.expression) a expr =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_update ?loc s (Label (Z.to_string i)) expr
        | I.Access_record a -> ok @@ O.e_record_update ?loc s (Label a) expr
        | I.Access_map k ->
          let%bind k = compile_expression k in
          ok @@ O.e_constant ?loc C_MAP_ADD [k;expr;s]
      in
      let aux (s, e : O.expression * _) lst =
        let%bind s' = accessor ~loc:s.location s lst in
        let e' = fun expr -> 
          let%bind u = updator ~loc:s.location s lst (expr)
          in e u 
        in
        ok @@ (s',e')
      in
      let%bind (_,rhs) = bind_fold_list aux (record, fun e -> ok @@ e) path in
      rhs @@ update
    | I.E_map map -> (
      let map = List.sort_uniq compare map in
      let aux = fun prev (k, v) ->
        let%bind (k', v') = bind_map_pair (compile_expression) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let%bind init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init map
    )
    | I.E_big_map big_map -> (
      let big_map = List.sort_uniq compare big_map in
      let aux = fun prev (k, v) ->
        let%bind (k', v') = bind_map_pair (compile_expression) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let%bind init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init big_map
    )
    | I.E_list lst ->
      let%bind lst' = bind_map_list (compile_expression) lst in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
      let%bind init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
      bind_fold_right_list aux init lst'
    | I.E_set set -> (
      let%bind lst' = bind_map_list (compile_expression) set in
      let lst' = List.sort_uniq compare lst' in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
      let%bind init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
      bind_fold_list aux init lst'
      )
    | I.E_ascription {anno_expr; type_annotation} ->
      let%bind anno_expr = compile_expression anno_expr in
      let%bind type_annotation = compile_type_expression type_annotation in
      return @@ O.E_ascription {anno_expr; type_annotation}
    | I.E_cond {condition; then_clause; else_clause} ->
      let%bind matchee = compile_expression condition in
      let%bind match_true = compile_expression then_clause in
      let%bind match_false = compile_expression else_clause in
      let muted = Location.wrap @@ Var.of_name "_" in
      return @@ O.E_matching {matchee; cases=Match_variant ([((Constructor "true", muted), match_true);((Constructor "false", muted), match_false)])}
    | I.E_sequence {expr1; expr2} ->
      let%bind expr1 = compile_expression expr1 in
      let%bind expr2 = compile_expression expr2 in
      return @@ O.E_let_in {let_binder=(Location.wrap @@ Var.of_name "_", Some (O.t_unit ())); rhs=expr1;let_result=expr2; inline=false}
    | I.E_skip -> ok @@ O.e_unit ~loc:sugar.location ~sugar ()
    | I.E_tuple t ->
      let aux (i,acc) el = 
        let%bind el = compile_expression el in
        ok @@ (i+1,(O.Label (string_of_int i), el)::acc) in
      let%bind (_, lst ) = bind_fold_list aux (0,[]) t in
      let m = O.LMap.of_list lst in
      return @@ O.E_record m

and compile_lambda : I.lambda -> (O.lambda , desugaring_error) result =
  fun {binder;input_type;output_type;result}->
    let%bind input_type = bind_map_option compile_type_expression input_type in
    let%bind output_type = bind_map_option compile_type_expression output_type in
    let%bind result = compile_expression result in
    ok @@ O.{binder;input_type;output_type;result}
and compile_matching : I.expression -> O.expression -> I.matching_expr -> (O.expression, desugaring_error) result =
  fun sugar e m -> 
  let loc = sugar.location in
  match m with 
    | I.Match_list {match_nil;match_cons} ->
      let%bind match_nil = compile_expression match_nil in
      let (hd,tl,expr) = match_cons in
      let%bind expr = compile_expression expr in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_list {match_nil; match_cons=(hd,tl,expr)}
    | I.Match_option {match_none;match_some} ->
      let%bind match_none = compile_expression match_none in
      let (n,expr) = match_some in
      let%bind expr = compile_expression expr in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_option {match_none; match_some=(n,expr)}
    | I.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ((c,n),expr) ->
          let%bind expr = compile_expression expr in
          ok @@ ((c,n),expr)
      ) lst 
      in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_variant lst
    | I.Match_record (fields,field_types, expr) ->
      let combine fields field_types =
        match field_types with
          Some ft -> List.combine fields @@ List.map (fun x -> Some x) ft
        | None    -> List.map (fun x -> (x, None)) fields
      in
      let%bind next   = compile_expression expr in
      let%bind field_types = bind_map_option (bind_map_list compile_type_expression) field_types in
      let aux ((index,expr) : int * _ ) ((field,name): (O.label * (O.expression_variable * O.type_expression option))) =
        let f = fun expr' -> O.e_let_in ~sugar name false (O.e_record_accessor ~sugar e field) expr' in
        (index+1, fun expr' -> expr (f expr'))
      in
      let (_,header) = List.fold_left aux (0, fun e -> e) @@
        List.map (fun ((a,b),c) -> (a,(b,c))) @@
        combine fields field_types
      in
      ok @@ header next
    | I.Match_tuple (fields,field_types, expr) ->
      let combine fields field_types =
        match field_types with
          Some ft -> List.combine fields @@ List.map (fun x -> Some x) ft
        | None    -> List.map (fun x -> (x, None)) fields
      in
      let%bind next   = compile_expression expr in
      let%bind field_types = bind_map_option (bind_map_list compile_type_expression) field_types in
      let aux ((index,expr) : int * _ ) (field: O.expression_variable * O.type_expression option) =
        let f = fun expr' -> O.e_let_in ~sugar field false (O.e_record_accessor ~sugar e (Label (string_of_int index))) expr' in
        (index+1, fun expr' -> expr (f expr'))
      in
      let (_,header) = List.fold_left aux (0, fun e -> e) @@
        combine fields field_types
      in
      ok @@ header next
    | I.Match_variable (a, ty_opt, expr) ->
      let%bind ty_opt = bind_map_option compile_type_expression ty_opt in
      let%bind expr = compile_expression expr in
      ok @@ O.e_let_in ~sugar (a,ty_opt) false e expr
 
let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | I.Declaration_constant (n, te_opt, inline, expr) ->
    let%bind expr = compile_expression expr in
    let%bind te_opt = bind_map_option compile_type_expression te_opt in
    return @@ O.Declaration_constant (n, te_opt, {inline}, expr)
  | I.Declaration_type (n, te) ->
    let%bind te = compile_type_expression te in
    return @@ O.Declaration_type (n,te)

let compile_program : I.program -> (O.program , desugaring_error) result =
  fun p ->
  bind_map_list compile_declaration p
