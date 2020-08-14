module I = Ast_sugar
module O = Ast_core
open Trace

open Errors

let cast_var = Location.map Var.todo_cast

let rec compile_type_expression : I.type_expression -> (O.type_expression , desugaring_error) result =
  fun te ->
  let return tc = ok @@ O.make_t ~loc:te.location ~sugar:te tc in
  match te.type_content with
    | I.T_sum sum -> 
      let%bind sum = 
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type ; michelson_annotation ; decl_pos} : I.row_element = v in
          let%bind associated_type = compile_type_expression associated_type in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          ok @@ v'
        ) sum
      in
      return @@ O.T_sum sum
    | I.T_record record -> 
      let%bind record = 
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type ; michelson_annotation ; decl_pos} : I.row_element = v in
          let%bind associated_type = compile_type_expression associated_type in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          ok @@ v'
        ) record
      in
      return @@ O.T_record record
    | I.T_tuple tuple ->
      let aux (i,acc) el = 
        let%bind el = compile_type_expression el in
        ok @@ (i+1,(O.Label (string_of_int i), ({associated_type=el;michelson_annotation=None;decl_pos=0}:O.row_element))::acc) in
      let%bind (_, lst ) = bind_fold_list aux (0,[]) tuple in
      let record = O.LMap.of_list lst in
      return @@ O.T_record record
    | I.T_arrow {type1;type2} ->
      let%bind type1 = compile_type_expression type1 in
      let%bind type2 = compile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | I.T_variable type_variable -> return @@ T_variable (Var.todo_cast type_variable)
    | I.T_wildcard -> return @@ T_wildcard
    | I.T_constant (type_constant, arguments) ->
      let%bind arguments = bind_map_list compile_type_expression arguments in
      let type_constant = type_constant in
      return @@ T_constant {type_constant ; arguments}

let rec compile_expression : I.expression -> (O.expression , desugaring_error) result =
  fun sugar ->
  let return expr = ok @@ O.make_e ~loc:sugar.location ~sugar expr in
  match sugar.expression_content with
    | I.E_literal literal -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} -> 
      let cons_name = cons_name in
      let%bind arguments = bind_map_list compile_expression arguments in
      return @@ O.E_constant {cons_name;arguments}
    | I.E_variable name -> return @@ O.E_variable (cast_var name)
    | I.E_application {lamb;args} -> 
      let%bind lamb = compile_expression lamb in
      let%bind args = compile_expression args in
      return @@ O.E_application {lamb; args}
    | I.E_lambda lambda ->
      let%bind lambda = compile_lambda lambda in
      return @@ O.E_lambda lambda
    | I.E_recursive {fun_name;fun_type;lambda} ->
      let fun_name = cast_var fun_name in
      let%bind fun_type = compile_type_expression fun_type in
      let%bind lambda = compile_lambda lambda in
      return @@ O.E_recursive {fun_name;fun_type;lambda}
    | I.E_let_in {let_binder;inline;rhs;let_result} ->
      let (binder,ty_opt) = let_binder in
      let binder = cast_var binder in
      let%bind ascr = bind_map_option compile_type_expression ty_opt in
      let%bind rhs = compile_expression rhs in
      let%bind let_result = compile_expression let_result in
      return @@ O.E_let_in {let_binder= {binder;ascr} ;inline;rhs;let_result}
    | I.E_raw_code {language;code} ->
      let%bind code = compile_expression code in
      return @@ O.E_raw_code {language;code} 
    | I.E_constructor {constructor;element} ->
      let constructor = constructor in
      let%bind element = compile_expression element in
      return @@ O.E_constructor {constructor;element}
    | I.E_matching {matchee; cases} ->
      let%bind matchee = compile_expression matchee in
      compile_matching sugar matchee cases
    | I.E_record record ->
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (I.Label k,v) ->
          let%bind v =compile_expression v in
          ok @@ (O.Label k,v)
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
      return @@ O.E_matching {
        matchee ;
        cases = Match_variant ([
          {constructor=Label "true"; proj=muted; body=match_true} ;
          {constructor=Label "false"; proj=muted; body=match_false} ;
        ])}
    | I.E_sequence {expr1; expr2} ->
      let%bind expr1 = compile_expression expr1 in
      let%bind expr2 = compile_expression expr2 in
      let let_binder : O.let_binder = {binder = Location.wrap @@ Var.of_name "_" ; ascr = Some (O.t_unit ())} in
      return @@ O.E_let_in {let_binder; rhs=expr1;let_result=expr2; inline=false}
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
    let binder = cast_var binder in
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
      let hd = cast_var hd in
      let tl = cast_var tl in
      let%bind body = compile_expression expr in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_list {match_nil; match_cons={hd;tl;body}}
    | I.Match_option {match_none;match_some} ->
      let%bind match_none = compile_expression match_none in
      let (opt,body) = match_some in
      let opt = cast_var opt in
      let%bind body = compile_expression body in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_option {match_none; match_some={opt;body}}
    | I.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ((c,proj),expr) ->
          let%bind body = compile_expression expr in
          ok @@ ({constructor=c;proj=cast_var proj;body} : O.match_variant)
      ) lst 
      in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_variant lst
    | I.Match_record (fields,associated_types, expr) ->
      let combine fields associated_types =
        match associated_types with
          Some ft -> List.combine fields @@ List.map (fun x -> Some x) ft
        | None    -> List.map (fun x -> (x, None)) fields
      in
      let%bind next   = compile_expression expr in
      let%bind associated_types = bind_map_option (bind_map_list compile_type_expression) associated_types in
      let aux ((index,expr) : int * _ ) ((I.Label field,(ev,topt)): (I.label * (I.expression_variable * O.type_expression option))) =
        let ev = cast_var ev in
        let f = fun expr' -> O.e_let_in ~sugar (ev,topt) false (O.e_record_accessor ~sugar e (O.Label field)) expr' in
        (index+1, fun expr' -> expr (f expr'))
      in
      let (_,header) = List.fold_left aux (0, fun e -> e) @@
        List.map (fun ((a,b),c) -> (a,(b,c))) @@
        combine fields associated_types
      in
      ok @@ header next
    | I.Match_tuple (fields,associated_types, expr) ->
      let combine fields associated_types =
        match associated_types with
          Some ft -> List.combine fields @@ List.map (fun x -> Some x) ft
        | None    -> List.map (fun x -> (x, None)) fields
      in
      let%bind next   = compile_expression expr in
      let%bind associated_types = bind_map_option (bind_map_list compile_type_expression) associated_types in
      let aux ((index,expr) : int * _ ) ((ev,topt): I.expression_variable * O.type_expression option) =
        let ev = cast_var ev in
        let f = fun expr' -> O.e_let_in ~sugar (ev,topt) false (O.e_record_accessor ~sugar e (Label (string_of_int index))) expr' in
        (index+1, fun expr' -> expr (f expr'))
      in
      let (_,header) = List.fold_left aux (0, fun e -> e) @@
        combine fields associated_types
      in
      ok @@ header next
    | I.Match_variable (a, ty_opt, expr) ->
      let a = cast_var a in
      let%bind ty_opt = bind_map_option compile_type_expression ty_opt in
      let%bind expr = compile_expression expr in
      ok @@ O.e_let_in ~sugar (a,ty_opt) false e expr
 
let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | I.Declaration_constant (binder, te_opt, inline, expr) ->
    let binder = cast_var binder in
    let%bind expr = compile_expression expr in
    let%bind type_opt = bind_map_option compile_type_expression te_opt in
    return @@ O.Declaration_constant {binder; type_opt; attr={inline}; expr}
  | I.Declaration_type (type_binder, type_expr) ->
    let type_binder = Var.todo_cast type_binder in
    let%bind type_expr = compile_type_expression type_expr in
    return @@ O.Declaration_type {type_binder ; type_expr}

let compile_program : I.program -> (O.program , desugaring_error) result =
  fun p ->
  bind_map_list compile_declaration p
