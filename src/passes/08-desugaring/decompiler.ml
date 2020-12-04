module I = Ast_sugar
module O = Ast_core

open Trace
open Stage_common.Maps
open Errors

let cast_var = Location.map Var.todo_cast

let rec decompile_type_expression : O.type_expression -> (I.type_expression, desugaring_error) result =
  fun te ->
  let self = decompile_type_expression in
  let return te = ok @@ I.make_t te in
  match te.sugar with
    Some te -> ok @@ te
  | None ->
    match te.type_content with
      | O.T_variable type_variable -> return @@ T_variable (Var.todo_cast type_variable)
      | O.T_app tc ->
        let%bind tc = type_app self tc in
        return @@ T_app tc
      | O.T_sum {fields;layout} ->
        let%bind fields =
          Stage_common.Helpers.bind_map_lmap (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let%bind associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ I.row_element = {associated_type;attributes;decl_pos} in
            ok @@ v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" O.PP.layout l))] | None -> [] in
        return @@ I.T_sum {fields ; attributes}
      | O.T_record {fields;layout} ->
        let%bind fields =
          Stage_common.Helpers.bind_map_lmap (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let%bind associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ I.row_element = {associated_type ; attributes ; decl_pos} in
            ok @@ v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" O.PP.layout l))] | None -> [] in
        return @@ I.T_record { fields ; attributes }
      | O.T_arrow arr ->
        let%bind arr = arrow self arr in
        return @@ T_arrow arr
      | O.T_module_accessor ma ->
        let%bind ma = module_access self ma in
        return @@ T_module_accessor ma

let rec decompile_expression : O.expression -> (I.expression, desugaring_error) result =
  fun e ->
  let self = decompile_expression in
  let self_type = decompile_type_expression in
  let return expr = ok @@ I.make_e ~loc:e.location expr in
  match e.sugar with
    Some e -> ok @@ e
  | None ->
    match e.content with
      O.E_literal lit -> return @@ I.E_literal (lit)
    | O.E_constant {cons_name;arguments} ->
      let%bind arguments = bind_map_list self arguments in
      return @@ I.E_constant {cons_name = cons_name;arguments}
    | O.E_variable name -> return @@ I.E_variable (cast_var name)
    | O.E_application app ->
      let%bind app = application self app in
      return @@ I.E_application app
    | O.E_lambda lamb ->
      let%bind lamb = lambda self self_type lamb in
      return @@ I.E_lambda lamb
    | O.E_recursive recs ->
      let%bind recs = recursive self self_type recs in
      return @@ I.E_recursive recs
    | O.E_let_in {let_binder = {var; ascr};inline=false;rhs=expr1;let_result=expr2}
      when Var.equal var.wrap_content (Var.of_name "_")
           && Stdlib.(=) ascr (Some (O.t_unit ())) ->
      let%bind expr1 = self expr1 in
      let%bind expr2 = self expr2 in
      return @@ I.E_sequence {expr1;expr2}
    | O.E_let_in {let_binder;inline;rhs;let_result} ->
      let%bind let_binder = binder self_type let_binder in
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      let attributes = if inline then ["inline"] else [] in
      return @@ I.E_let_in {let_binder;mut=false;attributes;rhs;let_result}
    | O.E_type_in ti ->
      let%bind ti = type_in self self_type ti in
      return @@ I.E_type_in ti
    | O.E_raw_code rc ->
      let%bind rc = raw_code self rc in
      return @@ I.E_raw_code rc
    | O.E_constructor const ->
      let%bind const = constructor self const in
      return @@ I.E_constructor const
    | O.E_matching {matchee; cases} ->
      let%bind matchee = self matchee in
      let%bind cases   = decompile_matching cases in
      return @@ I.E_matching {matchee;cases}
    | O.E_record record ->
      let record = O.LMap.to_kv_list_rev record in
      let%bind record =
        bind_map_list (fun (O.Label k,v) ->
          let%bind v = self v in
          ok @@ (I.Label k,v)
        ) record
      in
      return @@ I.E_record (I.LMap.of_list record)
    | O.E_record_accessor {record;path} ->
      let%bind record = self record in
      let Label path  = path in
      return @@ I.E_accessor {record;path=[I.Access_record path]}
    | O.E_record_update {record;path;update} ->
      let%bind record = self record in
      let%bind update = self update in
      let Label path  = path in
      return @@ I.E_update {record;path=[I.Access_record path];update}
    | O.E_ascription {anno_expr; type_annotation} ->
      let%bind anno_expr = self anno_expr in
      let%bind type_annotation = decompile_type_expression type_annotation in
      return @@ I.E_ascription {anno_expr; type_annotation}
    | O.E_module_accessor ma ->
      let%bind ma = module_access self ma in
      return @@ E_module_accessor ma

and decompile_lambda : _ O.lambda -> (_ I.lambda, desugaring_error) result =
  fun {binder=b;output_type;result}->
    let%bind binder = binder decompile_type_expression b in
    let%bind output_type = bind_map_option decompile_type_expression output_type in
    let%bind result = decompile_expression result in
    ok @@ I.{binder;output_type;result}
and decompile_matching : O.matching_expr -> (I.matching_expr, desugaring_error) result =
  fun m ->
  match m with
    | O.Match_list {match_nil;match_cons = { hd ; tl ; body }} ->
      let hd = cast_var hd in
      let tl = cast_var tl in
      let%bind match_nil = decompile_expression match_nil in
      let%bind expr = decompile_expression body in
      ok @@ I.Match_list {match_nil; match_cons=(hd,tl,expr)}
    | O.Match_option {match_none; match_some = { opt ; body }} ->
      let opt = cast_var opt in
      let%bind match_none = decompile_expression match_none in
      let%bind expr = decompile_expression body in
      ok @@ I.Match_option {match_none; match_some=(opt,expr)}
    | O.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ({ constructor; proj ; body } : O.match_variant) ->
          let%bind expr = decompile_expression body in
          ok @@ ((constructor, cast_var proj),expr)
      ) lst
      in
      ok @@ I.Match_variant lst

let decompile_declaration : O.declaration Location.wrap -> _ result = fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with
  | O.Declaration_type dt ->
    let%bind dt = declaration_type decompile_type_expression dt in
    return @@ I.Declaration_type dt
  | O.Declaration_constant {binder=b; attr={inline}; expr} ->
    let%bind binder = binder decompile_type_expression b in
    let%bind expr = decompile_expression expr in
    let attr = if inline then ["inline"] else [] in
    return @@ I.Declaration_constant {binder; attr; expr}

let decompile_program : O.program -> (I.program, desugaring_error) result = fun prg ->
  program decompile_declaration prg
