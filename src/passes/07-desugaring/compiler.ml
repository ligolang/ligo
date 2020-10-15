module I = Ast_sugar
module O = Ast_core

open Errors
open Trace
open Stage_common.Maps

let cast_var = Location.map Var.todo_cast

let is_michelson_annotation attr =
  if String.length attr > 6 && String.sub attr 0 6 = "annot:" then
    Some (String.sub attr 6 ((String.length attr)-6))
  else None

let is_layout attr =
  if String.length attr > 7 && String.sub attr 0 7 = "layout:" then
    Some (String.sub attr 7 ((String.length attr)-7))
  else None

let is_inline attr = String.equal "inline" attr

let get_michelson_annotation : (string list) -> string option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_michelson_annotation hd with
      | Some ann -> Some ann
      | None -> aux tl
    )
    | [] -> None
  in
  aux attributes

let get_layout : (string list) -> O.layout option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_layout hd with
      | Some "tree" -> Some O.L_tree
      | Some "comb" -> Some O.L_comb
      (*deal with wrong layout*)
      | None | Some _ -> aux tl
    )
    | [] -> None
  in
  aux attributes

let get_inline : (string list) -> bool = List.exists is_inline


let rec compile_type_expression : I.type_expression -> (O.type_expression , desugaring_error) result =
  fun te ->
  let self = compile_type_expression in
  let return tc = ok @@ O.make_t ~loc:te.location ~sugar:te tc in
  match te.type_content with
    | I.T_variable type_variable -> return @@ T_variable (Var.todo_cast type_variable)
    | I.T_constant {type_constant; arguments} ->
      let%bind arguments = bind_map_list self arguments in
      return @@ T_constant {type_constant; arguments}
    | I.T_sum {fields ; attributes} ->
      let%bind fields =
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ I.row_element = v in
          let michelson_annotation = get_michelson_annotation attributes in
          let%bind associated_type = compile_type_expression associated_type in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          ok @@ v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_sum {fields ; layout }
    | I.T_record {fields ; attributes} ->
      let%bind fields =
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ I.row_element = v in
          let%bind associated_type = compile_type_expression associated_type in
          let michelson_annotation = get_michelson_annotation attributes in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          ok @@ v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_record {fields ; layout }
    | I.T_tuple tuple ->
      let aux (i,acc) el =
        let%bind el = self el in
        ok @@ (i+1,(O.Label (string_of_int i), ({associated_type=el;michelson_annotation=None;decl_pos=0}:_ O.row_element_mini_c))::acc) in
      let%bind (_, lst ) = bind_fold_list aux (0,[]) tuple in
      let record = O.LMap.of_list lst in
      return @@ O.T_record {fields = record ; layout = None}
    | I.T_arrow arr ->
      let%bind arr = arrow self arr in
      return @@ T_arrow arr

let compile_binder = binder compile_type_expression

let rec compile_expression : I.expression -> (O.expression , desugaring_error) result =
  fun sugar ->
  let self = compile_expression in
  let self_type = compile_type_expression in
  let return expr = ok @@ O.make_e ~loc:sugar.location ~sugar expr in
  match sugar.expression_content with
    | I.E_literal literal -> return @@ O.E_literal literal
    | I.E_constant cons ->
      let%bind cons = constant self cons in
      return @@ O.E_constant cons
    | I.E_variable name -> return @@ O.E_variable (cast_var name)
    | I.E_application app ->
      let%bind app = application self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let%bind lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_recursive recs ->
      let%bind recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let%bind let_binder = binder self_type let_binder in
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      let inline = get_inline attributes in
      return @@ O.E_let_in {let_binder;inline;rhs;let_result}
    | I.E_raw_code rc ->
      let%bind rc = raw_code self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let%bind const = constructor self const in
      return @@ O.E_constructor const
    | I.E_matching {matchee; cases} ->
      let%bind matchee = self matchee in
      compile_matching sugar matchee cases
    | I.E_record recd ->
      let%bind recd = record self recd in
      return @@ O.E_record recd
    | I.E_accessor {record;path} ->
      let%bind record = self record in
      let accessor ?loc expr a =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_accessor ?loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ?loc expr (Label a)
        | I.Access_map k ->
          let%bind k = self k in
          ok @@ O.e_constant ?loc C_MAP_FIND_OPT [k;expr]
      in
      bind_fold_list accessor record path
    | I.E_update {record;path;update} ->
      let%bind record = self record in
      let%bind update = self update in
      let accessor ?loc expr a =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_accessor ?loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ?loc expr (Label a)
        | I.Access_map k ->
          let%bind k = self k in
          ok @@ O.e_constant ?loc C_MAP_FIND_OPT [k;expr]
      in
      let updator ?loc (s:O.expression) a expr =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_update ?loc s (Label (Z.to_string i)) expr
        | I.Access_record a -> ok @@ O.e_record_update ?loc s (Label a) expr
        | I.Access_map k ->
          let%bind k = self k in
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
        let%bind (k', v') = bind_map_pair (self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let%bind init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init map
    )
    | I.E_big_map big_map -> (
      let big_map = List.sort_uniq compare big_map in
      let aux = fun prev (k, v) ->
        let%bind (k', v') = bind_map_pair (self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let%bind init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init big_map
    )
    | I.E_list lst ->
      let%bind lst' = bind_map_list (self) lst in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
      let%bind init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
      bind_fold_right_list aux init lst'
    | I.E_set set -> (
      let%bind lst' = bind_map_list (self) set in
      let lst' = List.sort_uniq compare lst' in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
      let%bind init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
      bind_fold_list aux init lst'
      )
    | I.E_ascription {anno_expr; type_annotation} ->
      let%bind anno_expr = self anno_expr in
      let%bind type_annotation = self_type type_annotation in
      return @@ O.E_ascription {anno_expr; type_annotation}
    | I.E_cond {condition; then_clause; else_clause} ->
      let%bind matchee = self condition in
      let%bind match_true = self then_clause in
      let%bind match_false = self else_clause in
      let muted = Location.wrap @@ Var.of_name "_" in
      return @@ O.E_matching {
        matchee ;
        cases = Match_variant ([
          {constructor=Label "true"; proj=muted; body=match_true} ;
          {constructor=Label "false"; proj=muted; body=match_false} ;
        ])}
    | I.E_sequence {expr1; expr2} ->
      let%bind expr1 = self expr1 in
      let%bind expr2 = self expr2 in
      let let_binder : _ O.binder = {var = Location.wrap @@ Var.of_name "_" ; ascr = Some (O.t_unit ())} in
      return @@ O.E_let_in {let_binder; rhs=expr1;let_result=expr2; inline=false}
    | I.E_skip -> ok @@ O.e_unit ~loc:sugar.location ~sugar ()
    | I.E_tuple t ->
      let aux (i,acc) el =
        let%bind el = self el in
        ok @@ (i+1,(O.Label (string_of_int i), el)::acc) in
      let%bind (_, lst ) = bind_fold_list aux (0,[]) t in
      let m = O.LMap.of_list lst in
      return @@ O.E_record m

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
    | I.Match_record (fields, expr) ->
      let%bind next   = compile_expression expr in
      let aux ((index,expr) : int * _ ) ((I.Label field, binder): (I.label * _ I.binder)) =
        let%bind binder = compile_binder binder in
        let f = O.e_let_in ~sugar binder false (O.e_record_accessor ~sugar e (O.Label field)) in
        ok @@ (index+1, fun expr' -> expr (f expr'))
      in
      let%bind (_,header) = bind_fold_list aux (0, fun e -> e) @@ fields
      in
      ok @@ header next
    | I.Match_tuple (fields, expr) ->
      let%bind next   = compile_expression expr in
      let aux ((index,expr) : int * _ ) (binder: _ I.binder) =
        let%bind binder = compile_binder binder in
        let f = O.e_let_in ~sugar binder false (O.e_record_accessor ~sugar e (Label (string_of_int index))) in
        ok @@ (index+1, fun expr' -> expr (f expr'))
      in
      let%bind (_,header) = bind_fold_list aux (0, fun e -> e) @@ fields
      in
      ok @@ header next
    | I.Match_variable (a, expr) ->
      let%bind a = compile_binder a in
      let%bind expr = compile_expression expr in
      ok @@ O.e_let_in ~sugar a false e expr

let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with
  | I.Declaration_type dt ->
    let%bind dt = declaration_type compile_type_expression dt in
    return @@ O.Declaration_type dt
  | I.Declaration_constant {binder;attr;expr} ->
    let%bind binder = compile_binder binder in
    let%bind expr = compile_expression expr in
    let inline = get_inline attr in
    return @@ O.Declaration_constant {binder; attr={inline}; expr}

let compile_program : I.program -> (O.program , desugaring_error) result = fun p ->
  program compile_declaration p
