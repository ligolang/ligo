module I = Ast_sugar
module O = Ast_core

module Location = Simple_utils.Location
module Var      = Simple_utils.Var
module Pair     = Simple_utils.Pair

open Ligo_prim

let is_michelson_annotation = String.chop_prefix ~prefix:"annot:"

let is_layout = String.chop_prefix ~prefix:"layout:"

let get_michelson_annotation : (string list) -> string option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_michelson_annotation hd with
      | Some ann -> Some ann
      | None -> aux tl
    )
    | [] -> None
  in
  aux attributes

let get_layout : (string list) -> Layout.t option = fun attributes ->
  let rec aux lst = match lst with
    | hd::tl -> ( match is_layout hd with
      | Some "tree" -> Some Layout.L_tree
      | Some "comb" -> Some Layout.L_comb
      (*deal with wrong layout*)
      | None | Some _ -> aux tl
    )
    | [] -> None
  in
  aux attributes

let compile_exp_attributes : I.Attr.t -> O.ValueAttr.t = fun attributes ->
  let is_inline attr = String.equal "inline" attr in
  let is_no_mutation attr = String.equal "no_mutation" attr in
  let is_view attr = String.equal "view" attr in
  let is_hidden attr = String.equal "hidden" attr in
  let is_thunk attr = String.equal "thunk" attr in
  let get_inline : (string list) -> bool = List.exists ~f:is_inline in
  let get_no_mutation : (string list) -> bool = List.exists ~f:is_no_mutation in
  let get_view : (string list) -> bool = List.exists ~f:is_view in
  let get_hidden : (string list) -> bool = List.exists ~f:is_hidden in
  let get_public : (string list) -> bool = fun attr -> not (List.mem attr "private" ~equal:String.equal) in
  let get_thunk : (string list) -> bool = List.exists ~f:is_thunk in
  let inline = get_inline attributes in
  let no_mutation = get_no_mutation attributes in
  let public = get_public attributes in
  let view = get_view attributes in
  let hidden = get_hidden attributes in
  let thunk = get_thunk attributes in
  {inline; no_mutation; view; public; hidden; thunk}

let compile_type_attributes : I.Attr.t -> O.TypeOrModuleAttr.t = fun attributes ->
  let get_public : (string list) -> bool = fun attr -> not (List.mem attr "private" ~equal:String.equal) in
  let get_hidden : (string list) -> bool = fun attr -> List.mem attr "hidden" ~equal:String.equal in
  let public = get_public attributes in
  let hidden = get_hidden attributes in
  {public;hidden}
let compile_module_attributes : I.Attr.t -> O.TypeOrModuleAttr.t = compile_type_attributes

let rec compile_type_expression : I.type_expression -> O.type_expression =
  fun te ->
  let self = compile_type_expression in
  let return tc = O.make_t ~loc:te.location ~sugar:te tc in
  match te.type_content with
    | I.T_variable type_variable -> return @@ T_variable type_variable
    | I.T_app a ->
      let a' = Type_app.map self a in
      return @@ T_app a'
    | I.T_sum {fields ; attributes} ->
      let fields =
        Record.map (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ Rows.row_element = v in
          let michelson_annotation = get_michelson_annotation attributes in
          let associated_type = compile_type_expression associated_type in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_sum {fields ; layout }
    | I.T_record {fields ; attributes} ->
      let fields =
        Record.map (fun v ->
          let {associated_type ; attributes ; decl_pos} : _ Rows.row_element = v in
          let associated_type = compile_type_expression associated_type in
          let michelson_annotation = get_michelson_annotation attributes in
          let v' : O.row_element = {associated_type ; michelson_annotation ; decl_pos} in
          v'
        ) fields
      in
      let layout = get_layout attributes in
      return @@ O.T_record {fields ; layout }
    | I.T_tuple tuple ->
      let aux (i,acc) el =
        let el = self el in
        (i+1,(Label.of_int i, ({associated_type=el;michelson_annotation=None;decl_pos=i}:_ Rows.row_element_mini_c))::acc) in
      let (_, lst ) = List.fold ~f:aux ~init:(0,[]) tuple in
      let record = Record.of_list lst in
      return @@ O.T_record {fields = record ; layout = None}
    | I.T_arrow arr ->
      let arr = Arrow.map self arr in
      return @@ T_arrow arr
    | I.T_module_accessor ma -> return @@ O.T_module_accessor ma
    | I.T_singleton x ->
      return @@ O.T_singleton x
    | I.T_abstraction x ->
      let type_ = self x.type_ in
      return @@ O.T_abstraction { x with type_ }
    | I.T_for_all x ->
      let type_ = self x.type_ in
      return @@ O.T_for_all { x with type_ }

let compile_type_expression_option = Option.map ~f:compile_type_expression
let compile_binder = Binder.map compile_type_expression_option

let rec compile_expression : I.expression -> O.expression =
  fun sugar ->
  let self = compile_expression in
  let self_type = compile_type_expression in
  let self_type_opt = compile_type_expression_option in
  let return expr = O.make_e ~loc:sugar.location ~sugar expr in
  match sugar.expression_content with
    | I.E_literal literal -> return @@ O.E_literal literal
    | I.E_constant cons ->
      let cons = Constant.map self cons in
      return @@ O.E_constant cons
    | I.E_variable name -> return @@ O.E_variable name
    | I.E_application app ->
      let app = Application.map self app in
      return @@ O.E_application app
    | I.E_lambda lamb ->
      let lamb = Lambda.map self self_type_opt lamb in
      return @@ O.E_lambda lamb
    | I.E_type_abstraction ty_abs ->
      let ty_abs = Type_abs.map self ty_abs in
      return @@ O.E_type_abstraction ty_abs
    | I.E_recursive recs ->
      let recs = Recursive.map self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;attributes;rhs;let_result} ->
      let let_binder = Binder.map self_type_opt let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      let attr = compile_exp_attributes attributes in
      return @@ O.E_let_in {let_binder;attr;rhs;let_result}
    | I.E_type_in {type_binder; rhs; let_result} ->
      let rhs = self_type rhs in
      let let_result = self let_result in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    | I.E_mod_in {module_binder;rhs;let_result} ->
      let rhs = compile_module_expr rhs in
      let let_result = self let_result in
      return @@ O.E_mod_in {module_binder;rhs;let_result}
    | I.E_raw_code rc ->
      let rc = Raw_code.map self rc in
      return @@ O.E_raw_code rc
    | I.E_constructor const ->
      let const = Constructor.map self const in
      return @@ O.E_constructor const
    | I.E_matching m ->
      let m = Match_expr.map self self_type_opt m in
      return @@ O.E_matching m
    | I.E_record recd ->
      let recd = Record.map self recd in
      return @@ O.E_record recd
    | I.E_accessor {record;path} ->
      let record = self record in
      let accessor ~loc expr a =
        match (a : _ Access_path.access) with
          Access_tuple  i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
        | Access_record a -> O.e_record_accessor ~loc expr (Label a)
        | Access_map k ->
          let k = self k in
          O.e_constant ~loc C_MAP_FIND_OPT [k;expr]
      in
      List.fold ~f:(accessor ~loc:sugar.location) ~init:record path
    | I.E_update {record;path;update} ->
      let record = self record in
      let update = self update in
      let accessor ~loc expr a =
        match (a : _ Access_path.access) with
          Access_tuple  i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
        | Access_record a -> O.e_record_accessor ~loc expr (Label a)
        | Access_map k ->
          let k = self k in
          O.e_constant ~loc C_MAP_FIND_OPT [k;expr]
      in
      let updator ~loc (s:O.expression) a expr =
        match (a : _ Access_path.access) with
          Access_tuple  i -> O.e_record_update ~loc s (Label (Z.to_string i)) expr
        | Access_record a -> O.e_record_update ~loc s (Label a) expr
        | Access_map k ->
          let k = self k in
          O.e_constant ~loc C_MAP_ADD [k;expr;s]
      in
      let aux (s, e : O.expression * _) lst =
        let s' = accessor ~loc:s.location s lst in
        let e' = fun expr ->
          let u = updator ~loc:s.location s lst (expr) in
          e u
        in
        (s',e')
      in
      let (_,rhs) = List.fold ~f:aux ~init:(record, fun e -> e) path in
      rhs @@ update
    | I.E_map map -> (
      let map = List.dedup_and_sort ~compare:Caml.compare map in
      let aux = fun (k, v) prev ->
        let (k', v') = Pair.map ~f:(self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
      List.fold_right ~f:aux ~init map
    )
    | I.E_big_map big_map -> (
      let big_map = List.dedup_and_sort ~compare:Caml.compare big_map in
      let aux = fun (k, v) prev ->
        let (k', v') = Pair.map ~f:(self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
      List.fold_right ~f:aux ~init big_map
    )
    | I.E_list lst ->
      let lst' = List.map ~f:(self) lst in
      let aux = fun cur prev ->
        return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
      let init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
      List.fold_right ~f:aux ~init lst'
    | I.E_set set -> (
      let lst' = List.map ~f:(self) set in
      let lst' = List.dedup_and_sort ~compare:Caml.compare lst' in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
      let init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
      List.fold ~f:aux ~init:init lst'
      )
    | I.E_ascription {anno_expr; type_annotation} ->
      let anno_expr = self anno_expr in
      let type_annotation = self_type type_annotation in
      return @@ O.E_ascription {anno_expr; type_annotation}
    | I.E_module_accessor ma -> return @@ O.E_module_accessor ma
    | I.E_cond {condition; then_clause; else_clause} ->
      let matchee = self condition in
      let match_true = self then_clause in
      let match_false = self else_clause in
      return @@ O.E_matching {
          matchee ;
          cases = [
            { pattern = Location.wrap @@ Pattern.P_variant (Label "True" , Location.wrap Pattern.P_unit) ; body = match_true  } ;
            { pattern = Location.wrap @@ Pattern.P_variant (Label "False", Location.wrap Pattern.P_unit) ; body = match_false } ;
          ]
        }
    | I.E_sequence {expr1; expr2} ->
      let expr1 = self expr1 in
      let expr2 = self expr2 in
      let let_binder : _ Binder.t = {var = ValueVar.fresh ~name:"()" () ; ascr = Some (O.t_unit ()) ; attributes = Binder.empty_attribute} in
      return @@ O.E_let_in {let_binder; rhs=expr1;let_result=expr2; attr = {inline=false; no_mutation=false; view = false ; public=true ; hidden = false ; thunk = false }}
    | I.E_skip () -> O.e_unit ~loc:sugar.location ~sugar ()
    | I.E_tuple t ->
      let aux (i,acc) el =
        let el = self el in
        (i+1,(Label.of_int i, el)::acc) in
      let (_, lst ) = List.fold ~f:aux ~init:(0,[]) t in
      let m = Record.of_list lst in
      return @@ O.E_record m
    | I.E_assign a ->
      let a = Assign.map self self_type_opt a in
      return @@ O.E_assign a

and compile_declaration : I.declaration -> O.declaration = fun d ->
  let return wrap_content : O.declaration = {d with wrap_content} in
  match Location.unwrap d with
  | D_value {binder;expr;attr} ->
    let binder = Binder.map compile_type_expression_option binder in
    let expr   = compile_expression expr in
    let attr   = compile_exp_attributes attr in
    return @@ D_value {binder;expr;attr}
  | D_type {type_binder;type_expr;type_attr} ->
    let type_expr = compile_type_expression type_expr in
    let type_attr = compile_type_attributes type_attr in
    return @@ D_type {type_binder;type_expr;type_attr}
  | D_module {module_binder;module_;module_attr} ->
    let module_ = compile_module_expr module_ in
    let module_attr = compile_module_attributes module_attr in
    return @@ D_module {module_binder;module_;module_attr}

and compile_module_expr : I.module_expr -> O.module_expr = fun me ->
  let return wrap_content : O.module_expr = {me with wrap_content} in
  match me.wrap_content with
    M_struct lst ->
      let lst = compile_module lst in
      return @@ M_struct lst
  | M_variable mv ->
      return @@ M_variable mv
  | M_module_path mp ->
      return @@ M_module_path mp

and compile_decl : I.decl -> O.decl = fun (Decl d) -> Decl (compile_declaration d)
and compile_module : I.module_ -> O.module_ = fun m ->
  List.map ~f:compile_decl m

let compile_program = List.map ~f:compile_declaration
