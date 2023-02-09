module Location = Simple_utils.Location
open Ligo_prim
module I = Ast_imperative
module O = Ast_core

let decompile_row_attributes layout =
  layout |> Option.map ~f:(Format.asprintf "layout:%a" Layout.pp) |> Option.to_list


let decompile_row_elem_attributes michelson_annotation =
  Option.to_list michelson_annotation


let decompile_value_attributes : O.ValueAttr.t -> I.Attr.t =
 fun { inline; no_mutation; view; public; hidden; thunk } ->
  List.filter_map
    ~f:(fun (cond, ret) -> Option.some_if cond ret)
    [ inline, "inline"
    ; no_mutation, "no_mutation"
    ; view, "view"
    ; not public, "private"
    ; hidden, "hidden"
    ; thunk, "thunk"
    ]


let decompile_type_attributes : O.TypeOrModuleAttr.t -> I.Attr.t =
 fun { public; hidden } ->
  List.filter_map
    ~f:(fun (cond, ret) -> Option.some_if cond ret)
    [ not public, "private"; hidden, "hidden" ]


let decompile_module_attributes = decompile_type_attributes
let decompile_contract_attributes = decompile_type_attributes

let rec decompile_type_expression (type_ : O.type_expression) : I.type_expression =
  let self = decompile_type_expression in
  let loc = type_.location in
  let return content = I.make_t ~loc content in
  match type_.sugar with
  | Some ret -> ret
  | None ->
    (match type_.type_content with
    | O.T_variable type_variable -> return @@ T_variable type_variable
    | O.T_app tc ->
      let tc = Type_app.map (fun path -> path) self tc in
      return @@ I.T_app tc
    | O.T_sum row ->
      (* Bug: this type sum could be a michelson_or, we should use is_michelson_or *)
      let row = decompile_row row in
      return @@ I.T_sum row
    | O.T_record row ->
      (* Bug: this record could be a michelson_pair, we should use is_michelson_pair *)
      (* Bug: this record could be a tuple, we should use is_tuple *)
      let row = decompile_row row in
      return @@ I.T_record row
    | O.T_arrow arr ->
      let arr = Arrow.map self arr in
      return @@ T_arrow arr
    | O.T_module_accessor ma -> return @@ I.T_module_accessor ma
    | O.T_singleton x -> return @@ I.T_singleton x
    | O.T_abstraction abs ->
      let abs = Abstraction.map decompile_type_expression abs in
      return @@ I.T_abstraction abs
    | O.T_for_all for_all ->
      let for_all = Abstraction.map decompile_type_expression for_all in
      return @@ I.T_for_all for_all)


and decompile_row ({ fields; layout = _ } : O.row) : _ I.non_linear_rows =
  let fields =
    fields
    |> Record.map ~f:(fun ty ->
           Ast_imperative.
             { associated_type = decompile_type_expression ty; row_elem_attributes = [] })
    |> Record.to_list
  in
  { fields; attributes = [] }


let rec decompile_expression (expr : O.expression) : I.expression =
  let loc = expr.location in
  let self = decompile_expression in
  let return content = I.make_e ~loc content in
  match expr.sugar with
  | Some ret -> ret
  | None ->
    (match expr.expression_content with
    | O.E_literal lit -> return @@ I.E_literal lit
    | O.E_variable name -> return @@ I.E_variable name
    | O.E_constant { cons_name; arguments } ->
      let cons_name = Constant.Const cons_name in
      let arguments = List.map ~f:decompile_expression arguments in
      return @@ I.E_constant { cons_name; arguments }
    | O.E_application app ->
      let app = Application.map self app in
      return @@ I.E_application app
    | O.E_lambda lamb ->
      let lamb = Lambda.map self (Option.map ~f:decompile_type_expression) lamb in
      return @@ I.E_lambda lamb
    | O.E_type_abstraction ta ->
      let ta = Type_abs.map self ta in
      return @@ I.E_type_abstraction ta
    | O.E_recursive recs ->
      let recs = Recursive.map self decompile_type_expression recs in
      return @@ I.E_recursive recs
    | O.E_let_in { let_binder; attributes; rhs; let_result } ->
      let let_binder =
        O.Pattern.map (Option.map ~f:decompile_type_expression) let_binder
      in
      let let_binder = decompile_pattern let_binder in
      let rhs = decompile_expression rhs in
      let let_result = decompile_expression let_result in
      let attributes = decompile_value_attributes attributes in
      return @@ I.E_let_in { let_binder; attributes; rhs; let_result }
    | O.E_type_in ti ->
      let ti = Type_in.map self decompile_type_expression ti in
      return @@ I.E_type_in ti
    | O.E_mod_in { module_binder; rhs; let_result } ->
      let rhs = decompile_module_expr rhs in
      let let_result = self let_result in
      return @@ I.E_mod_in { module_binder; rhs; let_result }
    | O.E_raw_code rc ->
      let rc = Raw_code.map self rc in
      return @@ I.E_raw_code rc
    | O.E_constructor const ->
      let const = Constructor.map self const in
      return @@ I.E_constructor const
    | O.E_matching m ->
      let m = decompile_match_expr m in
      return @@ I.E_matching m
    | O.E_record record ->
      let record = Record.map ~f:self record in
      return @@ I.E_record (Record.to_list record)
    | O.E_accessor { struct_; path } ->
      let struct_ = self struct_ in
      let (Label path) = path in
      return @@ I.E_accessor { struct_; path = [ Access_record path ] }
    | O.E_update { struct_; path; update } ->
      let struct_ = self struct_ in
      let update = self update in
      let (Label path) = path in
      return @@ I.E_update { struct_; path = [ Access_record path ]; update }
    | O.E_ascription ascr ->
      let ascr = Ascription.map self decompile_type_expression ascr in
      return @@ I.E_ascription ascr
    | O.E_module_accessor ma -> return @@ I.E_module_accessor ma
    | O.E_assign a ->
      let a = Assign.map self (Option.map ~f:decompile_type_expression) a in
      return @@ I.E_assign a
    | O.E_for { binder; start; final; incr; f_body } ->
      let start = self start in
      let final = self final in
      let incr = self incr in
      let f_body = self f_body in
      return @@ I.E_for { binder; start; final; incr; f_body }
    | O.E_for_each { fe_binder; collection; collection_type; fe_body } ->
      let collection = self collection in
      let fe_body = self fe_body in
      return @@ I.E_for_each { fe_binder; collection; collection_type; fe_body }
    | O.E_while { cond; body } ->
      let cond = self cond in
      let body = self body in
      return @@ I.E_while { cond; body }
    | O.E_let_mut_in { let_binder; attributes; rhs; let_result } ->
      let let_binder =
        O.Pattern.map (Option.map ~f:decompile_type_expression) let_binder
      in
      let let_binder = decompile_pattern let_binder in
      let rhs = decompile_expression rhs in
      let let_result = decompile_expression let_result in
      let attributes = decompile_value_attributes attributes in
      return @@ I.E_let_mut_in { let_binder; attributes; rhs; let_result }
    | O.E_originate originate ->
      let originate = Originate.map decompile_expression originate in
      return @@ I.E_originate originate
    | O.E_contract_call contract_call ->
      let contract_call = Contract_call.map decompile_expression contract_call in
      return @@ I.E_contract_call contract_call)


and decompile_match_expr
    :  (O.expression, O.type_expression option) O.Match_expr.t
    -> (I.expression, I.type_expression option) I.Match_expr.t
  =
 fun m ->
  let O.Match_expr.{ matchee; cases } = m in
  let matchee = decompile_expression matchee in
  let cases =
    List.map cases ~f:(fun { pattern; body } ->
        let pattern = O.Pattern.map (Option.map ~f:decompile_type_expression) pattern in
        let pattern = decompile_pattern pattern in
        let body = decompile_expression body in
        I.Match_expr.{ pattern; body })
  in
  { matchee; cases }


and decompile_pattern : _ O.Pattern.t -> _ I.Pattern.t =
 fun p ->
  let loc = Location.get_location p in
  match Location.unwrap p with
  | P_unit -> Location.wrap ~loc I.Pattern.P_unit
  | P_var b -> Location.wrap ~loc (I.Pattern.P_var b)
  | P_list (Cons (h, t)) ->
    let h = decompile_pattern h in
    let t = decompile_pattern t in
    Location.wrap ~loc (I.Pattern.P_list (Cons (h, t)))
  | P_list (List ps) ->
    let ps = List.map ~f:decompile_pattern ps in
    Location.wrap ~loc (I.Pattern.P_list (List ps))
  | P_variant (l, p) ->
    let p = decompile_pattern p in
    Location.wrap ~loc (I.Pattern.P_variant (l, p))
  | P_tuple ps ->
    let ps = List.map ~f:decompile_pattern ps in
    Location.wrap ~loc (I.Pattern.P_tuple ps)
  | P_record lps ->
    let lps = Record.map ~f:decompile_pattern lps in
    let lps = Record.to_list lps in
    Location.wrap ~loc (I.Pattern.P_record lps)


and decompile_value_decl O.Value_decl.{ binder; expr; attr } =
  let binder = Binder.map (Option.map ~f:decompile_type_expression) binder in
  let expr = decompile_expression expr in
  let attr = decompile_value_attributes attr in
  I.Value_decl.{ binder; expr; attr }


and decompile_patten_decl O.Pattern_decl.{ pattern; expr; attr } =
  let pattern = O.Pattern.map (Option.map ~f:decompile_type_expression) pattern in
  let pattern = decompile_pattern pattern in
  let expr = decompile_expression expr in
  let attr = decompile_value_attributes attr in
  I.Pattern_decl.{ pattern; expr; attr }


and decompile_type_decl O.Type_decl.{ type_binder; type_expr; type_attr } =
  let type_expr = decompile_type_expression type_expr in
  let type_attr = decompile_type_attributes type_attr in
  I.Type_decl.{ type_binder; type_expr; type_attr }


and decompile_module_decl O.Module_decl.{ module_binder; module_; module_attr } =
  let module_ = decompile_module_expr module_ in
  let module_attr = decompile_module_attributes module_attr in
  I.Module_decl.{ module_binder; module_; module_attr }


and decompile_contract_decl O.Contract_decl.{ contract_binder; contract; contract_attr } =
  let contract = decompile_contract_expr contract in
  let contract_attr = decompile_contract_attributes contract_attr in
  I.Contract_decl.{ contract_binder; contract; contract_attr }


and decompile_declaration : O.declaration -> I.declaration =
 fun d ->
  let return wrap_content : I.declaration = { d with wrap_content } in
  match Location.unwrap d with
  | D_value value_decl ->
    let value_decl = decompile_value_decl value_decl in
    return @@ D_value value_decl
  | D_irrefutable_match pattern_decl ->
    let pattern_decl = decompile_patten_decl pattern_decl in
    return @@ D_irrefutable_match pattern_decl
  | D_type type_decl ->
    let type_decl = decompile_type_decl type_decl in
    return @@ D_type type_decl
  | D_module module_decl ->
    let module_decl = decompile_module_decl module_decl in
    return @@ D_module module_decl
  | D_contract contract_decl ->
    let contract_decl = decompile_contract_decl contract_decl in
    return @@ D_contract contract_decl


and decompile_contract_declaration : O.contract_declaration -> I.contract_declaration =
 fun d ->
  let return wrap_content : I.contract_declaration = { d with wrap_content } in
  match Location.unwrap d with
  | C_value value_decl ->
    let value_decl = decompile_value_decl value_decl in
    return @@ C_value value_decl
  | C_irrefutable_match pattern_decl ->
    let pattern_decl = decompile_patten_decl pattern_decl in
    return @@ C_irrefutable_match pattern_decl
  | C_type type_decl ->
    let type_decl = decompile_type_decl type_decl in
    return @@ C_type type_decl
  | C_module module_decl ->
    let module_decl = decompile_module_decl module_decl in
    return @@ C_module module_decl
  | C_contract contract_decl ->
    let contract_decl = decompile_contract_decl contract_decl in
    return @@ C_contract contract_decl
  | C_entry value_decl ->
    let value_decl = decompile_value_decl value_decl in
    return @@ C_entry value_decl
  | C_view value_decl ->
    let value_decl = decompile_value_decl value_decl in
    return @@ C_view value_decl


and decompile_module_expr : O.module_expr -> I.module_expr =
 fun me ->
  let return wrap_content : I.module_expr = { me with wrap_content } in
  match me.wrap_content with
  | M_struct lst ->
    let lst = decompile_module lst in
    return @@ M_struct lst
  | M_variable mv -> return @@ M_variable mv
  | M_module_path mp -> return @@ M_module_path mp


and decompile_contract_expr : O.contract_expr -> I.contract_expr =
 fun contract_expr ->
  let return wrap_content : I.contract_expr = { contract_expr with wrap_content } in
  match Location.unwrap contract_expr with
  | C_struct decls ->
    let decls = decompile_contract decls in
    return @@ C_struct decls
  | C_variable contract_var -> return @@ C_variable contract_var
  | C_module_path contract_path -> return @@ C_module_path contract_path


and decompile_decl : O.decl -> I.decl = fun d -> decompile_declaration d
and decompile_module : O.module_ -> I.module_ = fun m -> List.map ~f:decompile_decl m
and decompile_contract contract = List.map ~f:decompile_contract_declaration contract

let decompile_program = List.map ~f:decompile_declaration

let decompile_pattern_to_string ~(syntax : Syntax_types.t option) pattern =
  let pattern = O.Pattern.map (Option.map ~f:decompile_type_expression) pattern in
  let pattern = decompile_pattern pattern in
  match syntax with
  | Some JsLIGO -> Tree_abstraction.Jsligo.decompile_pattern_to_string pattern
  | Some CameLIGO | None -> Tree_abstraction.Cameligo.decompile_pattern_to_string pattern


let decompile_type_expression_to_string ~(syntax : Syntax_types.t) te =
  let te = decompile_type_expression te in
  match syntax with
  | JsLIGO -> Tree_abstraction.Jsligo.decompile_type_expression_to_string te
  | CameLIGO -> Tree_abstraction.Cameligo.decompile_type_expression_to_string te
