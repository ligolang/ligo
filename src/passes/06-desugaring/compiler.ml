module Location = Simple_utils.Location
open Simple_utils.Trace
open Errors
open Ligo_prim
module I = Ast_imperative
module O = Ast_core

let is_layout = String.chop_prefix ~prefix:"layout:"
let is_michelson_annotation = String.chop_prefix ~prefix:"annot:"

let compile_row_elem_attributes : string list -> string option =
 fun attributes -> List.find_map attributes ~f:(fun attr -> is_michelson_annotation attr)


let compile_value_attributes : I.Attr.t -> O.ValueAttr.t =
 fun attributes ->
  let is_inline attr = String.equal "inline" attr in
  let is_no_mutation attr = String.equal "no_mutation" attr in
  let is_view attr = String.equal "view" attr in
  let is_hidden attr = String.equal "hidden" attr in
  let is_thunk attr = String.equal "thunk" attr in
  let get_inline : string list -> bool = List.exists ~f:is_inline in
  let get_no_mutation : string list -> bool = List.exists ~f:is_no_mutation in
  let get_view : string list -> bool = List.exists ~f:is_view in
  let get_hidden : string list -> bool = List.exists ~f:is_hidden in
  let get_public : string list -> bool =
   fun attr -> not (List.mem attr "private" ~equal:String.equal)
  in
  let get_thunk : string list -> bool = List.exists ~f:is_thunk in
  let inline = get_inline attributes in
  let no_mutation = get_no_mutation attributes in
  let public = get_public attributes in
  let view = get_view attributes in
  let hidden = get_hidden attributes in
  let thunk = get_thunk attributes in
  { inline; no_mutation; view; public; hidden; thunk }


let compile_type_attributes : I.Attr.t -> O.TypeOrModuleAttr.t =
 fun attributes ->
  let get_public : string list -> bool =
   fun attr -> not (List.mem attr "private" ~equal:String.equal)
  in
  let get_hidden : string list -> bool =
   fun attr -> List.mem attr "hidden" ~equal:String.equal
  in
  let public = get_public attributes in
  let hidden = get_hidden attributes in
  { public; hidden }


let compile_module_attributes = compile_type_attributes
let compile_contract_attributes = compile_type_attributes

let compile_fields_to_layout_fields fields =
  List.map fields ~f:(fun (label, I.{ row_elem_attributes; _ }) ->
      let annot = compile_row_elem_attributes row_elem_attributes in
      { Layout.name = label; annot })


let compile_layout_tree fields = Layout.tree (compile_fields_to_layout_fields fields)
let compile_layout_comb fields = Layout.comb (compile_fields_to_layout_fields fields)

let compile_layout_default fields =
  Layout.default (compile_fields_to_layout_fields fields)


let compile_row_attributes
    : (Label.t * I.type_expression I.row_element) list -> string list -> Layout.t option
  =
 fun fields attributes ->
  (* if at least one annotation is present, we will build the default layout *)
  match attributes with
  | [] ->
    let has_michelson_annot =
      List.exists fields ~f:(fun (_, I.{ row_elem_attributes; _ }) ->
          Option.is_some (compile_row_elem_attributes row_elem_attributes))
    in
    if has_michelson_annot then Some (compile_layout_default fields) else None
  | _ ->
    List.find_map attributes ~f:(fun attr ->
        match is_layout attr with
        | Some "tree" -> Some (compile_layout_tree fields)
        | Some "comb" -> Some (compile_layout_comb fields)
        | _ -> None)


let rec compile_type_expression ~raise (type_ : I.type_expression) : O.type_expression =
  let loc = type_.location in
  let compile_type_expression = compile_type_expression ~raise in
  let compile_row = compile_row ~raise in
  let return content = O.make_t ~loc ~sugar:type_ content in
  match type_.type_content with
  | I.T_sum row ->
    let row = compile_row row in
    return @@ O.T_sum row
  | I.T_record row ->
    let row = compile_row row in
    return @@ O.T_record row
  | I.T_tuple tuple ->
    let row = desugar_tuple_to_row ~raise tuple in
    return @@ O.T_record row
  | I.T_arrow arr ->
    let arr = Arrow.map compile_type_expression arr in
    return @@ T_arrow arr
  | I.T_variable type_variable -> return @@ T_variable type_variable
  | I.T_app
      { type_operator = { module_path = []; element = type_operator }
      ; arguments = [ l; r ]
      }
    when Type_var.equal (Literal_types.v_michelson_or ~loc) type_operator ->
    let l, l_ann =
      trace_option ~raise (corner_case "not an annotated type") @@ I.get_t_annoted l
    in
    let r, r_ann =
      trace_option ~raise (corner_case "not an annotated type") @@ I.get_t_annoted r
    in
    let l = compile_type_expression l in
    let r = compile_type_expression r in
    O.t_michelson_sum ~loc l l_ann r r_ann
  | I.T_app
      { type_operator = { module_path = []; element = type_operator }
      ; arguments = [ l; r ]
      }
    when Type_var.equal (Literal_types.v_michelson_pair ~loc) type_operator ->
    let l, l_ann =
      trace_option ~raise (corner_case "not an annotated type") @@ I.get_t_annoted l
    in
    let r, r_ann =
      trace_option ~raise (corner_case "not an annotated type") @@ I.get_t_annoted r
    in
    let l = compile_type_expression l in
    let r = compile_type_expression r in
    O.t_michelson_pair ~loc l l_ann r r_ann
  | I.T_app type_app ->
    let type_app = Type_app.map (fun path -> path) compile_type_expression type_app in
    return @@ T_app type_app
  | I.T_module_accessor ma -> return @@ T_module_accessor ma
  | I.T_annoted (type_, _) -> compile_type_expression type_
  | I.T_singleton t -> return @@ O.T_singleton t
  | I.T_abstraction abs ->
    let abs = Abstraction.map compile_type_expression abs in
    return @@ O.T_abstraction abs
  | I.T_for_all for_all ->
    let for_all = Abstraction.map compile_type_expression for_all in
    return @@ O.T_for_all for_all


and compile_row ~raise ({ fields; attributes } : _ I.non_linear_rows) : O.row =
  let layout = compile_row_attributes fields attributes in
  let fields =
    List.Assoc.map fields ~f:(fun ({ associated_type; _ } : _ I.row_element) ->
        let associated_type = compile_type_expression ~raise associated_type in
        associated_type)
    |> Label.Map.of_alist_exn
  in
  { fields; layout }


and desugar_tuple_to_row ~raise (tuple : I.type_expression list) : O.row =
  let fields =
    tuple
    |> List.mapi ~f:(fun i type_ ->
           let type_ = compile_type_expression ~raise type_ in
           Label.of_int i, type_)
    |> Label.Map.of_alist_exn
  in
  { fields; layout = None }


let compile_type_expression_option ~raise te_opt =
  Option.map ~f:(compile_type_expression ~raise) te_opt


let rec compile_expression ~raise : I.expression -> O.expression =
 fun expr ->
  let loc = expr.location in
  let self = compile_expression ~raise in
  let self_type = compile_type_expression ~raise in
  let self_type_option = compile_type_expression_option ~raise in
  let return content = O.make_e ~loc ~sugar:expr content in
  match expr.expression_content with
  | I.E_literal literal -> return @@ O.E_literal literal
  | I.E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    let cons_name = Constant.const_name cons_name in
    return @@ O.E_constant { cons_name; arguments }
  | I.E_variable name -> return @@ O.E_variable name
  | I.E_application app ->
    let app = Application.map self app in
    return @@ O.E_application app
  | I.E_lambda lamb ->
    let lamb = Lambda.map self self_type_option lamb in
    return @@ O.E_lambda lamb
  | I.E_type_abstraction ta ->
    let ta = Type_abs.map self ta in
    return @@ O.E_type_abstraction ta
  | I.E_recursive recs ->
    let recs = Recursive.map self self_type recs in
    return @@ O.E_recursive recs
  | I.E_let_in { let_binder; attributes; rhs; let_result } ->
    let let_binder = I.Pattern.map self_type_option let_binder in
    let let_binder = compile_pattern ~raise let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    let attributes = compile_value_attributes attributes in
    return @@ O.E_let_in { let_binder; attributes; rhs; let_result }
  | I.E_type_in ti ->
    let ti = Type_in.map self self_type ti in
    return @@ O.E_type_in ti
  | I.E_mod_in { module_binder; rhs; let_result } ->
    let rhs = compile_module_expr ~raise rhs in
    let let_result = self let_result in
    return @@ O.E_mod_in { module_binder; rhs; let_result }
  | I.E_raw_code rc ->
    let rc = Raw_code.map self rc in
    return @@ O.E_raw_code rc
  | I.E_constructor const ->
    let const = Constructor.map self const in
    return @@ O.E_constructor const
  | I.E_matching match_expr ->
    let match_expr = compile_match_expr ~raise match_expr in
    return @@ O.E_matching match_expr
  | I.E_record record ->
    let record = record |> List.map ~f:(fun (l, e) -> l, self e) |> Record.of_list in
    return @@ O.E_record record
  | I.E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    let accessor expr a =
      match (a : _ Access_path.access) with
      | Access_tuple i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
      | Access_record a -> O.e_record_accessor ~loc expr (Label a)
      | Access_map k ->
        let k = self k in
        O.e_constant ~loc C_MAP_FIND_OPT [ k; expr ]
    in
    List.fold ~f:accessor ~init:struct_ path
  | I.E_update { struct_; update; path } ->
    let struct_ = self struct_ in
    let update = self update in
    let accessor ~loc expr a =
      match (a : _ Access_path.access) with
      | Access_tuple i -> O.e_record_accessor ~loc expr (Label (Z.to_string i))
      | Access_record a -> O.e_record_accessor ~loc expr (Label a)
      | Access_map k ->
        let k = self k in
        O.e_constant ~loc C_MAP_FIND_OPT [ k; expr ]
    in
    let updator ~loc (s : O.expression) a expr =
      match (a : _ Access_path.access) with
      | Access_tuple i -> O.e_record_update ~loc s (Label (Z.to_string i)) expr
      | Access_record a -> O.e_record_update ~loc s (Label a) expr
      | Access_map k ->
        let k = self k in
        O.e_constant ~loc C_MAP_ADD [ k; expr; s ]
    in
    let aux ((s, e) : O.expression * _) lst =
      let s' = accessor ~loc:s.location s lst in
      let e' expr =
        let u = updator ~loc:s.location s lst expr in
        e u
      in
      s', e'
    in
    let _, rhs = List.fold ~f:aux ~init:(struct_, fun e -> e) path in
    rhs @@ update
  | I.E_map map_expr ->
    let map_expr = Map_expr.map self map_expr in
    desugar_map_expr_to_consts ~loc ~empty:Constant.C_MAP_EMPTY map_expr
  | I.E_big_map map_expr ->
    let map_expr = Map_expr.map self map_expr in
    desugar_map_expr_to_consts ~loc ~empty:C_BIG_MAP_EMPTY map_expr
  | I.E_list list_expr ->
    list_expr
    |> List_expr.map self
    |> List.fold_right
         ~init:(return @@ E_constant { cons_name = C_LIST_EMPTY; arguments = [] })
         ~f:(fun elem list ->
           return @@ E_constant { cons_name = C_CONS; arguments = [ elem; list ] })
  | I.E_set set_expr ->
    set_expr
    |> Set_expr.dedup_and_sort ~compare:I.compare_expression
    |> Set_expr.map self
    |> List.fold
         ~init:(return @@ E_constant { cons_name = C_SET_EMPTY; arguments = [] })
         ~f:(fun set elem ->
           return @@ E_constant { cons_name = C_SET_ADD; arguments = [ elem; set ] })
  | I.E_ascription ascr ->
    let ascr = Ascription.map self self_type ascr in
    return @@ O.E_ascription ascr
  | I.E_module_accessor ma -> return @@ O.E_module_accessor ma
  | I.E_cond { condition; then_clause; else_clause } ->
    let condition = self condition in
    let then_clause = self then_clause in
    let else_clause = self else_clause in
    desugar_cond_to_match ~loc condition then_clause else_clause
  | I.E_sequence { expr1; expr2 } ->
    let expr1 = self expr1 in
    let expr2 = self expr2 in
    desugar_sequence_to_let ~loc expr1 expr2
  | I.E_tuple tuple ->
    let tuple = List.map ~f:self tuple in
    let record = desugar_tuple_to_record tuple in
    return @@ O.E_record record
  | I.E_assign { binder = b; expression } ->
    let binder = Binder.map self_type_option b in
    let expression = self expression in
    return @@ O.E_assign { binder; expression }
  | I.E_for for_loop ->
    let for_loop = For_loop.map self for_loop in
    return @@ O.E_for for_loop
  | I.E_for_each for_each_loop ->
    let for_each_loop = For_each_loop.map self for_each_loop in
    return @@ O.E_for_each for_each_loop
  | I.E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ O.E_while while_loop
  | I.E_let_mut_in { let_binder; attributes; rhs; let_result } ->
    let let_binder = I.Pattern.map self_type_option let_binder in
    let let_binder = compile_pattern ~raise let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    let attributes = compile_value_attributes attributes in
    return @@ O.E_let_mut_in { let_binder; attributes; rhs; let_result }
  | I.E_skip () -> O.e_unit ~loc ~sugar:expr ()
  | I.E_originate originate ->
    let originate = Originate.map self originate in
    return @@ O.E_originate originate
  | I.E_contract_call contract_call ->
    let contract_call = Contract_call.map self contract_call in
    return @@ O.E_contract_call contract_call


and compile_match_expr ~raise
    :  (I.expression, I.type_expression option) I.Match_expr.t
    -> (O.expression, O.type_expression option) O.Match_expr.t
  =
 fun { matchee; cases } ->
  let matchee = compile_expression ~raise matchee in
  let cases =
    List.map cases ~f:(fun { pattern; body } ->
        let pattern =
          I.Pattern.map (Option.map ~f:(compile_type_expression ~raise)) pattern
        in
        let pattern = compile_pattern ~raise pattern in
        let body = compile_expression ~raise body in
        O.Match_expr.{ pattern; body })
  in
  O.Match_expr.{ matchee; cases }


and compile_pattern ~raise : _ I.Pattern.t -> _ O.Pattern.t =
 fun pat ->
  let self = compile_pattern ~raise in
  let loc = Location.get_location pat in
  let return content = Location.wrap ~loc content in
  match Location.unwrap pat with
  | P_unit -> return @@ O.Pattern.P_unit
  | P_var var -> return @@ O.Pattern.P_var var
  | P_list (Cons (hd_pat, tl_pat)) ->
    let hd_pat = self hd_pat in
    let tl_pat = self tl_pat in
    return @@ O.Pattern.P_list (Cons (hd_pat, tl_pat))
  | P_list (List pats) ->
    let pats = List.map ~f:self pats in
    return @@ O.Pattern.P_list (List pats)
  | P_variant (label, arg_pat) ->
    let arg_pat = self arg_pat in
    return @@ O.Pattern.P_variant (label, arg_pat)
  | P_tuple pats ->
    let pats = List.map ~f:self pats in
    return @@ O.Pattern.P_tuple pats
  | P_record record ->
    let record = Record.of_list record in
    let record = Record.map record ~f:self in
    return @@ O.Pattern.P_record record


(* [desugar_map_expr_to_consts] desugars a map expr to a series of [C_MAP_ADD] constants.  *)
and desugar_map_expr_to_consts ~loc ~(empty : Constant.constant') map_expr =
  let return content = O.make_e ~loc content in
  map_expr
  |> Map_expr.dedup_and_sort ~compare:O.compare_expression
  |> List.fold_right
       ~init:(return @@ E_constant { cons_name = empty; arguments = [] })
       ~f:(fun (key, data) map ->
         return @@ E_constant { cons_name = C_MAP_ADD; arguments = [ key; data; map ] })


and desugar_cond_to_match ~loc condition then_clause else_clause =
  O.e_matching
    ~loc
    condition
    [ { pattern =
          Location.wrap ~loc
          @@ O.Pattern.P_variant (Label "True", Location.wrap ~loc O.Pattern.P_unit)
      ; body = then_clause
      }
    ; { pattern =
          Location.wrap ~loc
          @@ O.Pattern.P_variant (Label "False", Location.wrap ~loc O.Pattern.P_unit)
      ; body = else_clause
      }
    ]


and desugar_sequence_to_let ~loc expr1 expr2 =
  O.e_let_in_ez
    ~loc
    (Value_var.fresh ~loc ~name:"()" ())
    ~ascr:(O.t_unit ~loc ())
    expr1
    expr2
    { inline = false
    ; no_mutation = false
    ; view = false
    ; public = true
    ; hidden = false
    ; thunk = false
    }


and desugar_tuple_to_record exprs =
  exprs |> List.mapi ~f:(fun i elem -> Label.of_int i, elem) |> Record.of_list


and compile_value_decl ~raise I.Value_decl.{ binder; expr; attr } =
  let binder = Binder.map (compile_type_expression_option ~raise) binder in
  let expr = compile_expression ~raise expr in
  let attr = compile_value_attributes attr in
  O.Value_decl.{ binder; expr; attr }


and compile_pattern_decl ~raise I.Pattern_decl.{ pattern; expr; attr } =
  let pattern = I.Pattern.map (compile_type_expression_option ~raise) pattern in
  let pattern = compile_pattern ~raise pattern in
  let expr = compile_expression ~raise expr in
  let attr = compile_value_attributes attr in
  O.Pattern_decl.{ pattern; expr; attr }


and compile_type_decl ~raise I.Type_decl.{ type_binder; type_expr; type_attr } =
  let type_expr = compile_type_expression ~raise type_expr in
  let type_attr = compile_type_attributes type_attr in
  O.Type_decl.{ type_binder; type_expr; type_attr }


and compile_module_decl ~raise I.Module_decl.{ module_binder; module_; module_attr } =
  let module_ = compile_module_expr ~raise module_ in
  let module_attr = compile_module_attributes module_attr in
  O.Module_decl.{ module_binder; module_; module_attr }


and compile_contract_decl
    ~raise
    I.Contract_decl.{ contract_binder; contract; contract_attr }
  =
  let contract = compile_contract_expr ~raise contract in
  let contract_attr = compile_contract_attributes contract_attr in
  O.Contract_decl.{ contract_binder; contract; contract_attr }


and compile_declaration ~raise : I.declaration -> O.declaration =
 fun d ->
  let return wrap_content : O.declaration = { d with wrap_content } in
  match Location.unwrap d with
  | D_value value_decl ->
    let value_decl = compile_value_decl ~raise value_decl in
    return @@ D_value value_decl
  | D_irrefutable_match pattern_decl ->
    let pattern_decl = compile_pattern_decl ~raise pattern_decl in
    return @@ D_irrefutable_match pattern_decl
  | D_type type_decl ->
    let type_decl = compile_type_decl ~raise type_decl in
    return @@ D_type type_decl
  | D_module module_decl ->
    let module_decl = compile_module_decl ~raise module_decl in
    return @@ D_module module_decl
  | D_contract contract_decl ->
    let contract_decl = compile_contract_decl ~raise contract_decl in
    return @@ D_contract contract_decl


and compile_contract_declaration ~raise : I.contract_declaration -> O.contract_declaration
  =
 fun d ->
  let return wrap_content : O.contract_declaration = { d with wrap_content } in
  match Location.unwrap d with
  | C_value value_decl ->
    let value_decl = compile_value_decl ~raise value_decl in
    return @@ C_value value_decl
  | C_irrefutable_match pattern_decl ->
    let pattern_decl = compile_pattern_decl ~raise pattern_decl in
    return @@ C_irrefutable_match pattern_decl
  | C_type type_decl ->
    let type_decl = compile_type_decl ~raise type_decl in
    return @@ C_type type_decl
  | C_module module_decl ->
    let module_decl = compile_module_decl ~raise module_decl in
    return @@ C_module module_decl
  | C_contract contract_decl ->
    let contract_decl = compile_contract_decl ~raise contract_decl in
    return @@ C_contract contract_decl
  | C_entry value_decl ->
    let value_decl = compile_value_decl ~raise value_decl in
    return @@ C_entry value_decl
  | C_view value_decl ->
    let value_decl = compile_value_decl ~raise value_decl in
    return @@ C_view value_decl


and compile_module_expr ~raise : I.module_expr -> O.module_expr =
 fun me ->
  let return wrap_content : O.module_expr = { me with wrap_content } in
  match me.wrap_content with
  | M_struct lst ->
    let lst = compile_module ~raise lst in
    return @@ M_struct lst
  | M_variable mv -> return @@ M_variable mv
  | M_module_path mp -> return @@ M_module_path mp


and compile_contract_expr ~raise : I.contract_expr -> O.contract_expr =
 fun contract_expr ->
  let return wrap_content : O.contract_expr = { contract_expr with wrap_content } in
  match Location.unwrap contract_expr with
  | C_struct decls ->
    let decls = compile_contract ~raise decls in
    return @@ C_struct decls
  | C_variable contract_var -> return @@ C_variable contract_var
  | C_module_path contract_path -> return @@ C_module_path contract_path


and compile_decl ~raise : I.decl -> O.decl = fun d -> compile_declaration ~raise d

and compile_module ~raise : I.module_ -> O.module_ =
 fun m -> List.map ~f:(compile_decl ~raise) m


and compile_contract ~raise contract =
  List.map ~f:(compile_contract_declaration ~raise) contract


let compile_program ~raise : I.program -> O.program =
 fun p -> List.map ~f:(fun decl -> compile_declaration ~raise decl) p
