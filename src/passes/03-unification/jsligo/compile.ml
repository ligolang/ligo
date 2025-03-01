open Core
module Location = Unification_shared.Helpers.Location
module Make_Folding = Unification_shared.Helpers.Folding
module Region = Simple_utils.Region
module Ligo_string = Simple_utils.Ligo_string
module Ne_list = Simple_utils.Ne_list
module O = Ast_unified
module I = Typescript_stripper.Ast_stripped

(*open Region*)

type 'a reg = 'a Region.reg

(* Utilities *)

let ( <@ ) f g x = f (g x)
let mk_reg region value = Region.{ region; value }

let compile_decorator dec =
  let key, value = dec#payload in
  Nano_prim.Attribute.{ key; value }


let compile_decorators = List.map ~f:compile_decorator

let compile_mvar x =
  Ligo_prim.Module_var.of_input_var ~loc:(Location.File x#region) x#payload


let compile_var x =
  Ligo_prim.Value_var.of_input_var ~loc:(Location.File x#region) x#payload


let compile_tvar x =
  Ligo_prim.Type_var.of_input_var ~loc:(Location.File x#region) x#payload


let compile_path (t : I.simple_path reg) : O.Mod_variable.t Nonempty_list.t =
  let I.{ path; selected } = t.value in
  let rev_path = List.rev path in
  let rev_path = Nonempty_list.(selected :: rev_path) in
  let path = Nonempty_list.reverse rev_path in
  Nonempty_list.map ~f:compile_mvar path


(* Compiling conditional branches with an optimisation: If the
   statement is a block containing a single instruction, we do not
   want to emit a [ClauseBlock], but a [ClauseInstr]. *)

let compile_branch compile_statement (stmt : I.statement)
    : (I.statement, I.statements) O.Test_clause.t
  =
  let region = I.region_of_statement stmt in
  match Location.unwrap @@ compile_statement stmt with
  | O.S_instr (I.S_block block) ->
    (match block.value with
    | [ one ] ->
      (match Location.unwrap @@ compile_statement one with
      | S_instr i -> O.Test_clause.ClauseInstr i
      | _ ->
        let singleton = mk_reg region Nonempty_list.[ stmt ] in
        O.Test_clause.ClauseBlock singleton)
    | _ -> O.Test_clause.ClauseBlock block)
  | S_instr i -> O.Test_clause.ClauseInstr i
  | _ ->
    let singleton = mk_reg region Nonempty_list.[ stmt ] in
    O.Test_clause.ClauseBlock singleton


let mk_label (v : I.variable) : O.Label.t =
  O.Label.T.create ~loc:(Location.lift v#region) v#payload


let pattern_to_param pattern = O.Param.{ pattern; param_kind = `Const }

module Eq = struct
  type expr = I.expr
  type ty_expr = I.type_expr
  type pattern = I.pattern
  type statement = I.statement
  type block = I.statements
  type mod_expr = I.statements
  type instruction = I.statement
  type declaration = I.declaration
  type program_entry = I.statement
  type program = I.statements
  type sig_expr = I.intf_expr
  type sig_entry = I.intf_entry reg
end

module Folding = Make_Folding (Eq)

(* EXPRESSIONS *)

let compile_property (property : 'a I.property reg) =
  let I.{ decorators = _; comments = _; property_name; static = _; property_rhs } =
    property.value
  in
  let field_id = O.Object_.F_Name (mk_label property_name) in
  let field_rhs = Some property_rhs in
  let object_ = O.Object_.{ field_id; field_rhs } in
  Location.wrap ~loc:(Location.lift property.region) object_


let compile_properties (properties : 'a I.property reg list) =
  List.map ~f:compile_property properties


let compile_bin_op (sign : O.Operators.op) (op : (I.expr * I.expr) Region.reg) =
  let return x = Location.(wrap ~loc:(lift op.region)) x in
  let left, right = op.Region.value in
  return @@ O.E_binary_op { operator = return sign; left; right }


let compile_unary_op (sign : O.Operators.op) (op : I.expr Region.reg) =
  let return x = Location.(wrap ~loc:(lift op.region)) x in
  let arg = op.Region.value in
  return @@ O.E_unary_op { operator = return sign; arg }


let compile_postfix_op (expr : I.variable reg) op =
  let loc = Location.lift expr.region in
  let return x = Location.wrap ~loc x in
  let post_op = Location.wrap ~loc op in
  let expr = I.E_var expr.value in
  return @@ O.E_postfix { post_op; expr }


let compile_prefix_op (expr : I.variable reg) op =
  let loc = Location.lift expr.region in
  let return x = Location.wrap ~loc x in
  let pre_op = Location.wrap ~loc op in
  let expr = I.E_var expr.value in
  return @@ O.E_prefix { pre_op; expr }


let compile_chain_assignment op expr =
  let Region.{ value = expr1, expr2; region } = expr in
  let loc = Location.lift region in
  let op = O.Assign_chainable.Assignment_operator op in
  Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1; op; expr2 }


let compile_generics (node : I.variable list) : O.Ty_variable.t Nonempty_list.t option =
  match node with
  | [] -> None
  | t_var :: t_vars ->
    let params = Nonempty_list.(t_var :: t_vars) in
    Some (Nonempty_list.map ~f:compile_tvar params)


let compile_function (expr : I.arrow_fun_expr reg) =
  let I.{ generics; parameters; rhs_type; fun_body } = expr.value in
  let type_params = compile_generics generics in
  let parameters : I.pattern O.Param.t list =
    let f parameter =
      let Region.{ value; region } = parameter in
      let pattern, t_expr_opt = value in
      let pattern =
        match t_expr_opt with
        | None -> pattern
        | Some t_expr -> I.P_typed (mk_reg region (pattern, t_expr))
      in
      pattern_to_param pattern
    in
    List.map ~f parameters
  in
  let ret_type = rhs_type in
  match fun_body with
  | I.Stmt_body body -> O.E_block_poly_fun { type_params; parameters; ret_type; body }
  | Expr_body body -> O.E_poly_fun { type_params; parameters; ret_type; body }


let rec extract_rev_path ((expr, property) : I.expr * I.variable) =
  match expr with
  | I.E_member member ->
    let sub_expr, properties = extract_rev_path member.value in
    sub_expr, Nonempty_list.cons property properties
  | _ -> expr, Nonempty_list.[ property ]


let extract_rev_path (node : I.expr * I.variable) : I.expr * I.variable list =
  let expr, path = extract_rev_path node in
  expr, Nonempty_list.to_list path


let split_rev_path (node : I.variable list) : I.variable list * I.variable list =
  let rec filter vars property_path =
    match vars with
    | [] -> [], property_path
    | v :: path ->
      if String.is_empty v#payload
      then filter vars property_path (* Should not happen *)
      else if Char.is_lowercase v#payload.[0]
      then filter path (v :: property_path)
      else List.rev vars, property_path
  in
  filter node []


let compile_member (node : (I.expr * I.variable) reg) =
  let expr, path = extract_rev_path node.value in
  let module_path, property_path = split_rev_path path in
  match expr, module_path, property_path with
  | I.E_var v, m, p1 :: p when Char.is_uppercase v#payload.[0] ->
    let module_path = Nonempty_list.(v :: m) in
    let module_path = Nonempty_list.map ~f:compile_mvar module_path in
    let f acc var =
      let region = Region.cover (I.region_of_expr acc) var#region in
      I.E_member Region.{ region; value = acc, var }
    in
    let field = List.fold_left ~f ~init:(I.E_var p1) p in
    O.E_module_open_in { module_path; field; field_as_open = false }
  | _ ->
    let f variable = O.Selection.FieldName (mk_label variable) in
    let property_path = List.map ~f property_path in
    O.E_proj (expr, property_path)


let compile_ctor_app (node : (I.string_literal * I.expr list) reg) =
  let ctor, args = node.Region.value in
  let args = Ne_list.of_list_opt args in
  O.E_ctor_app (I.E_string ctor, args)


let compile_match_clause (node : I.match_clause) : _ O.Match_tc39.match_clause =
  let I.{ constructor; filter; clause_expr } = node in
  let filter =
    match filter with
    | None -> I.P_ctor_app (mk_reg constructor#region (constructor, []))
    | Some parameter ->
      let region = Region.cover constructor#region parameter.region in
      (match parameter.value with
      | pattern, None -> I.P_ctor_app (mk_reg region (constructor, [ pattern ]))
      | pattern, Some type_expr ->
        let param = I.P_typed (mk_reg parameter.region (pattern, type_expr)) in
        I.P_ctor_app (mk_reg region (constructor, [ param ])))
  in
  O.Match_tc39.{ filter; clause_expr }


let compile_match_clauses (node : (I.expr * I.match_clause Ne_list.t) reg) =
  let subject, clauses = node.Region.value in
  let clauses = Nonempty_list.map ~f:compile_match_clause clauses in
  let match_clauses = O.Match_tc39.AllClauses (clauses, None) in
  (* No default clauses *)
  O.E_match_tc39 { subject; match_clauses }


let expr (expr : Eq.expr) : Folding.expr =
  let loc = Location.lift (I.region_of_expr expr) in
  let return x = Location.wrap ~loc x in
  match expr with
  | E_add expr -> compile_bin_op PLUS expr
  | E_add_eq expr -> compile_chain_assignment Plus_eq expr
  | E_and expr -> compile_bin_op DAMPERSAND expr
  | E_app { value = expr, args; _ } -> return (O.E_call (expr, return args))
  | E_array { value = items; _ } ->
    let f : I.expr I.element -> _ O.Array_repr.item = function
      | Spread expr -> Rest_entry expr
      | Element expr -> Expr_entry expr
    in
    return (O.E_array (List.map ~f items))
  | E_arrow_fun expr -> return @@ compile_function expr
  | E_assign { value = expr1, expr2; _ } ->
    return (O.E_struct_assign_chainable { expr1; op = Eq; expr2 })
  | E_bit_and expr -> compile_bin_op WORD_LAND expr
  | E_bit_and_eq expr -> compile_chain_assignment BitAnd_eq expr
  | E_bit_neg expr -> compile_unary_op WORD_NOT expr
  | E_bit_or expr -> compile_bin_op WORD_LOR expr
  | E_bit_or_eq expr -> compile_chain_assignment BitOr_eq expr
  | E_bit_sl expr -> compile_bin_op WORD_LSL expr
  | E_bit_sl_eq expr -> compile_chain_assignment BitSl_eq expr
  | E_bit_sr expr -> compile_bin_op WORD_LSR expr
  | E_bit_sr_eq expr -> compile_chain_assignment BitSr_eq expr
  | E_bit_xor expr -> compile_bin_op WORD_LXOR expr
  | E_bit_xor_eq expr -> compile_chain_assignment BitXor_eq expr
  | E_bytes expr ->
    let hex = snd expr#payload in
    return @@ O.E_literal (Literal_bytes (Hex.to_bytes hex))
  | E_contract_of expr -> return (O.E_contract (compile_path expr.value))
  | E_ctor_app expr -> return @@ compile_ctor_app expr
  | E_div expr -> compile_bin_op SLASH expr
  | E_div_eq expr -> compile_chain_assignment Div_eq expr
  | E_equal expr -> compile_bin_op DEQ expr
  | E_false _ -> return @@ O.E_constr (Ligo_prim.Label.of_string "False")
  | E_function expr -> return @@ compile_function expr
  | E_geq expr -> compile_bin_op GE expr
  | E_gt expr -> compile_bin_op GT expr
  | E_int expr -> return @@ O.E_literal (Literal_int (snd expr#payload))
  | E_leq expr -> compile_bin_op LE expr
  | E_lt expr -> compile_bin_op LT expr
  | E_match expr -> return @@ compile_match_clauses expr
  | E_member expr -> return @@ compile_member expr
  | E_michelson expr ->
    (* Module [Strip] wraps for now a [E_typed] around the
       [E_michelson], so we can safely ignore here the type
       expression. *)
    let language, code, _type_expr = expr.value in
    let code = I.E_template code in
    return (O.E_raw_code { language = language#payload; code })
  | E_mult expr -> compile_bin_op STAR expr
  | E_mult_eq expr -> compile_chain_assignment Times_eq expr
  | E_neg expr -> compile_unary_op MINUS expr
  | E_neq expr -> compile_bin_op EQ_SLASH_EQ expr
  | E_not expr -> compile_unary_op EX_MARK expr
  | E_object expr -> return (O.E_object (compile_properties expr.value))
  | E_or expr -> compile_bin_op DPIPE expr
  | E_post_decr expr -> compile_postfix_op expr O.Prefix_postfix.Decrement
  | E_post_incr expr -> compile_postfix_op expr O.Prefix_postfix.Increment
  | E_pre_decr expr -> compile_prefix_op expr O.Prefix_postfix.Decrement
  | E_pre_incr expr -> compile_prefix_op expr O.Prefix_postfix.Increment
  | E_rem expr -> compile_bin_op PRCENT expr
  | E_rem_eq expr -> compile_chain_assignment Mod_eq expr
  | E_string expr ->
    return @@ O.E_literal (Literal_string (Ligo_string.Standard expr#payload))
  | E_sub expr -> compile_bin_op MINUS expr
  | E_subscript expr ->
    (* We assume that there is no need for unspooling [expr]. Correct? *)
    let expr, int = expr.value in
    let index = O.Selection.Component_num int#payload in
    return (O.E_proj (expr, [ index ]))
  | E_sub_eq expr -> compile_chain_assignment Min_eq expr
  | E_template expr ->
    return @@ O.E_literal (Literal_string (Ligo_string.Verbatim expr#payload))
  | E_ternary expr ->
    let I.{ condition; truthy; falsy } = expr.value in
    let ifnot = Some falsy in
    return (O.E_cond { test = condition; ifso = truthy; ifnot })
  | E_true _ -> return (O.E_constr (Ligo_prim.Label.of_string "True"))
  | E_typed expr ->
    let expr, type_expr = expr.value in
    return (O.E_annot (expr, type_expr))
  | E_update expr ->
    let I.{ obj_expr; updates } = expr.value in
    let updates = compile_properties updates in
    return (O.E_object_update { object_ = obj_expr; updates })
  | E_var v -> return @@ O.E_variable_esc (Raw (compile_var v))
  | E_xor expr -> compile_bin_op WORD_XOR expr


(* TYPE EXPRESSIONS *)

let compile_member_type (member : I.member_type reg) =
  let I.{ decorators; comments = _; property_name; rhs_type } = member.value in
  let decorators = compile_decorators decorators in
  let property_name = mk_label property_name in
  let property_rhs = Some rhs_type in
  property_name, property_rhs, decorators


let compile_parameter param : _ O.Named_fun.fun_type_arg =
  let name, type_expr = param.Region.value in
  { name = name#payload; type_expr }


let rec ty_expr (t_expr : Eq.ty_expr) : Folding.ty_expr =
  let loc = Location.lift (I.region_of_type_expr t_expr) in
  let return x = Location.wrap ~loc x in
  match t_expr with
  | T_apply t_expr ->
    let constr, args = t_expr.value in
    (match args with
    | [] -> (* Should not happen *) ty_expr constr
    | fst_arg :: more_args ->
      let type_args = Nonempty_list.(fst_arg :: more_args) in
      return (O.T_app { constr; type_args }))
  | T_tuple t_expr -> return (O.T_prod t_expr.value)
  | T_for_all t_expr ->
    let type_vars, type_ = t_expr.value in
    let ty_binders = List.map ~f:compile_tvar type_vars
    and kind = Ligo_prim.Kind.Type in
    return (O.T_for_alls { ty_binders; kind; type_ })
  | T_fun t_expr ->
    let parameters, ret_type = t_expr.value in
    let parameters = List.map ~f:compile_parameter parameters in
    return (O.T_named_fun (parameters, ret_type))
  | T_int t ->
    let s, z = t#payload in
    return (O.T_int (s, z))
  | T_object t_expr ->
    let members = List.map ~f:compile_member_type t_expr.value in
    let fields = O.Non_linear_rows.make members in
    return (O.T_record_raw fields)
  | T_path simple_path ->
    let I.{ path; selected } = simple_path.value in
    (match path with
    | [] -> return @@ O.T_var_esc (Raw (compile_tvar selected))
    | fst_mod :: other_mods ->
      let path = Nonempty_list.(fst_mod :: other_mods) in
      let module_path = Nonempty_list.map ~f:compile_mvar path in
      let field_as_open = false in
      let field = compile_tvar selected in
      return @@ O.T_module_access { module_path; field; field_as_open })
  | T_parameter_of t_expr ->
    let path = compile_path t_expr.value in
    return (O.T_contract_parameter path)
  | T_string t_expr -> return @@ O.T_string t_expr#payload
  | T_union t_expr ->
    let variants = Nonempty_list.to_list t_expr.value in
    return (O.T_union variants)
  | T_sum t_expr ->
    let destruct variant : O.Label.t * I.type_expr option * _ list =
      let ctor, arguments = variant.Region.value in
      let tuple =
        match arguments with
        | [] -> None
        | [ t ] -> Some t
        | fst :: more ->
          let components = Nonempty_list.(fst :: more) in
          Some (I.T_tuple (mk_reg variant.region components))
      in
      mk_label ctor, tuple, [] (* TODO: Decorators? *)
    in
    let variants =
      Nonempty_list.to_list t_expr.Region.value
      |> List.map ~f:destruct
      |> O.Non_linear_rows.make
    in
    return @@ O.T_sum_raw variants


(* PATTERNS *)

let compile_property_pattern (property : I.pattern I.property Region.reg)
    : (O.Label.t, I.pattern) O.Field.t
  =
  let I.{ decorators = _; comments = _; property_name; static = _; property_rhs } =
    property.value
  in
  let property_name = mk_label property_name in
  O.Field.Complete (property_name, property_rhs)


let pattern (pattern : Eq.pattern) : Folding.pattern =
  Location.wrap ~loc:(Location.lift (I.region_of_pattern pattern))
  @@
  match pattern with
  | P_array { value; _ } ->
    let f (elem : I.pattern I.element) =
      match elem with
      | Spread pattern -> O.{ pattern; ellipsis = true }
      | Element pattern -> O.{ pattern; ellipsis = false }
    in
    O.P_tuple_with_ellipsis (List.map ~f value)
  | P_bytes pattern ->
    let bytes = Hex.to_bytes (snd pattern#payload) in
    O.P_literal (Literal_bytes bytes)
  | P_false _ -> O.P_ctor (Ligo_prim.Label.of_string "False")
  | P_int pattern -> O.P_literal (Literal_int (snd pattern#payload))
  | P_object pattern ->
    let fields = List.map ~f:compile_property_pattern pattern.value in
    O.P_pun_record fields
  | P_string pattern ->
    let string = Ligo_string.standard pattern#payload in
    O.P_literal (Literal_string string)
  | P_true _ -> O.P_ctor (Ligo_prim.Label.of_string "True")
  | P_var simple_path ->
    let I.{ path; selected } = simple_path.value in
    (match path with
    | [] -> O.P_var_esc (Raw (compile_var selected))
    | fst_mod :: other_mods ->
      let path = Nonempty_list.(fst_mod :: other_mods) in
      let module_path = Nonempty_list.map ~f:compile_mvar path in
      let field_as_open = false in
      let field = I.P_var (mk_reg selected#region I.{ path = []; selected }) in
      O.P_mod_access { module_path; field; field_as_open })
  | P_typed pattern ->
    let pattern, type_expr = pattern.value in
    O.P_typed (type_expr, pattern)
  | P_ctor_app pattern ->
    let variable, patterns = pattern.value in
    O.P_ctor_app (I.P_string variable :: patterns)


(* STATEMENTS *)

let statement (stmt : Eq.statement) : Folding.statement =
  let loc = Location.lift (I.region_of_statement stmt) in
  let return = Location.wrap ~loc in
  match stmt with
  | S_block _ | S_break _ -> return @@ O.S_instr stmt
  | S_decl decl -> return @@ O.S_decl decl
  | S_export decl -> return @@ O.S_export decl
  | S_expr _ | S_for _ | S_for_of _ | S_if _ | S_return _ | S_switch _ | S_while _ ->
    return @@ O.S_instr stmt


(* INSTRUCTIONS *)

let instruction (instr : Eq.instruction) : Folding.instruction =
  Location.wrap ~loc:(Location.lift (I.region_of_statement instr))
  @@
  match instr with
  | S_block stmts -> O.I_block stmts
  | S_break _ -> O.I_break
  | S_decl _ | S_export _ -> assert false
  | S_expr expr -> O.I_expr expr
  | S_for stmt ->
    let I.{ initialiser; condition; afterthought; for_body } = stmt.value in
    let afterthought =
      match afterthought with
      | [] -> None
      | fst_expr :: more_exprs -> Some Nonempty_list.(fst_expr :: more_exprs)
    in
    let statement = for_body in
    O.I_for_stmt { initialiser; condition; afterthought; statement }
  | S_for_of stmt ->
    let I.{ index_kind; index; expr; for_of_body } = stmt.value in
    let index_kind =
      match index_kind with
      | Some (`Let _) -> `Let
      | _ -> `Const
    in
    let index' : I.pattern I.element list =
      match index.value with
      | var, None ->
        let var = I.{ path = []; selected = var } in
        let var = mk_reg index.region var in
        [ I.Element (I.P_var var) ]
      | key, Some value ->
        let key' = I.{ path = []; selected = key } in
        let key' = mk_reg key#region key' in
        let value' = I.{ path = []; selected = value } in
        let value' = mk_reg value#region value' in
        [ I.Element (I.P_var key'); I.Element (I.P_var value') ]
    in
    let index = I.P_array (mk_reg index.region index') in
    O.I_for_of { index_kind; index; expr; for_stmt = for_of_body }
  | S_if stmt ->
    let I.{ test; if_so; if_not } = stmt.value in
    let compile_branch = compile_branch statement in
    let ifso = compile_branch if_so
    and ifnot = Option.map if_not ~f:compile_branch in
    O.I_cond { test; ifso; ifnot }
  | S_return stmt -> O.I_return stmt.value
  | S_switch stmt ->
    let switch_subject, cases = stmt.value in
    let switch_cases, default_case = cases in
    let f (case : I.switch_case) : _ O.Switch.switch_case =
      let case_subject, case_body = case in
      O.Switch.{ expr = case_subject; case_body }
    in
    let cases = Nonempty_list.map ~f switch_cases in
    let cases = O.Switch.AllCases (cases, default_case) in
    O.I_switch { subject = switch_subject; cases }
  | S_while stmt ->
    let cond, statement = stmt.value in
    let block = Nonempty_list.singleton statement in
    let block = mk_reg stmt.region block in
    O.I_while { cond; block }


(* DECLARATIONS *)

let compile_method_definition (node : I.method_definition reg) : I.statement =
  let I.{ method_sig; method_body } = node.value in
  let I.{ decorators; comments; static; method_name; generics; parameters; rhs_type } =
    method_sig.value
  in
  let fun_name = method_name in
  let mk_param (param : (I.variable * I.type_expr) reg) : I.parameter reg =
    let var, type_expr = param.value in
    let path = I.{ path = []; selected = var } in
    let pattern = I.P_var (mk_reg var#region path) in
    mk_reg param.region (pattern, Some type_expr)
  in
  let parameters = List.map ~f:mk_param parameters in
  let rhs_type = Some rhs_type in
  let fun_body = method_body in
  let fun_decl : I.fun_decl =
    I.{ comments; fun_name; generics; parameters; rhs_type; fun_body }
  in
  let decl = I.D_function (mk_reg node.region fun_decl) in
  let decorate dec decl = I.D_decorated (dec, decl) in
  let decl = List.fold_right ~f:decorate ~init:decl decorators in
  match static with
  | None -> I.S_decl decl
  | Some _ -> I.S_export decl


let compile_public_field_definition (node : I.public_field_definition reg) : I.statement =
  let I.{ decorators; static; name; field_type; field_value } = node.value in
  let kind = `Const Region.ghost in
  let var = I.{ path = []; selected = name } in
  let pattern = I.P_var (mk_reg name#region var) in
  let rhs_type = field_type in
  let rhs_expr = field_value in
  let region = Region.cover name#region (I.region_of_expr rhs_expr) in
  let binding = I.{ pattern; rhs_type; rhs_expr } in
  let binding = mk_reg region binding in
  let bindings = Nonempty_list.singleton binding in
  let value_decl = I.{ comments = []; kind; bindings } in
  let decl = I.D_value (mk_reg node.region value_decl) in
  let decorate dec decl = I.D_decorated (dec, decl) in
  let decl = List.fold_right ~f:decorate ~init:decl decorators in
  match static with
  | None -> I.S_decl decl
  | Some _ -> I.S_export decl


let compile_class_member (node : I.class_member) : I.statement =
  match node with
  | I.Method_definition def -> compile_method_definition def
  | Public_field_definition def -> compile_public_field_definition def


let compile_val_binding (node : I.val_binding reg)
    : (Eq.pattern, I.expr, I.type_expr) O.Simple_decl.t
  =
  let I.{ pattern; rhs_type; rhs_expr } = node.value in
  let type_params, rhs_type =
    match rhs_type with
    | None -> None, None
    | Some (I.T_for_all { value = vars, type_expr; _ }) ->
      compile_generics vars, Some type_expr
    | Some type_expr -> None, Some type_expr
  in
  O.Simple_decl.{ type_params; pattern; rhs_type; let_rhs = rhs_expr }


let compile_import_decl = function
  | I.Import_alias import ->
    let alias, path = import.value in
    let alias = compile_mvar alias in
    let module_path = compile_path path in
    O.Import.Import_rename { alias; module_path }
  | I.Import_all_as import ->
    let alias, file_path = import.value in
    let alias = compile_mvar alias in
    let module_str = file_path#payload in
    O.Import.Import_all_as { alias; module_str }
  | I.Import_from import ->
    let imported, file_path = import.value in
    let imported = Nonempty_list.map ~f:compile_var imported in
    let module_str = file_path#payload in
    O.Import.Import_selected { imported; module_str }


let compile_fun_decl (node : I.fun_decl reg) =
  let I.{ comments = _; fun_name; generics; parameters; rhs_type; fun_body } =
    node.value
  in
  let type_params = compile_generics generics in
  let fun_body = I.Stmt_body fun_body in
  let function_expr = I.{ generics; parameters; rhs_type; fun_body } in
  let function_expr = mk_reg node.region function_expr in
  let let_rhs = I.E_function function_expr in
  let path = I.{ path = []; selected = fun_name } in
  let pattern = I.P_var (mk_reg fun_name#region path) in
  O.Simple_decl.{ type_params; pattern; rhs_type = None; let_rhs }


let compile_type_decl (node : I.type_decl reg) =
  let I.{ name; generics; type_expr } = node.value in
  let name = compile_tvar name in
  let params = compile_generics generics in
  O.Type_abstraction_decl.{ name; params; type_expr }


let rec declaration (decl : Eq.declaration) : Folding.declaration =
  let region = I.region_of_declaration decl in
  let return = Location.wrap ~loc:(Location.lift region) in
  match decl with
  | I.D_function decl ->
    let const = compile_fun_decl decl in
    return @@ O.D_multi_const Nonempty_list.[ const ]
  | D_decorated (decorator, decl) -> return @@ O.D_attr (compile_decorator decorator, decl)
  | D_import decl -> return @@ O.D_import (compile_import_decl decl)
  | D_interface decl ->
    let I.{ intf_name; intf_extends; intf_body } = decl.value in
    let name = compile_mvar intf_name in
    let extends = List.map ~f:(fun p -> I.I_path p) intf_extends in
    return @@ O.D_signature { name; sig_expr = I_body intf_body; extends }
  | D_namespace decl ->
    let I.{ namespace_name; namespace_type; namespace_body } = decl.value in
    let name = compile_mvar namespace_name in
    let mod_expr = namespace_body in
    let annotation = O.Mod_decl.{ signatures = namespace_type; filter = false } in
    return @@ O.D_module { name; mod_expr; annotation }
  | D_class decl ->
    let I.{ comments = _; class_name; implements; class_body } = decl.value in
    let namespace_name = class_name in
    let namespace_type = List.map ~f:(fun p -> I.I_path p) implements in
    let namespace_body = Nonempty_list.map ~f:compile_class_member class_body.value in
    let namespace_body = mk_reg class_body.region namespace_body in
    let decl' = I.{ namespace_name; namespace_type; namespace_body } in
    declaration (I.D_namespace (mk_reg decl.region decl'))
  | D_type decl -> return @@ O.D_type_abstraction (compile_type_decl decl)
  | D_value decl ->
    let I.{ comments = _; kind; bindings } = decl.value in
    let bindings = Nonempty_list.map ~f:compile_val_binding bindings in
    return
    @@
    (match kind with
    | `Let _ -> O.D_multi_var bindings
    | `Const _ -> O.D_multi_const bindings)


(* PROGRAM *)

let program_entry (stmt : Eq.program_entry) : Folding.program_entry =
  match Location.unwrap @@ statement stmt with
  | O.S_export decl -> PE_export (I.S_decl decl)
  | O.S_decl decl -> PE_declaration decl
  | O.S_instr _ -> PE_top_level_instruction stmt
  | O.S_directive () -> PE_preproc_directive ()
  | O.S_attr (attr, s) -> PE_attr (attr, stmt)


let program (stmts : Eq.program) : Folding.program = Nonempty_list.to_list stmts.value

(* INTERFACES *)

let sig_expr : Eq.sig_expr -> Folding.sig_expr = function
  | I_body { value = entries; region } ->
    let loc = Location.lift region in
    Location.wrap ~loc @@ O.S_body entries
  | I_path path ->
    let loc = Location.lift path.region in
    let path = compile_path path in
    Location.wrap ~loc @@ O.S_path path


let sig_entry (node : Eq.sig_entry) : Folding.sig_entry =
  let return = Location.wrap ~loc:(Location.lift node.region) in
  let I.{ decorators; comments = _; entry_name; entry_optional; entry_type } =
    node.value
  in
  return
  @@
  match decorators with
  | fst_dec :: more_decs ->
    let entry' = { node.value with decorators = more_decs } in
    let entry' = { node with value = entry' } in
    (O.S_attr (compile_decorator fst_dec, entry') : _ O.sig_entry_content_)
  | [] ->
    let var = compile_var entry_name in
    O.S_value (var, entry_type, Option.is_some entry_optional)


(* BLOCKS *)

let block (node : Eq.block) : Folding.block =
  Location.wrap ~loc:(Location.lift node.region) node.value


(* MODULE EXPRESSIONS *)

let mod_expr (node : Eq.mod_expr) : Folding.mod_expr =
  Location.wrap ~loc:(Location.lift node.region) (O.M_body node)
