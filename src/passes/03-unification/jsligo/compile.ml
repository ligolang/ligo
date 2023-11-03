open Simple_utils.Utils
module List = Simple_utils.List
open Unification_shared.Helpers
module Option = Simple_utils.Option
module O = Ast_unified
module I = Cst.Jsligo

let ghost : string I.wrap = I.Wrap.ghost ""

let sep_or_term_to_nelist : ('a, 'b) Utils.sep_or_term -> 'a List.Ne.t option =
  Option.map ~f:(function
      | `Sep x -> nsepseq_to_nseq x
      | `Term x -> nseq_map fst x)


let nsep_or_term_to_nelist : ('a, 'b) Utils.nsep_or_term -> 'a List.Ne.t = function
  | `Sep x -> nsepseq_to_nseq x
  | `Term x -> nseq_map fst x


let nsep_or_term_hd : ('a, 'b) Utils.nsep_or_term -> 'a * ('a, 'b) Utils.sep_or_term
  = function
  | `Sep (a, []) -> a, None
  | `Sep (a, (b, a_) :: tl) -> a, Some (`Sep (a_, tl))
  | `Term ((a, s), []) -> a, None
  | `Term ((a, s), (a_, b) :: tl) -> a, Some (`Term ((a_, b), tl))


module TODO_do_in_parsing = struct
  let conv_attr attr_reg =
    let (key, value), _loc = w_split attr_reg in
    let f = function
      | I.Attr.String x -> x
      | Ident x -> x
    in
    Nano_prim.Attribute.{ key; value = Option.map ~f value }


  let conv_attrs = List.map ~f:conv_attr
  let weird_attr _ = ()
  let unused_node () = failwith "unused node, can we clean ?"
  let labelize x = O.Label.of_string x
  let pattern_to_param pattern = O.Param.{ pattern; param_kind = `Const }

  let field_as_open_t (ma : I.type_expr) =
    (* here, we should use module expressions, maybe ? *)
    match ma with
    | I.T_Par t -> Some t.value.inside
    | _ -> None


  let is_open = function
    | I.E_Par _ -> true
    | _ -> false


  let control_flow_clause compile_statement (x : I.statement)
      : (I.statement, I.statements) O.Test_clause.t
    =
    (* if the statement is a block containing a single instruction,
       we do not want to emit a ClauseBlock, but a ClauseInstr *)
    let single_stmt_block (x : I.statement) = List.Ne.singleton @@ (x, None) in
    match Location.unwrap @@ compile_statement x with
    | O.S_instr (I.S_Block { value = { inside; _ }; _ }) ->
      (match nseq_to_list inside with
      | [ (one, _) ] ->
        (match Location.unwrap @@ compile_statement one with
        | S_instr i -> O.Test_clause.ClauseInstr i
        | _ -> O.Test_clause.ClauseBlock (single_stmt_block x))
      | _ -> O.Test_clause.ClauseBlock inside)
    | S_instr i -> O.Test_clause.ClauseInstr i
    | _ -> O.Test_clause.ClauseBlock (single_stmt_block x)


  let get_var = function
    | I.Var v | I.Esc v -> v


  let mvar x = Ligo_prim.Module_var.of_input_var ~loc:(Location.File x#region) x#payload
  let var x = Ligo_prim.Value_var.of_input_var ~loc:(Location.File x#region) x#payload

  let esc_var x =
    let x = get_var x in
    Ligo_prim.Value_var.of_input_var ~loc:(Location.File x#region) x#payload


  let tvar x = Ligo_prim.Type_var.of_input_var ~loc:(Location.File x#region) x#payload

  let esc_tvar x =
    let x = get_var x in
    Ligo_prim.Type_var.of_input_var ~loc:(Location.File x#region) x#payload


  let selection_path (t : I.namespace_selection) =
    match t with
    | M_Alias p -> List.Ne.singleton p
    | M_Path path ->
      let I.{ namespace_path; property = last; _ } = path.value in
      let init = nsepseq_to_list namespace_path in
      List.Ne.of_list (init @ [ last ])


  let compile_rows = O.Non_linear_rows.make
end

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
  type program = I.t
  type sig_expr = I.intf_expr
  type sig_entry = I.intf_entry

  let not_part_of_the_language _ = assert false
end

let pattern_of_expr x = `Expr x
let pattern_of_pattern x = `Pattern x

module Folding = Folding (Eq)

let rec expr : Eq.expr -> Folding.expr =
 fun e ->
  let loc = Location.lift (I.expr_to_region e) in
  let return = Location.wrap ~loc in
  let compile_bin_op (sign : O.Operators.op) (op : _ I.bin_op Region.reg) =
    let I.{ op = _; arg1; arg2 } = r_fst op in
    O.E_binary_op { operator = Location.wrap ~loc sign; left = arg1; right = arg2 }
  in
  let compile_unary_op (sign : AST.Operators.op) op =
    let I.{ op = _; arg } = r_fst op in
    O.E_unary_op { operator = Location.wrap ~loc sign; arg }
  in
  let compile_function (type_vars : I.generics option) parameters rhs_type fun_body =
    let type_params =
      let open Simple_utils.Option in
      let* type_vars in
      let* tvs = sep_or_term_to_nelist type_vars.value.inside in
      return (List.Ne.map TODO_do_in_parsing.esc_tvar tvs)
    in
    let parameters =
      match parameters with
      | I.ParParams x ->
        x.value.inside
        |> sep_or_term_to_list
        |> List.map ~f:TODO_do_in_parsing.pattern_to_param
      | NakedParam x -> [ TODO_do_in_parsing.pattern_to_param x ]
    in
    let ret_type = Option.map ~f:snd rhs_type in
    match fun_body with
    | I.StmtBody body ->
      return
      @@ O.E_block_poly_fun
           { type_params; parameters; ret_type; body = body.value.inside }
    | ExprBody body -> return @@ E_poly_fun { type_params; parameters; ret_type; body }
  in
  let ctor_app_kind_to_expr : I.ctor_app_kind -> I.expr = function
    | CtorStr ctor -> E_String ctor
    | CtorName ctor -> E_String ctor
  in
  match e with
  | E_Var (Var var) -> return @@ O.E_variable_esc (Raw (TODO_do_in_parsing.var var))
  | E_Var (Esc var) -> return @@ O.E_variable_esc (Esc (TODO_do_in_parsing.var var))
  | E_Par par -> expr par.value.inside
  | E_False _ -> return @@ E_constr (Ligo_prim.Label.of_string "False")
  | E_True _ -> return @@ E_constr (Ligo_prim.Label.of_string "True")
  | E_Bytes b ->
    let _lexeme, b = b#payload in
    return @@ E_literal (Literal_bytes (Hex.to_bytes b))
  | E_String str ->
    return @@ E_literal (Literal_string (Simple_utils.Ligo_string.Standard str#payload))
  | E_Verbatim str ->
    return @@ E_literal (Literal_string (Simple_utils.Ligo_string.Verbatim str#payload))
  | E_Add plus -> return @@ compile_bin_op PLUS plus
  | E_Sub minus -> return @@ compile_bin_op MINUS minus
  | E_Mult times -> return @@ compile_bin_op STAR times
  | E_Div slash -> return @@ compile_bin_op SLASH slash
  | E_Rem mod_ -> return @@ compile_bin_op PRCENT mod_
  | E_Neg minus -> return @@ compile_unary_op MINUS minus
  | E_Int i -> return @@ E_literal (Literal_int (snd i#payload))
  | E_Or or_ -> return @@ compile_bin_op DPIPE or_
  | E_And and_ -> return @@ compile_bin_op DAMPERSAND and_
  | E_Not not_ -> return @@ compile_unary_op EX_MARK not_
  | E_Lt lt -> return @@ compile_bin_op LT lt
  | E_Leq le -> return @@ compile_bin_op LE le
  | E_Gt gt -> return @@ compile_bin_op GT gt
  | E_Geq ge -> return @@ compile_bin_op GE ge
  | E_Equal eq -> return @@ compile_bin_op DEQ eq
  | E_Neq ne -> return @@ compile_bin_op EQ_SLASH_EQ ne
  | E_App { value = expr, args; _ } ->
    let args = sepseq_to_list args.value.inside in
    return @@ E_call (expr, Location.wrap ~loc @@ args)
  | E_CtorApp (Variant { value = { attributes = _; tuple }; region = _ }) ->
    return
    @@
    (match tuple with
    | _sharp, ZeroArg ctor -> E_ctor_app (ctor_app_kind_to_expr ctor, None)
    | _sharp, MultArg (ctor, args) ->
      let args = nsep_or_term_to_nelist args.value.inside in
      E_ctor_app (ctor_app_kind_to_expr ctor, Some args))
  | E_CtorApp (Bracketed { value = { attributes = _; sharp = _; tuple }; region = _ }) ->
    let ({ ctor; args } : I.expr I.bracketed_variant_args) = tuple.value.inside in
    return
    @@
    (match args with
    | None -> E_ctor_app (ctor, None)
    | Some (_comma, args) ->
      let args = sep_or_term_to_nelist args in
      E_ctor_app (ctor, args))
  | E_CtorApp (Legacy { value = { attributes = _; tuple }; region = _ }) ->
    let ({ ctor; args } : I.expr I.legacy_variant_args) = tuple.value.inside in
    let args = List.Ne.of_list_opt @@ List.map ~f:snd args in
    return @@ E_ctor_app (E_String ctor, args)
  | E_Array { value = items; _ } ->
    let items =
      let translate_array_item : I.expr I.element -> _ AST.Array_repr.item = function
        | None, e -> Expr_entry e
        | Some _, e -> Rest_entry e
      in
      Option.value_map items.inside ~default:[] ~f:(fun lst ->
          List.map ~f:translate_array_item (nsep_or_term_to_list lst))
    in
    return @@ E_array items
  | E_Object { value; _ } ->
    let f x =
      let I.{ attributes; property_id; property_rhs } = r_fst x in
      TODO_do_in_parsing.weird_attr attributes;
      let open O.Object_ in
      let field_id =
        match property_id with
        | F_Name n -> F_Name (O.Label.of_string (TODO_do_in_parsing.get_var n)#payload)
        | F_Int i -> F_Int (snd i#payload)
        | F_Str s -> F_Str s#payload
      in
      O.Object_.{ field_id; field_rhs = Option.map ~f:snd property_rhs }
    in
    return @@ E_object (List.map ~f (sep_or_term_to_list value.inside))
  | E_Update { value = { inside; _ }; _ } ->
    let I.{ _object; updates; _ } = inside in
    let f x =
      let I.{ attributes; property_id; property_rhs } = r_fst x in
      TODO_do_in_parsing.weird_attr attributes;
      let open O.Object_ in
      let field_id =
        match property_id with
        | F_Name n -> F_Name (O.Label.of_string (TODO_do_in_parsing.get_var n)#payload)
        | F_Int i -> F_Int (snd i#payload)
        | F_Str s -> F_Str s#payload
      in
      O.Object_.{ field_id; field_rhs = Option.map ~f:snd property_rhs }
    in
    let updates = List.map ~f (sep_or_term_to_list updates) in
    return @@ E_object_update { object_ = _object; updates }
  | E_Proj { value = { object_or_array; property_path }; _ } ->
    let f : I.selection -> _ O.Selection.t = function
      | I.PropertyStr fstr -> Component_expr I.(E_String fstr.value.inside)
      | I.PropertyName (_dot, name) ->
        FieldName (O.Label.of_string (TODO_do_in_parsing.get_var name)#payload)
      | Component comp ->
        let comp = (r_fst comp).inside#payload in
        Component_num comp
    in
    let property_path = nseq_map f property_path in
    return @@ E_proj (object_or_array, nseq_to_list @@ property_path)
  | E_NamePath { value = { namespace_path; property; _ }; _ } ->
    let property_as_open = TODO_do_in_parsing.is_open property in
    let namespace_path =
      nsepseq_to_nseq @@ nsepseq_map TODO_do_in_parsing.mvar namespace_path
    in
    return
    @@ E_module_open_in
         { module_path = namespace_path
         ; field = property
         ; field_as_open = property_as_open
         }
  | E_ArrowFun f ->
    let I.{ generics; parameters; rhs_type; arrow = _; fun_body } = f.value in
    compile_function generics parameters rhs_type fun_body
  | E_Function f ->
    let I.{ generics; parameters; rhs_type; kwd_function = _; fun_body } = f.value in
    compile_function generics parameters rhs_type fun_body
  | E_Typed a ->
    let e, _, te = a.value in
    return @@ E_annot (e, te)
  | E_CodeInj { value = { language; code; _ }; _ } ->
    let language = w_fst language in
    return @@ E_raw_code { language; code }
  (* | E_Seq seq -> return @@ E_sequence (nsepseq_to_list seq.value) *)
  | E_Assign { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    Location.wrap ~loc
    @@ O.E_struct_assign_chainable { expr1 = arg1; op = Eq; expr2 = arg2 }
  | E_AddEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator Plus_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_SubEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator Min_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_MultEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator Times_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_DivEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator Div_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_RemEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator Mod_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_BitAndEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator BitAnd_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_BitOrEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator BitOr_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_BitXorEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator BitXor_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_BitSlEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator BitSl_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_BitSrEq { value = { arg1; op; arg2 }; _ } ->
    let loc =
      Location.lift @@ Region.cover (I.expr_to_region arg1) (I.expr_to_region arg2)
    in
    let op = O.Assign_chainable.Assignment_operator BitSr_eq in
    Location.wrap ~loc @@ O.E_struct_assign_chainable { expr1 = arg1; op; expr2 = arg2 }
  | E_Ternary { value = { condition; truthy; falsy; _ }; _ } ->
    let ifnot = Some falsy in
    return @@ E_cond { test = condition; ifso = truthy; ifnot }
  | E_ContractOf
      { value = { namespace_path = { value = { inside = selection; _ }; _ }; _ }; _ } ->
    let selection = TODO_do_in_parsing.selection_path selection in
    let lst = List.Ne.map TODO_do_in_parsing.mvar selection in
    return @@ E_contract lst
  | E_PreIncr { region = _; value = { op; arg = expr } } ->
    let loc = Location.lift op#region in
    let pre_op = Location.wrap ~loc O.Prefix_postfix.Increment in
    return @@ E_prefix { pre_op; expr }
  | E_PreDecr { region = _; value = { op; arg = expr } } ->
    let loc = Location.lift op#region in
    let pre_op = Location.wrap ~loc O.Prefix_postfix.Decrement in
    return @@ E_prefix { pre_op; expr }
  | E_PostIncr { region = _; value = { op; arg = expr } } ->
    let loc = Location.lift op#region in
    let post_op = Location.wrap ~loc O.Prefix_postfix.Increment in
    return @@ E_postfix { post_op; expr }
  | E_PostDecr { region = _; value = { op; arg = expr } } ->
    let loc = Location.lift op#region in
    let post_op = Location.wrap ~loc O.Prefix_postfix.Decrement in
    return @@ E_postfix { post_op; expr }
  | E_Nat n -> return @@ E_literal (Literal_nat (snd n#payload))
  | E_Mutez m -> return @@ E_literal (Literal_mutez (Z.of_int64 (snd m#payload)))
  | E_BitAnd bitand -> return @@ compile_bin_op WORD_LAND bitand
  | E_BitNeg bitneg -> return @@ compile_unary_op WORD_NOT bitneg
  | E_BitOr bitor -> return @@ compile_bin_op WORD_LOR bitor
  | E_BitXor bitxor -> return @@ compile_bin_op WORD_LXOR bitxor
  | E_BitSl lsl_ -> return @@ compile_bin_op WORD_LSL lsl_
  | E_BitSr lsr_ -> return @@ compile_bin_op WORD_LSR lsr_
  | E_Xor lsl_ -> return @@ compile_bin_op WORD_XOR lsl_
  | E_Attr (x, y) -> return @@ E_attr (TODO_do_in_parsing.conv_attr x, y)
  | E_Match { region = _; value } ->
    let I.{ kwd_match = _; subject; clauses } = value in
    let aux : I.match_clause I.reg -> (_, _) O.Match_tc39.match_clause =
     fun { value = { filter; clause_expr; _ }; _ } ->
      { filter = filter.value.inside; clause_expr }
    in
    let match_clauses =
      match clauses.value.inside with
      | AllClauses (clauses, default_expr) ->
        let clauses = List.Ne.map aux clauses in
        let default_opt = Option.map ~f:(fun x -> x.value.default_expr) default_expr in
        O.Match_tc39.AllClauses (clauses, default_opt)
      | DefaultClause { value; _ } -> O.Match_tc39.DefaultClause value.default_expr
    in
    return @@ E_match_tc39 { subject = subject.value.inside; match_clauses }
  | E_Do { region = _; value } ->
    let I.{ kwd_do = _; statements } = value in
    return @@ E_do statements.value.inside


let rec ty_expr : Eq.ty_expr -> Folding.ty_expr =
 fun t ->
  let loc = Location.lift (I.type_expr_to_region t) in
  let return = Location.wrap ~loc in
  let get_ty_variable (e : I.type_expr) =
    match e with
    | T_Var v -> Some v
    | _ -> None
  in
  let get_p_variable (e : I.pattern) =
    match e with
    | P_Var v -> Some v
    | _ -> None
  in
  match t with
  | T_Attr (attr, t) -> return @@ O.T_attr (TODO_do_in_parsing.conv_attr attr, t)
  | T_Array { value = { inside; _ }; _ } ->
    let t = List.Ne.of_list @@ nsep_or_term_to_list inside in
    return @@ T_prod t
  | T_Variant { value = variants; region } ->
    let variants = nsep_or_pref_to_list variants in
    let destruct : I.type_expr I.variant_kind -> _ = function
      | Variant { value = { tuple; attributes }; region = _ } ->
        let ctor, ctor_params =
          match snd tuple with
          | I.ZeroArg ctor -> ctor, None
          | MultArg (ctor, args) ->
            let args = nsep_or_term_to_nelist args.value.inside in
            ctor, Some args
        in
        let ctor =
          match ctor with
          | CtorStr s -> s
          | CtorName s -> s
        in
        let ctor_params : (I.type_expr, I.comma) nsep_or_term option =
          Option.map ~f:(fun x -> `Sep (nsepseq_of_nseq ~sep:ghost x)) ctor_params
        in
        let ty =
          match ctor_params with
          | None -> None
          | Some (`Sep (t, []) | `Term ((t, _), _)) -> Some t
          | Some ctor_params ->
            let inside : I.array_type =
              Region.wrap_ghost
              @@ I.{ lbracket = ghost; inside = ctor_params; rbracket = ghost }
            in
            Some (I.T_Array inside)
        in
        ( TODO_do_in_parsing.labelize ctor#payload
        , ty
        , TODO_do_in_parsing.conv_attrs attributes )
      | Bracketed { value = { tuple; sharp; attributes }; region = _ } ->
        let ({ ctor; args } : I.type_expr I.bracketed_variant_args) =
          tuple.value.inside
        in
        let ctor =
          match ctor with
          | T_String s -> s
          | _ -> failwith "Expected string from parser."
        in
        let ctor_params =
          Option.value_map ~default:None ~f:(sep_or_term_to_nelist <@ snd) args
        in
        let ctor_params : (I.type_expr, I.comma) nsep_or_term option =
          Option.map ~f:(fun x -> `Sep (nsepseq_of_nseq ~sep:ghost x)) ctor_params
        in
        let ty =
          match ctor_params with
          | None -> None
          | Some (`Sep (t, []) | `Term ((t, _), _)) -> Some t
          | Some ctor_params ->
            let inside : I.array_type =
              Region.wrap_ghost
              @@ I.{ lbracket = ghost; inside = ctor_params; rbracket = ghost }
            in
            Some (I.T_Array inside)
        in
        ( TODO_do_in_parsing.labelize ctor#payload
        , ty
        , TODO_do_in_parsing.conv_attrs attributes )
      | Legacy { value = { attributes; tuple }; region } ->
        let ({ ctor; args } : I.type_expr I.legacy_variant_args) = tuple.value.inside in
        let ctor_params =
          args |> List.map ~f:(fun (x, y) -> y, x) |> List.Ne.of_list_opt
        in
        let ty =
          match ctor_params with
          | None -> None
          | Some ((t, _), []) -> Some t
          | Some ctor_params ->
            let inside : I.array_type =
              { value =
                  I.{ lbracket = ghost; inside = `Term ctor_params; rbracket = ghost }
              ; region
              }
            in
            Some (I.T_Array inside)
        in
        ( TODO_do_in_parsing.labelize ctor#payload
        , ty
        , TODO_do_in_parsing.conv_attrs attributes )
    in
    let variants = variants |> List.map ~f:destruct |> TODO_do_in_parsing.compile_rows in
    return @@ T_sum_raw variants
  | T_Object { value = { inside = ne_elements; _ }; region } ->
    let fields =
      let destruct (I.{ property_id; property_rhs; attributes } : _ I.property) =
        let property_id =
          match property_id with
          | F_Name n -> O.Label.of_string (TODO_do_in_parsing.get_var n)#payload
          | F_Int i -> O.Label.of_string @@ fst i#payload
          | F_Str s -> O.Label.of_string s#payload
        in
        let property_rhs = Option.map ~f:snd property_rhs in
        property_id, property_rhs, TODO_do_in_parsing.conv_attrs attributes
      in
      let lst = List.map ~f:(destruct <@ r_fst) @@ sep_or_term_to_list ne_elements in
      O.Non_linear_rows.make lst
    in
    return @@ T_record_raw fields
  | T_App t ->
    let constr, args = t.value in
    let args = args.value.inside in
    let type_args = List.Ne.of_list @@ nsep_or_term_to_list args in
    return @@ T_app { constr; type_args }
  | T_Fun { value = fta, _, te2; _ } ->
    let fun_type_args =
      let compile_fun_type_arg : I.fun_type_param Region.reg -> _ O.Named_fun.fun_type_arg
        =
       fun { value = pat, type_expr; _ } ->
        let name =
          match get_p_variable pat with
          | Some pvar -> pvar
          | None -> failwith "Expected pattern variable"
        in
        let type_expr = snd type_expr in
        { name = (TODO_do_in_parsing.get_var name)#payload; type_expr }
      in
      List.map ~f:compile_fun_type_arg (sep_or_term_to_list fta.value.inside)
    in
    let type_expr = te2 in
    return @@ T_named_fun (fun_type_args, type_expr)
  | T_Par t -> ty_expr (r_fst t).inside
  | T_Var (Var t) -> return @@ T_var_esc (Raw (TODO_do_in_parsing.tvar t))
  | T_Var (Esc t) -> return @@ T_var_esc (Esc (TODO_do_in_parsing.tvar t))
  | T_String t -> return @@ T_string t#payload
  | T_Int t ->
    let s, z = t#payload in
    return @@ T_int (s, z)
  | T_NamePath { value = { namespace_path; property; _ }; _ } ->
    let namespace_path = List.Ne.of_list @@ nsepseq_to_list namespace_path in
    let module_path = List.Ne.map TODO_do_in_parsing.mvar namespace_path in
    let field_as_open, property =
      match TODO_do_in_parsing.field_as_open_t property with
      | Some t -> true, t
      | None -> false, property
    in
    let field =
      match get_ty_variable property with
      | Some tvar -> TODO_do_in_parsing.esc_tvar tvar
      | None -> failwith "Expected variable property."
    in
    return @@ T_module_access { module_path; field; field_as_open }
  | T_ParameterOf { value = { namespace_path; _ }; region } ->
    let namespace_path = TODO_do_in_parsing.selection_path namespace_path in
    let namespace_path = List.Ne.map TODO_do_in_parsing.mvar namespace_path in
    return @@ T_contract_parameter namespace_path
  | T_Union t ->
    let fields =
      let destruct_obj (x : I.type_expr I._object)
          : unit * I.type_expr * O.Attribute.t list
        =
        (), I.T_Object x, []
      in
      let lst = List.map ~f:destruct_obj (nsep_or_pref_to_list t.value) in
      O.Non_linear_disc_rows.make lst
    in
    Location.wrap ~loc @@ O.T_disc_union fields


let pattern : Eq.pattern -> Folding.pattern =
 fun p ->
  let loc = Location.lift (I.pattern_to_region p) in
  let return = Location.wrap ~loc in
  match p with
  | P_Attr (attr, p) -> return @@ O.P_attr (TODO_do_in_parsing.conv_attr attr, p)
  | P_CtorApp variant ->
    (match variant with
    | Variant { value = { attributes = _; tuple = _, app }; _ } ->
      let ctor, args =
        match app with
        | ZeroArg ctor -> ctor, []
        | MultArg (ctor, args) -> ctor, nsep_or_term_to_list args.value.inside
      in
      let ctor =
        match ctor with
        | CtorStr ctor -> ctor
        | CtorName ctor -> ctor
      in
      return @@ P_ctor_app (P_String ctor, args)
    | Bracketed { value = { attributes = _; sharp = _; tuple }; _ } ->
      let ({ ctor; args } : I.pattern I.bracketed_variant_args) = tuple.value.inside in
      let args = Option.value_map ~default:[] ~f:(sep_or_term_to_list <@ snd) args in
      return @@ P_ctor_app (ctor, args)
    | Legacy { value = { attributes = _; tuple }; _ } ->
      let ({ ctor; args } : I.pattern I.legacy_variant_args) = tuple.value.inside in
      return @@ P_ctor_app (P_String ctor, List.map ~f:snd args))
  | P_NamePath { value = { namespace_path; property; _ }; _ } ->
    let module_path = nseq_map TODO_do_in_parsing.mvar (nsepseq_to_nseq namespace_path) in
    return @@ P_mod_access { module_path; field = property; field_as_open = false }
  | P_False _ -> return @@ P_ctor (Ligo_prim.Label.of_string "False")
  | P_True _ -> return @@ P_ctor (Ligo_prim.Label.of_string "True")
  | P_Var (Var p) -> return @@ P_var_esc (Raw (TODO_do_in_parsing.var p))
  | P_Var (Esc p) -> return @@ P_var_esc (Esc (TODO_do_in_parsing.var p))
  | P_Int v -> return @@ P_literal (Literal_int (snd (w_fst v)))
  | P_Nat v -> return @@ P_literal (Literal_nat (snd (w_fst v)))
  | P_Mutez v -> return @@ P_literal (Literal_mutez (Z.of_int64 (snd (w_fst v))))
  | P_Bytes v -> return @@ P_literal (Literal_bytes (Hex.to_bytes (snd (w_fst v))))
  | P_String v ->
    return @@ P_literal (Literal_string (Simple_utils.Ligo_string.standard (w_fst v)))
  | P_Verbatim v ->
    return @@ P_literal (Literal_string (Simple_utils.Ligo_string.verbatim (w_fst v)))
  | P_Typed { value = pattern, (_, ty); _ } -> return @@ P_typed (ty, pattern)
  | P_Object { value = { inside = p; _ }; _ } ->
    let p = Utils.sep_or_term_to_list p in
    let compile_property_pattern ({ value; region } : I.pattern I.property Region.reg)
        : (O.Label.t, I.pattern) O.Field.t
      =
      let property_id = value.property_id in
      let property_id =
        match property_id with
        | F_Name n -> O.Label.of_string (TODO_do_in_parsing.get_var n)#payload
        | F_Int i -> O.Label.of_string @@ fst i#payload
        | F_Str s -> O.Label.of_string s#payload
      in
      match value.property_rhs with
      | Some (_, p) -> O.Field.Complete (property_id, p)
      | None -> O.Field.Punned Location.(wrap ~loc:(lift region) property_id)
    in
    let lps = List.map ~f:compile_property_pattern p in
    return @@ P_pun_record lps
  | P_Array { value = { inside = p; _ }; _ } ->
    let p = sep_or_term_to_list p in
    (match p with
    | lst ->
      let f (v : I.pattern I.element) =
        match v with
        | None, pattern -> O.{ pattern; ellipsis = false }
        | Some _, pattern -> { pattern; ellipsis = true }
      in
      return @@ P_tuple_with_ellipsis (List.map ~f p))


(* in JSLIGO, instruction ; statements and declaration are all statement *)

let block : Eq.block -> Folding.block =
 fun statements ->
  let locs =
    nseq_map (fun x -> Location.lift @@ I.statement_to_region @@ fst x) statements
  in
  let loc = List.Ne.fold_right1 ~f:Location.cover locs in
  let statements = nseq_map fst statements in
  Location.wrap ~loc statements


(* It seems we do no have module expressions in JsLIGO? *)
let mod_expr : Eq.mod_expr -> Folding.mod_expr =
 fun statements ->
  let locs =
    nseq_map (fun x -> Location.lift @@ I.statement_to_region @@ fst x) statements
  in
  let loc = List.Ne.fold_right1 ~f:Location.cover locs in
  Location.wrap ~loc (O.M_body I.{ statements; eof = ghost })


let rec statement : Eq.statement -> Folding.statement =
 fun s ->
  let loc = Location.lift (I.statement_to_region s) in
  let return = Location.wrap ~loc in
  match s with
  | S_Decl d -> return @@ O.S_decl d
  | S_Attr (attr, s) -> return @@ O.S_attr (TODO_do_in_parsing.conv_attr attr, s)
  | S_Export { value = _, decl; _ } -> return @@ O.S_export decl
  | S_Directive _ -> return @@ O.S_directive ()
  | S_Block _
  | S_Expr _
  | S_Return _
  | S_Switch _
  | S_Break _
  | S_Continue _
  | S_If _
  | S_While _
  | S_ForOf _
  | S_For _ -> return @@ S_instr s


and instruction : Eq.instruction -> Folding.instruction =
 fun i ->
  let loc = Location.lift (I.statement_to_region i) in
  let return = Location.wrap ~loc in
  let single_stmt_block (x : I.statement) = List.Ne.singleton @@ (x, None) in
  match i with
  | S_Continue _ -> return @@ O.I_continue
  | S_Block s -> return @@ O.I_block s.value.inside
  | S_Expr expr -> return @@ I_expr expr
  | S_If c ->
    let c = c.value in
    let I.{ if_so = if_so, _; if_not; test; _ } = c in
    let ifso = TODO_do_in_parsing.control_flow_clause statement if_so in
    let ifnot =
      Option.map if_not ~f:(TODO_do_in_parsing.control_flow_clause statement <@ snd)
    in
    return @@ I_cond { test = test.value.inside; ifso; ifnot }
  | S_Return s -> return @@ I_return (snd s.value)
  | S_Switch { value = { cases; subject; _ }; _ } ->
    let cases =
      match cases.value.inside with
      | AllCases (cases, default) ->
        let cases =
          List.Ne.map
            (fun case ->
              let I.{ expr; case_body; _ } = case.Region.value in
              O.Switch.{ expr; case_body })
            cases
        in
        let default_opt = Option.map ~f:(fun x -> x.value.default_body) default in
        O.Switch.AllCases (cases, default_opt)
      | Default case -> O.Switch.Default case.value.default_body
    in
    return @@ I_switch { subject = subject.value.inside; cases }
  | S_Break _ -> return @@ I_break
  | S_While s ->
    let I.{ invariant; while_body; _ } = s.value in
    let cond = invariant.value.inside in
    let block = single_stmt_block while_body in
    return @@ I_while { cond; block }
  | S_ForOf s ->
    let I.{ range; for_of_body; _ } = s.value in
    let I.{ index_kind; index; expr; _ } = range.value.inside in
    let index_kind =
      match index_kind with
      | `Let _ -> `Let
      | `Const _ -> `Const
    in
    let index = TODO_do_in_parsing.esc_var index in
    return @@ I_for_of { index_kind; index; expr; for_stmt = for_of_body }
  | S_For s ->
    let I.{ range; for_body; _ } = s.value in
    let I.{ initialiser; condition; afterthought; _ } = range.value.inside in
    let afterthought = Option.map afterthought ~f:Utils.nsepseq_to_nseq in
    return @@ I_for_stmt { initialiser; condition; afterthought; statement = for_body }
  (* impossible, if triggered, look at functions 'statement' *)
  | S_Directive _ | S_Decl _ | S_Export _ | S_Attr _ -> assert false


and declaration : Eq.declaration -> Folding.declaration =
 fun d ->
  let region = I.declaration_to_region d in
  let loc = Location.lift region in
  let return = Location.wrap ~loc in
  let return_region (value : _) : _ Region.reg = { value; region } in
  let compile_val_binding
      : I.val_binding -> (Eq.pattern, I.expr, I.type_expr) O.Simple_decl.t
    =
   fun { pattern; generics; rhs_type; eq = _; rhs_expr } ->
    let type_params =
      let open Simple_utils.Option in
      let* generics in
      let* tvs = sep_or_term_to_nelist (r_fst generics).inside in
      return (List.Ne.map TODO_do_in_parsing.esc_tvar tvs)
    in
    let rhs_type = Option.map ~f:snd rhs_type in
    { type_params; pattern; rhs_type; let_rhs = rhs_expr }
  in
  match d with
  | D_Namespace { value; _ } ->
    let I.{ kwd_namespace; namespace_name; namespace_type; namespace_body } = value in
    let annotation =
      match namespace_type with
      | None -> O.Mod_decl.{ signatures = []; filter = false }
      | Some { region; value = _, value } ->
        O.Mod_decl.{ signatures = nsepseq_to_list value; filter = false }
    in
    let name = TODO_do_in_parsing.mvar namespace_name in
    let mod_expr = namespace_body.value.inside in
    return @@ O.D_module { name; mod_expr; annotation }
  | D_Import s ->
    let import =
      match s with
      | ImportAlias { value = { alias; namespace_path; _ }; _ } ->
        let alias = TODO_do_in_parsing.mvar alias in
        let module_path =
          List.Ne.map
            TODO_do_in_parsing.mvar
            (TODO_do_in_parsing.selection_path namespace_path)
        in
        O.Import.Import_rename { alias; module_path }
      | ImportAllAs { value = { alias; file_path; _ }; _ } ->
        let alias = TODO_do_in_parsing.mvar alias in
        let module_str = file_path#payload in
        O.Import.Import_all_as { alias; module_str }
      | ImportFrom { value = { imported; file_path; _ }; _ } ->
        let imported =
          match sep_or_term_to_nelist (r_fst imported).inside with
          | Some imported -> imported
          | None -> failwith "Expected imported name?"
        in
        let imported = List.Ne.map TODO_do_in_parsing.esc_var imported in
        let module_str = file_path#payload in
        O.Import.Import_selected { imported; module_str }
    in
    return @@ D_import import
  | D_Interface { value; _ } ->
    let I.{ kwd_interface; intf_name; intf_extends; intf_body } = value in
    let name = TODO_do_in_parsing.mvar intf_name in
    let extends : I.intf_expr list =
      match intf_extends with
      | None -> []
      | Some { region; value = _, value } -> nsepseq_to_list value
    in
    return @@ O.D_signature { name; sig_expr = I_Body intf_body; extends }
  | D_Value { value; _ } ->
    let I.{ kind; bindings } = value in
    let bindings =
      List.Ne.map (compile_val_binding <@ r_fst) (nsepseq_to_nseq bindings)
    in
    (match kind with
    | `Let _ -> return @@ O.D_multi_var bindings
    | `Const _ -> return @@ O.D_multi_const bindings)
  | D_Type { value; region } ->
    let I.{ name; generics; type_expr; _ } = value in
    let name = TODO_do_in_parsing.esc_tvar name in
    let params =
      let open Simple_utils.Option in
      let* generics in
      let* tvs = sep_or_term_to_nelist (r_fst generics).inside in
      return (List.Ne.map TODO_do_in_parsing.esc_tvar tvs)
    in
    return @@ O.D_type_abstraction { name; params; type_expr }
  | D_Fun { value; _ } ->
    let I.{ kwd_function; fun_name; generics; parameters; rhs_type; fun_body } = value in
    let let_rhs : I.expr =
      let fun_body : I.fun_body = StmtBody fun_body in
      let parameters : I.arrow_fun_params = ParParams parameters in
      let function_expr : I.function_expr =
        { kwd_function; generics; parameters; rhs_type; fun_body }
      in
      E_Function (return_region function_expr)
    in
    let type_params =
      let open Simple_utils.Option in
      let* generics in
      let* tvs = sep_or_term_to_nelist generics.value.inside in
      return (List.Ne.map TODO_do_in_parsing.esc_tvar tvs)
    in
    let pattern : I.pattern = P_Var fun_name in
    return @@ O.D_multi_const ({ type_params; pattern; rhs_type = None; let_rhs }, [])


and program_entry : Eq.program_entry -> Folding.program_entry =
 fun s ->
  match Location.unwrap @@ statement s with
  | O.S_export d -> PE_export I.(S_Decl d)
  | O.S_decl d -> PE_declaration d
  | O.S_instr _ -> PE_top_level_instruction s
  | O.S_directive () -> PE_preproc_directive ()
  | O.S_attr (attr, s) -> PE_attr (attr, s)


and program : Eq.program -> Folding.program = function
  | { statements; eof = _ } -> List.map ~f:fst @@ nseq_to_list statements


and sig_expr : Eq.sig_expr -> Folding.sig_expr = function
  | I_Body { value = { inside; lbrace = _; rbrace = _ }; region } ->
    let loc = Location.lift region in
    let sig_items = sep_or_term_to_list inside in
    Location.wrap ~loc @@ O.S_body sig_items
  | I_Path selection ->
    let selection = TODO_do_in_parsing.selection_path selection in
    let locs =
      List.Ne.map (fun (n : I.namespace_name) -> Location.lift n#region) selection
    in
    let loc = List.Ne.fold_right1 ~f:Location.cover locs in
    let value = List.Ne.map TODO_do_in_parsing.mvar selection in
    Location.wrap ~loc @@ O.S_path value


and sig_entry : Eq.sig_entry -> Folding.sig_entry =
 fun se ->
  let return ~loc = Location.wrap ~loc in
  (* TODO: Wouldn't it better to have a region in I_Attr? *)
  let rec get_intf_entry_loc (x : I.intf_entry) : Location.t =
    match x with
    | I_Type { region; _ } -> Location.lift region
    | I_Const { region; _ } -> Location.lift region
    | I_Attr (attr, entry) ->
      Location.cover (Location.lift attr#region) @@ get_intf_entry_loc entry
  in
  let loc = get_intf_entry_loc se in
  match se with
  | I_Attr (attr, entry) ->
    return ~loc
    @@ (O.S_attr (TODO_do_in_parsing.conv_attr attr, entry) : _ O.sig_entry_content_)
  | I_Type { value; _ } ->
    let I.{ type_name; type_rhs; _ } = value in
    let var = TODO_do_in_parsing.esc_tvar type_name in
    (match type_rhs with
    | None -> return ~loc @@ O.S_type_var var
    | Some (_, type_rhs) -> return ~loc @@ O.S_type (var, type_rhs))
  | I_Const { value; _ } ->
    let I.{ const_name; const_type; const_optional; _ } = value in
    let var = TODO_do_in_parsing.esc_var const_name in
    let _, type_ = const_type in
    return ~loc @@ O.S_value (var, type_, Option.is_some const_optional)
