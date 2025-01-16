open Core
open Unification_shared.Helpers
open Region

module Utils = Simple_utils.Utils
module Ligo_option = Simple_utils.Ligo_option
module Ligo_string = Simple_utils.Ligo_string
module Ne_list = Simple_utils.Ne_list
module O = Ast_unified
module I = Cst.Jsligo
module T = Typescript_ast.Ast_stripped

(* Utilities *)

let ( <@ ) f g x = f (g x)
let mk_reg region value = Region.{ region; value }

(* Is this really needed? Check unified -> core translation. *)
(*
let rec unspool_path : T.expr -> T.expr * T.variable list = function
  | E_member expr ->
     let expr, name = expr.value in
     let expr, path = unspool_path expr in
     expr, name :: path
  | expr -> expr, []

let unspool_path expr =
  let expr, path = unspool_path expr in
  expr, List.rev path
 *)

(* Generics *)

(* OLD *)

let split_for_all = function
  | I.T_ForAll { value = generics, type_expr; _ } -> Some generics, type_expr
  | type_expr -> None, type_expr


let split_for_all_opt = function
  | None -> None, None
  | Some te ->
    let gen, te = split_for_all te in
    gen, Some te


(* NEW *)

let split_for_all' = function
  | T.T_for_all { value = type_vars, type_expr; _ } -> Some type_vars, type_expr
  | type_expr -> None, type_expr


let split_for_all_opt' = function
  | None -> None, None
  | Some te ->
    let gen, te = split_for_all' te in
    gen, Some te


(* LISTS *)

(* TODO: Remove *)

let ghost : string I.wrap = I.Wrap.ghost ""

let sep_or_term_to_nelist : ('a, 'b) Utils.sep_or_term -> 'a Nonempty_list.t option =
  Option.map ~f:(function
      | `Sep x -> Utils.nsepseq_to_ne_list x
      | `Term x -> Nonempty_list.map ~f:fst x)


let nsep_or_term_to_nelist : ('a, 'b) Utils.nsep_or_term -> 'a Ne_list.t = function
  | `Sep x -> Utils.nsepseq_to_ne_list x
  | `Term x -> Nonempty_list.map ~f:fst x


let nsep_or_term_hd : ('a, 'b) Utils.nsep_or_term -> 'a * ('a, 'b) Utils.sep_or_term
  = function
  | `Sep (a, []) -> a, None
  | `Sep (a, (b, a_) :: tl) -> a, Some (`Sep (a_, tl))
  | `Term [ (a, s) ] -> a, None
  | `Term ((a, s) :: (a_, b) :: tl) -> a, Some (`Term ((a_, b) :: tl))


(* In parsing *)

module TODO_do_in_parsing = struct
  let conv_attr attr_reg =
    let (key, value), _loc = w_split attr_reg in
    let f = function
      | I.Attr.String x -> x
      | Ident x -> x
    in
    Nano_prim.Attribute.{ key; value = Option.map ~f value }


  let conv_attrs = List.map ~f:conv_attr

  let conv_decorator dec =
    let key, value = dec#payload in
    Nano_prim.Attribute.{ key; value }


  let conv_decorators = List.map ~f:conv_decorator
  let ignore_attr _ = ()
  let labelize x = O.Label.T.create ~loc:(w_snd x) (w_fst x)
  let pattern_to_param pattern = O.Param.{ pattern; param_kind = `Const }

  (* TODO: Remove *)
  let field_as_open_t (ma : I.type_expr) =
    (* here, we should use module expressions, maybe ? *)
    match ma with
    | I.T_Par t -> Some t.value.inside
    | _ -> None


  (* TODO: Remove *)
  let is_open = function
    | I.E_Par _ -> true
    | _ -> false


  let control_flow_clause compile_statement (x : I.statement)
      : (I.statement, I.statements) O.Test_clause.t
    =
    (* if the statement is a block containing a single instruction,
       we do not want to emit a ClauseBlock, but a ClauseInstr *)
    let single_stmt_block (x : I.statement) = Nonempty_list.singleton (x, None) in
    match Location.unwrap @@ compile_statement x with
    | O.S_instr (I.S_Block { value = { inside; _ }; _ }) ->
      (match Nonempty_list.to_list inside with
      | [ (one, _) ] ->
        (match Location.unwrap @@ compile_statement one with
        | S_instr i -> O.Test_clause.ClauseInstr i
        | _ -> O.Test_clause.ClauseBlock (single_stmt_block x))
      | _ -> O.Test_clause.ClauseBlock inside)
    | S_instr i -> O.Test_clause.ClauseInstr i
    | _ -> O.Test_clause.ClauseBlock (single_stmt_block x)


  let control_flow_clause' compile_statement (x : T.statement)
      : (T.statement, T.statement list) O.Test_clause.t
    =
    (* if the statement is a block containing a single instruction,
       we do not want to emit a ClauseBlock, but a ClauseInstr *)
    match Location.unwrap @@ compile_statement x with
    | O.S_instr (T.S_block { value; _ }) ->
      (match value with
      | [ one ] ->
        (match Location.unwrap @@ compile_statement one with
        | S_instr i -> O.Test_clause.ClauseInstr i
        | _ -> O.Test_clause.ClauseBlock [ x ])
      | _ -> O.Test_clause.ClauseBlock (Nonempty_list.to_list value))
    | S_instr i -> O.Test_clause.ClauseInstr i
    | _ -> O.Test_clause.ClauseBlock [ x ]


  (* TODO: Remove *)
  let get_var = function
    | I.Var v | I.Esc v -> v


  let esc_var x =
    let x = get_var x in
    Ligo_prim.Value_var.of_input_var ~loc:(Location.File x#region) x#payload


  let esc_tvar x =
    let x = get_var x in
    Ligo_prim.Type_var.of_input_var ~loc:(Location.File x#region) x#payload


  (* Keep *)

  let mvar x = Ligo_prim.Module_var.of_input_var ~loc:(Location.File x#region) x#payload
  let var x = Ligo_prim.Value_var.of_input_var ~loc:(Location.File x#region) x#payload
  let tvar x = Ligo_prim.Type_var.of_input_var ~loc:(Location.File x#region) x#payload

  (* OLD *)

  let selection_path (t : I.namespace_selection) =
    match t with
    | M_Alias p -> Nonempty_list.singleton p
    | M_Path path ->
      let I.{ namespace_path; property = last; _ } = path.value in
      let init = Utils.nsepseq_to_ne_list namespace_path in
      Ne_list.append init (Nonempty_list.singleton last)


  (* NEW *)

  let selection_path' (t : T.simple_path reg) : T.variable Nonempty_list.t =
    let T.{ path; selected } = t.value in
    let rev_path = List.rev path in
    let rev_path = Nonempty_list.(selected :: rev_path) in
    Nonempty_list.reverse rev_path


  let compile_rows = O.Non_linear_rows.make
end

(* OLD *)

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
end

(* NEW *)

module Eq' = struct
  type expr = T.expr
  type ty_expr = T.type_expr
  type pattern = T.pattern
  type statement = T.statement
  type block = T.statements
  type mod_expr = T.statements
  type instruction = T.statement
  type declaration = T.declaration
  type program_entry = T.statement
  type program = T.t
  type sig_expr = T.simple_path
  type sig_entry = T.intf_entry
end

let pattern_of_expr x = `Expr x
let pattern_of_pattern x = `Pattern x

(* OLD *)

module Folding_orig = Folding (* TEMPORARY (shadowing) *)
module Folding = Folding (Eq)

(* NEW *)

module Folding' = Folding_orig (Eq')

(* EXPRESSIONS *)

(* OLD *)

let rec expr : Eq.expr -> Folding.expr =
 fun e ->
  let loc = Location.lift (I.expr_to_region e) in
  let return = Location.wrap ~loc in
  let compile_bin_op (sign : O.Operators.op) (op : _ I.bin_op Region.reg) =
    let I.{ op = _; arg1; arg2 } = r_fst op in
    O.E_binary_op { operator = Location.wrap ~loc sign; left = arg1; right = arg2 }
  in
  let compile_unary_op (sign : O.Operators.op) op =
    let I.{ op = _; arg } = r_fst op in
    O.E_unary_op { operator = Location.wrap ~loc sign; arg }
  in
  let compile_function
      (type_vars : I.generics option)
      (parameters : I.arrow_fun_params)
      (rhs_type : I.type_annotation option)
      (fun_body : I.fun_body)
    =
    let type_params : O.Ty_variable.t Nonempty_list.t option =
      let open Ligo_option in
      let* type_vars in
      let* tvs = sep_or_term_to_nelist type_vars.value.inside in
      Option.return (Nonempty_list.map ~f:TODO_do_in_parsing.esc_tvar tvs)
    in
    let parameters : I.pattern O.Param.t list =
      match parameters with
      | I.ParParams x ->
        x.value.inside
        |> Utils.sep_or_term_to_list
        |> List.map ~f:TODO_do_in_parsing.pattern_to_param
      | NakedParam x -> [ TODO_do_in_parsing.pattern_to_param x ]
    in
    let ret_type : I.type_expr option = Option.map ~f:snd rhs_type in
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
    return @@ E_literal (Literal_string (Ligo_string.Standard str#payload))
  | E_Verbatim str ->
    return @@ E_literal (Literal_string (Ligo_string.Verbatim str#payload))
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
    let args = Utils.sepseq_to_list args.value.inside in
    return @@ E_call (expr, Location.wrap ~loc @@ args)
  | E_CtorApp (Variant { value = { attributes = _; tuple }; region = _ }) ->
    return
    @@
    (match tuple with
    | ZeroArg ctor -> E_ctor_app (ctor_app_kind_to_expr ctor, None)
    | MultArg (ctor, args) ->
      let args = nsep_or_term_to_nelist args.value.inside in
      E_ctor_app (ctor_app_kind_to_expr ctor, Some args))
  | E_CtorApp (Legacy { value = { attributes = _; tuple }; region = _ }) ->
    let ({ ctor; args } : I.expr I.legacy_variant_args) = tuple.value.inside in
    let args = Ne_list.of_list_opt @@ List.map ~f:snd args in
    return @@ E_ctor_app (E_String ctor, args)
  | E_Array { value = items; _ } ->
    let items =
      let translate_array_item : I.expr I.element -> _ AST.Array_repr.item = function
        | None, e -> Expr_entry e
        | Some _, e -> Rest_entry e
      in
      Option.value_map items.inside ~default:[] ~f:(fun lst ->
          List.map ~f:translate_array_item (Utils.nsep_or_term_to_list lst))
    in
    return @@ E_array items
  | E_Object { value; _ } ->
    let f x =
      let I.{ attributes; property_id; property_rhs } = r_fst x in
      let loc = r_snd x in
      TODO_do_in_parsing.ignore_attr attributes;
      let open O.Object_ in
      let field_id =
        match property_id with
        | F_Name n -> F_Name TODO_do_in_parsing.(labelize @@ get_var n)
        | F_Int i -> F_Int (snd i#payload)
        | F_Str s -> F_Str s#payload
      in
      Location.wrap
        ~loc
        O.Object_.{ field_id; field_rhs = Option.map ~f:snd property_rhs }
    in
    return @@ E_object (List.map ~f (Utils.sep_or_term_to_list value.inside))
  | E_Update { value = { inside; _ }; _ } ->
    let I.{ _object; updates; _ } = inside in
    let f x =
      let I.{ attributes; property_id; property_rhs } = r_fst x in
      let loc = r_snd x in
      TODO_do_in_parsing.ignore_attr attributes;
      let open O.Object_ in
      let field_id =
        match property_id with
        | F_Name n -> F_Name TODO_do_in_parsing.(labelize @@ get_var n)
        | F_Int i -> F_Int (snd i#payload)
        | F_Str s -> F_Str s#payload
      in
      Location.wrap
        ~loc
        O.Object_.{ field_id; field_rhs = Option.map ~f:snd property_rhs }
    in
    let updates = List.map ~f (Utils.sep_or_term_to_list updates) in
    return @@ E_object_update { object_ = _object; updates }
  | E_Proj { value = { object_or_array; property_path }; _ } ->
    let f : I.selection -> _ O.Selection.t = function
      | I.PropertyStr fstr -> Component_expr I.(E_String fstr.value.inside)
      | I.PropertyName (_dot, name) ->
        FieldName TODO_do_in_parsing.(labelize @@ get_var name)
      | Component comp ->
        let comp = (r_fst comp).inside#payload in
        Component_num comp
    in
    let property_path = Nonempty_list.map ~f property_path in
    return @@ E_proj (object_or_array, Nonempty_list.to_list property_path)
  | E_NamePath { value = { namespace_path; property; _ }; _ } ->
    let property_as_open = TODO_do_in_parsing.is_open property in
    let namespace_path =
      Utils.nsepseq_to_ne_list @@ Utils.nsepseq_map TODO_do_in_parsing.mvar namespace_path
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
  (* | E_Seq seq -> return @@ E_sequence (Utils.nsepseq_to_list seq.value) *)
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
    let lst = Nonempty_list.map ~f:TODO_do_in_parsing.mvar selection in
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
  | E_Tez m ->
    let mutez_bigint = Q.(to_bigint (mul (of_int 1_000_000) (snd m#payload))) in
    let mutez_int64 = Z.to_int64 mutez_bigint in
    return @@ E_literal (Literal_mutez (Z.of_int64 mutez_int64))
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
        let clauses = Nonempty_list.map ~f:aux clauses in
        let default_opt = Option.map ~f:(fun x -> x.value.default_expr) default_expr in
        O.Match_tc39.AllClauses (clauses, default_opt)
      | DefaultClause { value; _ } -> O.Match_tc39.DefaultClause value.default_expr
    in
    return @@ E_match_tc39 { subject = subject.value.inside; match_clauses }
  | E_Do { region = _; value } ->
    let I.{ kwd_do = _; statements } = value in
    return @@ E_do statements.value.inside

(* NEW *)

let compile_property (property : 'a T.property reg) =
  let T.
        { decorators = _
        ; comments = _
        ; property_name
        ; static = _
        ; optional = _
        ; property_rhs
        }
    =
    property.value
  in
  let field_id = O.Object_.F_Name TODO_do_in_parsing.(labelize property_name) in
  let field_rhs = Some property_rhs in
  let object_ = O.Object_.{ field_id; field_rhs } in
  Location.wrap ~loc:(Location.lift property.region) object_

let compile_properties (properties : 'a T.property reg list) =
  List.map ~f:compile_property properties

let compile_bin_op (sign : O.Operators.op) (op : (T.expr * T.expr) Region.reg) =
  let return x = Location.(wrap ~loc:(lift op.region)) x in
  let left, right = op.Region.value in
  return @@ O.E_binary_op { operator = return sign; left; right }

let compile_unary_op (sign : O.Operators.op) (op : T.expr Region.reg) =
  let return x = Location.(wrap ~loc:(lift op.region)) x in
  let arg = op.Region.value in
  return @@ O.E_unary_op { operator = return sign; arg }

let compile_postfix_op (expr : T.variable reg) op =
  let loc = Location.lift expr.region in
  let return x = Location.wrap ~loc x in
  let post_op = Location.wrap ~loc op in
  let expr = T.E_var expr.value in
  return @@ O.E_postfix { post_op; expr }

let compile_prefix_op (expr : T.variable reg) op =
  let loc = Location.lift expr.region in
  let return x = Location.wrap ~loc x in
  let pre_op = Location.wrap ~loc op in
  let expr = T.E_var expr.value in
  return @@ O.E_prefix { pre_op; expr }

let compile_chain_assignment op expr =
  let { value = expr1, expr2; region } = expr in
  let loc = Location.lift expr.region in
  let return x = Location.wrap ~loc x in
  let op = O.Assign_chainable.Assignment_operator op in
  return @@ O.E_struct_assign_chainable { expr1; op; expr2 }

let compile_function (expr : T.arrow_fun_expr reg) =
  let loc = Location.lift expr.region in
  let return x = Location.wrap ~loc x in
  let T.{ generics; parameters; rhs_type; fun_body } = expr.value in
  let type_params : O.Ty_variable.t Nonempty_list.t option =
    match generics with
    | [] -> None
    | t_var :: t_vars ->
      let t_var = TODO_do_in_parsing.tvar t_var
      and t_vars = List.map ~f:TODO_do_in_parsing.tvar t_vars in
      Some Nonempty_list.(t_var :: t_vars)
  in
  let parameters : T.pattern O.Param.t list =
    let f parameter =
      let Region.{ value; region } = parameter in
      let pattern, t_expr_opt = value in
      let pattern =
        match t_expr_opt with
        | None -> pattern
        | Some t_expr -> T.P_typed (mk_reg region (pattern, t_expr))
      in
      TODO_do_in_parsing.pattern_to_param pattern
    in
    List.map ~f parameters
  in
  let ret_type = rhs_type in
  return @@
  match fun_body with
  | T.Stmt_body body ->
    O.E_block_poly_fun { type_params; parameters; ret_type; body = body.value }
  | Expr_body body -> O.E_poly_fun { type_params; parameters; ret_type; body }

let expr' (expr : Eq'.expr) : Folding'.expr =
  let loc = Location.lift (T.region_of_expr expr) in
  let return x = Location.wrap ~loc x in
  match expr with
  | E_add expr -> compile_bin_op PLUS expr
  | E_add_eq expr -> compile_chain_assignment Plus_eq expr
  | E_and expr -> compile_bin_op DAMPERSAND expr
  | E_app { value = expr, args; _ } -> return @@ O.E_call (expr, return args)
  | E_array { value = items; _ } ->
    let f : T.expr T.element -> _ AST.Array_repr.item = function
      | Spread expr -> Rest_entry expr
      | Element expr -> Expr_entry expr
    in
    return @@ O.E_array (List.map ~f items)
  | E_arrow_fun expr -> compile_function expr
  | E_assign { value = expr1, expr2; _ } ->
    return @@ O.E_struct_assign_chainable { expr1; op = Eq; expr2 }
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
  | E_contract_of expr ->
    let path = TODO_do_in_parsing.selection_path' expr.value in
    let path = Nonempty_list.map ~f:TODO_do_in_parsing.mvar path in
    return @@ O.E_contract path
  | E_div expr -> compile_bin_op SLASH expr
  | E_div_eq expr -> compile_chain_assignment Div_eq expr
  | E_equal expr -> compile_bin_op DEQ expr
  | E_false _ -> return @@ O.E_constr (Ligo_prim.Label.of_string "False")
  | E_function expr -> compile_function expr
  | E_geq expr -> compile_bin_op GE expr
  | E_gt expr -> compile_bin_op GT expr
  | E_int expr -> return @@ O.E_literal (Literal_int (snd expr#payload))
  | E_leq expr -> compile_bin_op LE expr
  | E_lt expr -> compile_bin_op LT expr
  | E_member expr ->
    (* We assume that there is no need for unspooling [expr]. Correct? *)
    let expr, name = expr.value in
    let name = O.Selection.FieldName (TODO_do_in_parsing.labelize name) in
    return @@ O.E_proj (expr, [ name ])
  | E_michelson expr ->
    (* Module [Strip] wraps for now a [E_typed] around the
        [E_michelson], so we can safely ignore here the type
        expression. *)
    let language, code, _type_expr = expr.value in
    let code = T.E_template code in
    return @@ O.E_raw_code { language = language#payload; code }
  | E_mult expr -> compile_bin_op STAR expr
  | E_mult_eq expr -> compile_chain_assignment Times_eq expr
  | E_neg expr -> compile_unary_op MINUS expr
  | E_neq expr -> compile_bin_op EQ_SLASH_EQ expr
  | E_not expr -> compile_unary_op EX_MARK expr
  | E_object expr -> return @@ O.E_object (compile_properties expr.value)
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
    return @@ O.E_proj (expr, [ index ])
  | E_sub_eq expr -> compile_chain_assignment Min_eq expr
  | E_template expr ->
    return @@ O.E_literal (Literal_string (Ligo_string.Verbatim expr#payload))
  | E_ternary expr ->
    let T.{ condition; truthy; falsy } = expr.value in
    let ifnot = Some falsy in
    return @@ O.E_cond { test = condition; ifso = truthy; ifnot }
  | E_true _ -> return @@ O.E_constr (Ligo_prim.Label.of_string "True")
  | E_typed expr ->
    let expr, type_expr = expr.value in
    return @@ O.E_annot (expr, type_expr)
  | E_update expr ->
    let T.{ obj_expr; updates } = expr.value in
    let updates = compile_properties updates in
    return @@ O.E_object_update { object_ = obj_expr; updates }
  | E_var v -> return @@ O.E_variable_esc (Raw (TODO_do_in_parsing.var v))
  | E_xor expr -> compile_bin_op WORD_XOR expr

(* TYPE EXPRESSIONS *)

(* OLD *)

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
  | T_ForAll { value = generics, t; _ } ->
    let ty_binders =
      List.map ~f:TODO_do_in_parsing.esc_tvar
      @@ Utils.sep_or_term_to_list (r_fst generics).inside
    and kind = Ligo_prim.Kind.Type
    and type_ = t in
    return @@ O.T_for_alls { ty_binders; kind; type_ }
  | T_Attr (attr, t) -> return @@ O.T_attr (TODO_do_in_parsing.conv_attr attr, t)
  | T_Array { value = { inside; _ }; _ } ->
    let t = Utils.nsep_or_term_to_ne_list inside in
    return @@ T_prod t
  | T_Sum { value = variants; region } ->
    let variants = Utils.nsep_or_pref_to_list variants in
    let destruct : I.type_expr I.variant_kind -> _ = function
      | Variant { value = { tuple; attributes }; region = _ } ->
        let ctor, ctor_params =
          match tuple with
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
        let ctor_params : (I.type_expr, I.comma) Utils.nsep_or_term option =
          Option.map
            ~f:(fun x -> `Sep (Utils.nsepseq_of_ne_list ~sep:ghost x))
            ctor_params
        in
        let ty =
          match ctor_params with
          | None -> None
          | Some (`Sep (t, []) | `Term ((t, _) :: _)) -> Some t
          | Some ctor_params ->
            let inside : I.array_type =
              Region.wrap_ghost
              @@ I.{ lbracket = ghost; inside = ctor_params; rbracket = ghost }
            in
            Some (I.T_Array inside)
        in
        TODO_do_in_parsing.labelize ctor, ty, TODO_do_in_parsing.conv_attrs attributes
      | Legacy { value = { attributes; tuple }; region } ->
        let ({ ctor; args } : I.type_expr I.legacy_variant_args) = tuple.value.inside in
        let ctor_params =
          args |> List.map ~f:(fun (x, y) -> y, x) |> Ne_list.of_list_opt
        in
        let ty =
          match ctor_params with
          | None -> None
          | Some [ (t, _) ] -> Some t
          | Some ctor_params ->
            let inside : I.array_type =
              { value =
                  I.{ lbracket = ghost; inside = `Term ctor_params; rbracket = ghost }
              ; region
              }
            in
            Some (I.T_Array inside)
        in
        TODO_do_in_parsing.labelize ctor, ty, TODO_do_in_parsing.conv_attrs attributes
    in
    let variants = variants |> List.map ~f:destruct |> TODO_do_in_parsing.compile_rows in
    return @@ T_sum_raw variants
  | T_Object { value = { inside = ne_elements; _ }; region } ->
    let fields =
      let destruct (I.{ property_id; property_rhs; attributes } : _ I.property) =
        let property_id =
          match property_id with
          | F_Name n -> TODO_do_in_parsing.(labelize @@ get_var n)
          | F_Int i -> O.Label.of_string @@ fst i#payload
          | F_Str s -> TODO_do_in_parsing.labelize s
        in
        let property_rhs = Option.map ~f:snd property_rhs in
        property_id, property_rhs, TODO_do_in_parsing.conv_attrs attributes
      in
      let lst =
        List.map ~f:(destruct <@ r_fst) @@ Utils.sep_or_term_to_list ne_elements
      in
      O.Non_linear_rows.make lst
    in
    return @@ T_record_raw fields
  | T_App t ->
    let constr, args = t.value in
    let args = args.value.inside in
    let type_args = Utils.nsep_or_term_to_ne_list args in
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
      List.map ~f:compile_fun_type_arg (Utils.sep_or_term_to_list fta.value.inside)
    in
    let type_expr = te2 in
    return @@ T_named_fun (fun_type_args, type_expr)
  | T_Par t -> ty_expr (r_fst t).inside
  | T_String t -> return @@ T_string t#payload
  | T_Int t ->
    let s, z = t#payload in
    return @@ T_int (s, z)
  | T_Nat t ->
    let s, z = t#payload in
    return @@ T_nat (s, z)
  | T_Var (Var t) -> return @@ T_var_esc (Raw (TODO_do_in_parsing.tvar t))
  | T_Var (Esc t) -> return @@ T_var_esc (Esc (TODO_do_in_parsing.tvar t))
  | T_NamePath { value = { namespace_path; property; _ }; _ } ->
    let namespace_path = Utils.nsepseq_to_ne_list namespace_path in
    let module_path = Nonempty_list.map ~f:TODO_do_in_parsing.mvar namespace_path in
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
    let namespace_path = Nonempty_list.map ~f:TODO_do_in_parsing.mvar namespace_path in
    return @@ T_contract_parameter namespace_path
  | T_Union t ->
    let summands = Utils.nsep_or_pref_to_list t.value in
    Location.wrap ~loc @@ O.T_union summands

(* NEW *)

let compile_member_type (member : T.member_type reg) =
  let T.{ decorators; comments = _; static = _; property_name; optional = _; rhs_type } =
    member.value
  in
  let decorators = TODO_do_in_parsing.conv_decorators decorators in
  let property_name = TODO_do_in_parsing.(labelize property_name) in
  let property_rhs = Some rhs_type in
  property_name, property_rhs, decorators

let compile_parameter param : _ O.Named_fun.fun_type_arg =
  let name, type_expr = param.value in
  { name = name#payload; type_expr }

let rec type_expr' (type_expr : Eq'.ty_expr) : Folding'.ty_expr =
  let loc = Location.lift (T.region_of_type_expr type_expr) in
  let return x = Location.wrap ~loc x in
  match type_expr with
  | T_apply type_expr ->
    let constr, args = type_expr.value in
    (match args with
    | [] -> (* Should not happen *) type_expr' constr
    | fst_arg :: more_args ->
      let type_args = Nonempty_list.(fst_arg :: more_args) in
      return @@ O.T_app { constr; type_args })
  | T_tuple type_expr -> return @@ O.T_prod type_expr.value
  | T_for_all type_expr ->
    let type_vars, type_expr = type_expr.value in
    let ty_binders = List.map ~f:TODO_do_in_parsing.tvar type_vars
    and kind = Ligo_prim.Kind.Type
    and type_ = type_expr in
    return @@ O.T_for_alls { ty_binders; kind; type_ }
  | T_fun type_expr ->
    let parameters, ret_type = type_expr.value in
    let parameters = List.map ~f:compile_parameter parameters in
    return @@ O.T_named_fun (parameters, ret_type)
  | T_int t ->
    let s, z = t#payload in
    return @@ O.T_int (s, z)
  | T_object type_expr ->
    let members = List.map ~f:compile_member_type type_expr.value in
    let fields = O.Non_linear_rows.make members in
    return @@ O.T_record_raw fields
  | T_path type_expr ->
    let T.{ path; selected } = type_expr.value in
    (match path with
    | [] -> return @@ O.T_var_esc (Raw (TODO_do_in_parsing.tvar selected))
    | fst_mod :: other_mods ->
      let path = Nonempty_list.(fst_mod :: other_mods) in
      let module_path = Nonempty_list.map ~f:TODO_do_in_parsing.mvar path in
      let field_as_open = false in
      let field = TODO_do_in_parsing.tvar selected in
      return @@ O.T_module_access { module_path; field; field_as_open })
  | T_parameter_of type_expr ->
    let path = TODO_do_in_parsing.selection_path' type_expr.value in
    let path = Nonempty_list.map ~f:TODO_do_in_parsing.mvar path in
    return @@ O.T_contract_parameter path
  | T_string type_expr -> return @@ O.T_string type_expr#payload
  | T_union type_expr ->
    let variants = Nonempty_list.to_list type_expr.value in
    return @@ O.T_union variants

(* PATTERNS *)

(* OLD *)

let pattern : Eq.pattern -> Folding.pattern =
 fun p ->
  let loc = Location.lift (I.pattern_to_region p) in
  let return = Location.wrap ~loc in
  match p with
  | P_Attr (attr, p) -> return @@ O.P_attr (TODO_do_in_parsing.conv_attr attr, p)
  | P_CtorApp variant ->
    (match variant with
    | Variant { value = { attributes = _; tuple }; _ } ->
      let ctor, args =
        match tuple with
        | ZeroArg ctor -> ctor, []
        | MultArg (ctor, args) -> ctor, Utils.nsep_or_term_to_list args.value.inside
      in
      let ctor =
        match ctor with
        | CtorStr ctor -> ctor
        | CtorName ctor -> ctor
      in
      return @@ P_ctor_app (P_String ctor :: args)
    | Legacy { value = { attributes = _; tuple }; _ } ->
      let ({ ctor; args } : I.pattern I.legacy_variant_args) = tuple.value.inside in
      return @@ P_ctor_app (P_String ctor :: List.map ~f:snd args))
  | P_NamePath { value = { namespace_path; property; _ }; _ } ->
    let module_path =
      Nonempty_list.map
        ~f:TODO_do_in_parsing.mvar
        (Utils.nsepseq_to_ne_list namespace_path)
    in
    return @@ P_mod_access { module_path; field = property; field_as_open = false }
  | P_False _ -> return @@ P_ctor (Ligo_prim.Label.of_string "False")
  | P_True _ -> return @@ P_ctor (Ligo_prim.Label.of_string "True")
  | P_Var (Var p) -> return @@ P_var_esc (Raw (TODO_do_in_parsing.var p))
  | P_Var (Esc p) -> return @@ P_var_esc (Esc (TODO_do_in_parsing.var p))
  | P_Int v -> return @@ P_literal (Literal_int (snd (w_fst v)))
  | P_Nat v -> return @@ P_literal (Literal_nat (snd (w_fst v)))
  | P_Mutez v -> return @@ P_literal (Literal_mutez (Z.of_int64 (snd (w_fst v))))
  | P_Tez v ->
    let mutez_bigint = Q.(to_bigint (mul (of_int 1_000_000) (snd (w_fst v)))) in
    let mutez_int64 = Z.to_int64 mutez_bigint in
    return @@ P_literal (Literal_mutez (Z.of_int64 mutez_int64))
  | P_Bytes v -> return @@ P_literal (Literal_bytes (Hex.to_bytes (snd (w_fst v))))
  | P_String v -> return @@ P_literal (Literal_string (Ligo_string.standard (w_fst v)))
  | P_Verbatim v -> return @@ P_literal (Literal_string (Ligo_string.verbatim (w_fst v)))
  | P_Typed { value = pattern, (_, ty); _ } -> return @@ P_typed (ty, pattern)
  | P_Object { value = { inside = p; _ }; _ } ->
    let p = Utils.sep_or_term_to_list p in
    let compile_property_pattern ({ value; region } : I.pattern I.property Region.reg)
        : (O.Label.t, I.pattern) O.Field.t
      =
      let property_id = value.property_id in
      let property_id =
        match property_id with
        | F_Name n -> TODO_do_in_parsing.(labelize @@ get_var n)
        | F_Int i -> O.Label.of_string @@ fst i#payload
        | F_Str s -> TODO_do_in_parsing.labelize s
      in
      match value.property_rhs with
      | Some (_, p) -> O.Field.Complete (property_id, p)
      | None -> O.Field.Punned Location.(wrap ~loc:(lift region) property_id)
    in
    let lps = List.map ~f:compile_property_pattern p in
    return @@ P_pun_record lps
  | P_Array { value = { inside = p; _ }; _ } ->
    let p = Utils.sep_or_term_to_list p in
    (match p with
    | lst ->
      let f (v : I.pattern I.element) =
        match v with
        | None, pattern -> O.{ pattern; ellipsis = false }
        | Some _, pattern -> { pattern; ellipsis = true }
      in
      return @@ P_tuple_with_ellipsis (List.map ~f p))

(* NEW *)

let compile_property_pattern (property : T.pattern T.property Region.reg)
    : (O.Label.t, T.pattern) O.Field.t
  =
  let T.
        { decorators = _
        ; comments = _
        ; property_name
        ; static = _
        ; optional = _
        ; property_rhs
        }
    =
    property.value
  in
  let property_name = TODO_do_in_parsing.(labelize property_name) in
  O.Field.Complete (property_name, property_rhs)

let pattern' (pattern : Eq'.pattern) : Folding'.pattern =
  let loc = Location.lift (T.region_of_pattern pattern) in
  let return x = Location.wrap ~loc x in
  match pattern with
  | P_array { value; _ } ->
    let f (elem : T.pattern T.element) =
      match elem with
      | Spread pattern -> O.{ pattern; ellipsis = true }
      | Element pattern -> O.{ pattern; ellipsis = false }
    in
    return @@ O.P_tuple_with_ellipsis (List.map ~f value)
  | P_bytes pattern ->
    let bytes = Hex.to_bytes (snd pattern#payload) in
    return @@ O.P_literal (Literal_bytes bytes)
  | P_false _ -> return @@ O.P_ctor (Ligo_prim.Label.of_string "False")
  | P_int pattern -> return @@ O.P_literal (Literal_int (snd pattern#payload))
  | P_object pattern ->
    let fields = List.map ~f:compile_property_pattern pattern.value in
    return @@ O.P_pun_record fields
  | P_string pattern ->
    let string = Ligo_string.standard pattern#payload in
    return @@ O.P_literal (Literal_string string)
  | P_true _ -> return @@ O.P_ctor (Ligo_prim.Label.of_string "True")
  | P_var pattern ->
    let T.{ path; selected } = pattern.value in
    (match path with
    | [] -> return @@ O.P_var_esc (Raw (TODO_do_in_parsing.var selected))
    | fst_mod :: other_mods ->
      let path = Nonempty_list.(fst_mod :: other_mods) in
      let module_path = Nonempty_list.map ~f:TODO_do_in_parsing.mvar path in
      let field_as_open = false in
      let field = T.P_var (mk_reg selected#region T.{ path = []; selected }) in
      return @@ O.P_mod_access { module_path; field; field_as_open })
  | P_typed pattern ->
    let pattern, type_expr = pattern.value in
    return @@ O.P_typed (type_expr, pattern)

(* in JSLIGO, instruction ; statements and declaration are all statements *)

(* OLD *)

let block : Eq.block -> Folding.block =
 fun statements ->
  let locs =
    Nonempty_list.map
      ~f:(fun x -> Location.lift @@ I.statement_to_region @@ fst x)
      statements
  in
  let loc = Ne_list.fold_right1 ~f:Location.cover locs in
  let statements = Nonempty_list.map ~f:fst statements in
  Location.wrap ~loc statements

(* NEW *)

(* Shouldn't we use the type [statement list reg] for blocks? *)

(*
let block' : Eq'.block -> Folding'.block =
 fun statements ->
  let locs =
    List.map
      ~f:(fun stmt -> Location.lift @@ T.region_of_statement stmt)
      statements
  in
  let loc = List.fold_right ~f:Location.cover locs in
  Location.wrap ~loc statements
 *)

(* It seems we do no have module expressions in JsLIGO? *)
let mod_expr : Eq.mod_expr -> Folding.mod_expr =
 fun statements ->
  let locs =
    Nonempty_list.map
      ~f:(fun x -> Location.lift @@ I.statement_to_region @@ fst x)
      statements
  in
  let loc = Ne_list.fold_right1 ~f:Location.cover locs in
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
  let single_stmt_block (x : I.statement) = Nonempty_list.singleton @@ (x, None) in
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
          Nonempty_list.map
            ~f:(fun case ->
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
    return @@ I_for_of { index_kind; index; expr; for_stmt = for_of_body }
  | S_For s ->
    let I.{ range; for_body; _ } = s.value in
    let I.{ initialiser; condition; afterthought; _ } = range.value.inside in
    let afterthought = Option.map afterthought ~f:Utils.nsepseq_to_ne_list in
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
   fun { pattern; rhs_type; eq = _; rhs_expr } ->
    let rhs_type = Option.map ~f:snd rhs_type in
    let generics, rhs_type = split_for_all_opt rhs_type in
    let type_params =
      let open Ligo_option in
      let* generics in
      let* tvs = sep_or_term_to_nelist (r_fst generics).inside in
      Option.return (Nonempty_list.map ~f:TODO_do_in_parsing.esc_tvar tvs)
    in
    { type_params; pattern; rhs_type; let_rhs = rhs_expr }
  in
  match d with
  | D_Namespace { value; _ } ->
    let I.{ kwd_namespace; namespace_name; namespace_type; namespace_body } = value in
    let annotation =
      match namespace_type with
      | None -> O.Mod_decl.{ signatures = []; filter = false }
      | Some { region; value = _, value } ->
        O.Mod_decl.{ signatures = Utils.nsepseq_to_list value; filter = false }
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
          Nonempty_list.map
            ~f:TODO_do_in_parsing.mvar
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
        let imported = Nonempty_list.map ~f:TODO_do_in_parsing.esc_var imported in
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
      | Some { region; value = _, value } -> Utils.nsepseq_to_list value
    in
    return @@ O.D_signature { name; sig_expr = I_Body intf_body; extends }
  | D_Value { value; _ } ->
    let I.{ kind; bindings } = value in
    let bindings =
      Nonempty_list.map
        ~f:(compile_val_binding <@ r_fst)
        (Utils.nsepseq_to_ne_list bindings)
    in
    (match kind with
    | `Let _ -> return @@ O.D_multi_var bindings
    | `Const _ -> return @@ O.D_multi_const bindings)
  | D_Type { value; region } ->
    let I.{ name; type_expr; generics; _ } = value in
    let name = TODO_do_in_parsing.esc_tvar name in
    let params =
      let open Ligo_option in
      let* generics in
      let* tvs = sep_or_term_to_nelist (r_fst generics).inside in
      Option.return (Nonempty_list.map ~f:TODO_do_in_parsing.esc_tvar tvs)
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
      let open Ligo_option in
      let* generics in
      let* tvs = sep_or_term_to_nelist generics.value.inside in
      Option.return (Nonempty_list.map ~f:TODO_do_in_parsing.esc_tvar tvs)
    in
    let pattern : I.pattern = P_Var fun_name in
    return @@ O.D_multi_const [ { type_params; pattern; rhs_type = None; let_rhs } ]


and program_entry : Eq.program_entry -> Folding.program_entry =
 fun s ->
  match Location.unwrap @@ statement s with
  | O.S_export d -> PE_export I.(S_Decl d)
  | O.S_decl d -> PE_declaration d
  | O.S_instr _ -> PE_top_level_instruction s
  | O.S_directive () -> PE_preproc_directive ()
  | O.S_attr (attr, s) -> PE_attr (attr, s)


and program : Eq.program -> Folding.program = function
  | { statements; eof = _ } -> List.map ~f:fst @@ Nonempty_list.to_list statements


and sig_expr : Eq.sig_expr -> Folding.sig_expr = function
  | I_Body { value = { inside; lbrace = _; rbrace = _ }; region } ->
    let loc = Location.lift region in
    let sig_items = Utils.sep_or_term_to_list inside in
    Location.wrap ~loc @@ O.S_body sig_items
  | I_Path selection ->
    let selection = TODO_do_in_parsing.selection_path selection in
    let locs =
      Nonempty_list.map
        ~f:(fun (n : I.namespace_name) -> Location.lift n#region)
        selection
    in
    let loc = Ne_list.fold_right1 ~f:Location.cover locs in
    let value = Nonempty_list.map ~f:TODO_do_in_parsing.mvar selection in
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
    let I.{ kwd_type = _; type_name; type_rhs; generics } = value in
    let var = TODO_do_in_parsing.esc_tvar type_name in
    let generics =
      match generics with
      | None -> []
      | Some generics ->
        List.map ~f:TODO_do_in_parsing.esc_tvar
        @@ Utils.sep_or_term_to_list (r_fst generics).inside
    in
    (match type_rhs with
    | None -> return ~loc @@ O.S_type_var var
    | Some (_, type_rhs) -> return ~loc @@ O.S_type (var, generics, type_rhs))
  | I_Const { value; _ } ->
    let I.{ const_name; const_type; const_optional; _ } = value in
    let var = TODO_do_in_parsing.esc_var const_name in
    let _, type_ = const_type in
    return ~loc @@ O.S_value (var, type_, Option.is_some const_optional)
