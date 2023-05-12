open Lsp_helpers
open Simple_utils.Utils

let mk_folding_range : FoldingRangeKind.t -> Region.t -> FoldingRange.t =
 fun kind reg ->
  let line_diff = 1 in
  let character_diff = 0 in
  let to_column pos = pos#point_num - pos#point_bol + 1 - character_diff in
  let startLine = reg#start#line - line_diff in
  let endLine = reg#stop#line - line_diff in
  let startCharacter = Some (to_column reg#start) in
  let endCharacter = Some (to_column reg#stop) in
  FoldingRange.create ~startLine ?startCharacter ~endLine ?endCharacter ~kind ()


let mk_region : Region.t -> FoldingRange.t = mk_folding_range FoldingRangeKind.Region
let mk_imports : Region.t -> FoldingRange.t = mk_folding_range FoldingRangeKind.Imports
let nseq_concat_map nseq ~f = List.concat_map (nseq_to_list nseq) ~f
let nsepseq_concat_map nsepseq ~f = List.concat_map (nsepseq_to_list nsepseq) ~f
let sepseq_concat_map sepseq ~f = List.concat_map (sepseq_to_list sepseq) ~f

let folding_range_cameligo : Cst.Cameligo.t -> FoldingRange.t list option =
 fun cst ->
  let open! Cst.Cameligo in
  (* General *)
  let rec declaration_nseq decls = nseq_concat_map decls ~f:declaration
  (* Contract *)
  and contract _ = []
  (* Type expression *)
  and type_expr = function
    | T_App { value; region } ->
      let ctor, arg = value in
      (mk_region region :: type_expr ctor) @ type_ctor_arg arg
    | T_Arg { region; _ } -> [ mk_region region ]
    | T_Attr (_, texpr) -> type_expr texpr
    | T_Cart { value; _ } ->
      let fst_cmp, times, rest = value in
      let seq = Utils.nsepseq_cons fst_cmp times rest in
      nsepseq_concat_map ~f:type_expr seq
    | T_Fun { value; _ } ->
      let left, _arrow, right = value in
      type_expr left @ type_expr right
    | T_Int _ -> []
    | T_ModPath { value; _ } -> type_expr value.field
    | T_Par { value; region } -> mk_region region :: type_expr value.inside
    | T_Record { value; region } ->
      mk_region region
      :: sepseq_concat_map value.inside ~f:(fun value -> field_decl value.value)
    | T_String _ -> []
    | T_Variant { value; region } ->
      mk_region region :: nsepseq_concat_map value.variants ~f:variant
    | T_Var _ -> []
    | T_Parameter { value; region } -> mk_region region :: contract value (* FIXME *)
  and type_ctor_arg = function
    | TC_Single texp -> type_expr texp
    | TC_Tuple { value; _ } -> nsepseq_concat_map value.inside ~f:type_expr
  and variant { value; region } =
    mk_region region :: Option.value_map ~f:(type_expr <@ snd) ~default:[] value.ctor_args
  (* Expression *)
  and expr = function
    | E_Add { value; _ } -> bin_op value
    | E_And { value; _ } -> bin_op value
    | E_App { value; _ } -> expr (fst value) @ nseq_concat_map (snd value) ~f:expr
    | E_Assign { value; _ } -> expr value.expr
    | E_Attr (_, e) -> expr e
    | E_Bytes _ -> []
    | E_Cat { value; _ } -> bin_op value
    | E_CodeInj { value; region } -> mk_region region :: expr value.code
    | E_Cond { value; region } -> mk_region region :: cond_expr value
    | E_Cons { value; _ } -> bin_op value
    | E_Contract { value; region } -> mk_region region :: contract value (* FIXME *)
    | E_Ctor _ -> []
    | E_Div { value; _ } -> bin_op value
    | E_Equal { value; _ } -> bin_op value
    | E_For { value; region } -> mk_region region :: for_loop value
    | E_ForIn { value; region } -> mk_region region :: for_in_loop value
    | E_Fun { value; region } -> mk_region region :: fun_expr value
    | E_Geq { value; _ } -> bin_op value
    | E_Gt { value; _ } -> bin_op value
    | E_Int _ -> []
    | E_Land { value; _ } -> bin_op value
    | E_Leq { value; _ } -> bin_op value
    | E_LetIn { value; _ } -> let_binding value.binding @ expr value.body
    | E_LetMutIn { value; _ } -> let_binding value.binding @ expr value.body
    | E_List { value; _ } -> sepseq_concat_map ~f:expr value.inside
    | E_Lor { value; _ } -> bin_op value
    | E_Lsl { value; _ } -> bin_op value
    | E_Lsr { value; _ } -> bin_op value
    | E_Lt { value; _ } -> bin_op value
    | E_Lxor { value; _ } -> bin_op value
    | E_Match { value; region } -> mk_region region :: match_expr value
    | E_Mod { value; _ } -> bin_op value
    | E_ModIn { value; _ } -> module_decl value.mod_decl @ expr value.body
    | E_ModPath { value; _ } -> expr value.field
    | E_Mult { value; _ } -> bin_op value
    | E_Mutez _ -> []
    | E_Nat _ -> []
    | E_Neg { value; _ } -> expr value.arg
    | E_Neq { value; _ } -> bin_op value
    | E_Not { value; _ } -> expr value.arg
    | E_Or { value; _ } -> bin_op value
    | E_Par { value; region } -> mk_region region :: expr value.inside
    | E_Proj _ -> []
    | E_Record { value; region } ->
      mk_region region :: sepseq_concat_map value.inside ~f:field_assign
    | E_Sub { value; _ } -> bin_op value
    | E_String _ -> []
    | E_Tuple { value; _ } -> nsepseq_concat_map ~f:expr value
    | E_Typed { value; _ } -> typed_expr value.inside
    | E_TypeIn { value; _ } -> type_decl value.type_decl @ expr value.body
    | E_Unit _ -> []
    | E_Update { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside.updates ~f:field_path_assignment
    | E_Var _ -> []
    | E_Verbatim _ -> []
    | E_Seq { value; region } ->
      mk_region region :: sepseq_concat_map value.elements ~f:expr
    | E_RevApp { value; _ } -> bin_op value
    | E_While { value; region } -> mk_region region :: while_loop value

  and match_expr value = expr value.subject @ clauses value.clauses
  and clauses { value; _ } = nsepseq_concat_map value ~f:match_clause
  and match_clause { value; region } =
    mk_region region :: (pattern value.pattern @ expr value.rhs)
  and cond_expr value =
    expr value.test
    @ expr value.if_so
    @ Option.value_map ~f:(expr <@ snd) ~default:[] value.if_not
  and typed_expr (e, annot) = expr e @ type_annotation annot
  and bin_op value = expr value.arg1 @ expr value.arg2
  and fun_expr value =
    nseq_concat_map value.binders ~f:pattern
    @ type_annotation_opt value.rhs_type
    @ expr value.body
  and field_assign = function
    | Complete { region; value } -> mk_region region :: expr value.field_rhs
    | Punned _ -> []
  and field_path_assignment = function
    | Complete { region; value } -> mk_region region :: full_field_path_assignment value
    | Punned _ -> []
  and full_field_path_assignment value = expr value.field_rhs
  and field_decl value = type_annotation_opt value.field_type
  and type_annotation_opt (ta : type_annotation option) =
    Option.value_map ~default:[] ~f:type_annotation ta
  and type_annotation (_, texpr) = type_expr texpr
  (* Pattern *)
  and pattern = function
    | P_App { value; _ } -> Option.value_map ~f:pattern ~default:[] (snd value)
    | P_Attr (_, p) -> pattern p
    | P_Bytes _ -> []
    | P_Cons { value; _ } ->
      let left, _cons, right = value in
      pattern left @ pattern right
    | P_Ctor _ -> []
    | P_Int _ -> []
    | P_List { region; value } ->
      mk_region region :: sepseq_concat_map ~f:pattern value.inside
    | P_ModPath { value; _ } -> pattern value.field
    | P_Mutez _ -> []
    | P_Nat _ -> []
    | P_Par { value; region } -> mk_region region :: pattern value.inside
    | P_Record { value; region } ->
      mk_region region :: sepseq_concat_map value.inside ~f:field_pattern
    | P_String _ -> []
    | P_Tuple { value; _ } -> nsepseq_concat_map ~f:pattern value
    | P_Typed { value; _ } -> typed_pattern value
    | P_Var _ -> []
    | P_Verbatim _ -> []
    | P_Unit _ -> []
  and field_pattern = function
    | Complete { region; value } -> mk_region region :: pattern value.field_rhs
    | Punned _ -> []
  and typed_pattern (p, annot) = pattern p @ type_annotation annot
  and type_decl value = type_expr value.type_expr
  and module_decl value = module_expr value.module_expr
  and module_expr = function
    | M_Body { value; _ } -> module_body value
    | M_Path _ -> []
    | M_Var _ -> []
  and module_body value =
    List.fold_right
      ~f:(fun decl ranges -> declaration decl @ ranges)
      ~init:[]
      value.declarations
  and let_decl (_, _, value) = let_binding value
  and let_binding value =
    nseq_concat_map value.binders ~f:pattern
    @ Option.value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.rhs_type
    @ expr value.let_rhs
  and declaration = function
    | D_Attr { value; _ } -> declaration (snd value)
    | D_Directive _ -> []
    | D_Let { value; region } -> mk_region region :: let_decl value
    | D_Module { value; region } -> mk_region region :: module_decl value
    | D_Type { value; region } -> mk_region region :: type_decl value
  and while_loop value = expr value.cond @ loop_body value.body
  and for_loop value = expr value.bound1 @ expr value.bound2 @ loop_body value.body
  and for_in_loop value =
    pattern value.pattern @ expr value.collection @ loop_body value.body
  and loop_body { value ; _ } =
    Option.value_map ~f:(nsepseq_concat_map ~f:expr) ~default:[] value.seq_expr
  in
  Some (declaration_nseq cst.decl)


let folding_range_pascaligo : Cst.Pascaligo.t -> FoldingRange.t list option =
 fun _cst -> None (* TODO: #1691 *)


let folding_range_jsligo : Cst.Jsligo.t -> FoldingRange.t list option =
 fun cst ->
  let open! Cst.Jsligo in
  (* General *)
  let rec statement_list (value : Cst.Jsligo.t) =
    nseq_concat_map value.statements ~f:toplevel_statement
  and arguments = function
    | Multiple { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:expr
    | Unit _ -> []
  (* Statement *)
  and toplevel_statement = function
    | TopLevel (statement', _) -> statement statement'
    | Directive _ -> []
  and statement = function
    | SBlock { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:statement
    | SExpr (_attrs, value) -> expr value
    | SCond { value; region } -> mk_region region :: cond_statement value
    | SReturn { value; region } ->
      mk_region region :: Option.value_map ~f:expr ~default:[] value.expr
    | SLet { value; region } -> mk_region region :: let_decl value
    | SConst { value; region } -> mk_region region :: const_decl value
    | SType { value; region } -> mk_region region :: type_decl value
    | SSwitch { region; value } -> mk_region region :: switch value
    | SBreak _ -> []
    | SNamespace { region; value } ->
      let _namespace, _name, statements', _attrs = value in
      mk_region region :: statements statements'.value.inside
    | SExport { region; value } -> mk_region region :: statement (snd value)
    | SImport { region; value } -> mk_imports region :: import value
    | SWhile { region; value } -> mk_region region :: while_statement value
    | SForOf { region; value } -> mk_region region :: for_of value
  and statements value = nsepseq_concat_map value ~f:statement
  and cond_statement value =
    expr value.test.inside
    @ statement value.ifso
    @ Option.value_map ~f:(fun (_, value) -> statement value) ~default:[] value.ifnot
  and while_statement value = expr value.expr @ statement value.statement
  and switch value = expr value.expr @ nseq_concat_map value.cases ~f:switch_case
  (* FIXME: LIGO doesn't provide range for switch case *)
  and switch_case = function
    | Switch_case value ->
      expr value.expr @ Option.value_map ~f:statements ~default:[] value.statements
    | Switch_default_case value ->
      Option.value_map ~f:statements ~default:[] value.statements
  and import = function
    | Import_rename _ | Import_all_as _ | Import_selected _ -> []
  and contract _ = []
  and for_of value = expr value.expr @ statement value.statement
  (* Pattern *)
  and pattern = function
    | PAssign { value; _ } -> expr value.value
    | PDestruct { value; _ } -> val_binding value.target
    | PObject { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:pattern
    | PArray { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:pattern
    | PRest _ | PVar _ | PConstr _ -> []
  and val_binding { value; region } =
    mk_region region
    :: (pattern value.binders
       @ Option.value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.lhs_type
       @ expr value.expr)
  (* Type expression *)
  and type_expr = function
    | TProd value ->
      let inside = value.inside in
      mk_region inside.region :: nsepseq_concat_map inside.value.inside ~f:type_expr
    | TSum { value; region } ->
      mk_region region
      :: nsepseq_concat_map value.variants.value ~f:(fun value -> variant value.value)
    | TObject value -> obj_type value
    | TApp { value; _ } -> nsepseq_concat_map ~f:type_expr (snd value).value.inside
    | TFun { value; _ } ->
      let left, _arrow, right = value in
      let args = nsepseq_concat_map ~f:(fun arg -> type_expr arg.type_expr) left.inside in
      args @ type_expr right
    | TPar { value; region } -> mk_region region :: type_expr value.inside
    | TVar _ | TString _ | TInt _ -> []
    | TModA { value; _ } -> type_expr value.field
    | TDisc value -> nsepseq_concat_map value ~f:obj_type
    | TParameter { value; region } -> mk_region region :: contract value (* FIXME *)
  and variant value =
    let tuple = value.tuple in
    mk_region tuple.region :: variant_comp tuple.value.inside
  and variant_comp value =
    Option.value_map
      ~f:(fun (_, texp) -> nsepseq_concat_map ~f:type_expr texp)
      ~default:[]
      value.params
  and field_decl value = type_expr value.field_type
  and obj_type { value; region } =
    mk_region region
    :: nsepseq_concat_map value.ne_elements ~f:(fun value -> field_decl value.value)
  (* Expression *)
  and expr = function
    | EFun { value; region } -> mk_region region :: fun_expr value
    | EPar { value; region } -> mk_region region :: expr value.inside
    | ESeq { value; region } -> mk_region region :: nsepseq_concat_map value ~f:expr
    | EModA { value; _ } -> expr value.field
    | ELogic value -> logic_expr value
    | EArith value -> arith_expr value
    | ECall { value; _ } -> expr (fst value) @ arguments (snd value)
    | EArray { value; _ } -> sepseq_concat_map value.inside ~f:array_item
    | EObject { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:property
    | EString value -> string_expr value
    | EProj { value; _ } -> expr value.expr
    | EAssign (left, _op, right) -> expr left @ expr right
    | EConstr { value; _ } -> Option.value_map ~f:expr ~default:[] (snd value)
    | EAnnot { value; _ } -> annot_expr value
    | ECodeInj { value; region } -> mk_region region :: expr value.code
    | ETernary { value; region } ->
      mk_region region :: (expr value.condition @ expr value.truthy @ expr value.falsy)
    | EContract { value; region } -> mk_region region :: contract value (* FIXME *)
    | EVar _ | EBytes _ | EUnit _ -> []
  and fun_expr value =
    expr value.parameters
    @ Option.value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.lhs_type
    @ body value.body
  and bin_op value = expr value.arg1 @ expr value.arg2
  and logic_expr = function
    | BoolExpr value -> bool_expr value
    | CompExpr value -> comp_expr value
  and bool_expr = function
    | Or { value; _ } -> bin_op value
    | And { value; _ } -> bin_op value
    | Not { value; _ } -> expr value.arg
  and comp_expr = function
    | Lt { value; _ } -> bin_op value
    | Leq { value; _ } -> bin_op value
    | Gt { value; _ } -> bin_op value
    | Geq { value; _ } -> bin_op value
    | Equal { value; _ } -> bin_op value
    | Neq { value; _ } -> bin_op value
  and arith_expr = function
    | Add { value; _ } -> bin_op value
    | Sub { value; _ } -> bin_op value
    | Mult { value; _ } -> bin_op value
    | Div { value; _ } -> bin_op value
    | Mod { value; _ } -> bin_op value
    | Neg { value; _ } -> expr value.arg
    | Int _ -> []
  and string_expr = function
    | String _ | Verbatim _ -> []
  and annot_expr (exp, _as, texp) = expr exp @ type_expr texp
  and body = function
    | FunctionBody { value; region } -> mk_region region :: statements value.inside
    | ExpressionBody value -> expr value
  and array_item = function
    | Expr_entry value -> expr value
    | Rest_entry { value; _ } -> expr value.expr
  and property = function
    | Punned_property { value; _ } -> expr value
    | Property { value; _ } -> expr value.name @ expr value.value
    | Property_rest { value; _ } -> expr value.expr
  (* Declaration *)
  and let_decl value =
    nsepseq_concat_map value.bindings ~f:(fun value -> val_binding value)
  and const_decl value =
    nsepseq_concat_map value.bindings ~f:(fun value -> val_binding value)
  and type_decl value = type_expr value.type_expr in
  Some (statement_list cst)


let on_req_folding_range : DocumentUri.t -> FoldingRange.t list option Handler.t =
 fun uri ->
  Handler.with_cst uri None
  @@ function
  | Dialect_cst.CameLIGO_cst cst -> Handler.return @@ folding_range_cameligo cst
  | Dialect_cst.JsLIGO_cst cst -> Handler.return @@ folding_range_jsligo cst
  | Dialect_cst.PascaLIGO_cst cst -> Handler.return @@ folding_range_pascaligo cst
