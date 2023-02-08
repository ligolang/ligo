open Lsp.Types
module Region = Simple_utils.Region
open Simple_utils.Utils
module Option = Caml.Option

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
let value_map ~f ~default value = Option.value (Option.map f value) ~default

let folding_range_cameligo : Cst.Cameligo.t -> FoldingRange.t list option =
 fun cst ->
  let open Cst.Cameligo in
  (* General *)
  let rec declaration_list value = nseq_concat_map value.decl ~f:declaration
  (* Module *)
  and module_alias _ = []
  (* Type expression *)
  and type_expr = function
    | TProd { value; region } -> nsepseq_concat_map value ~f:type_expr
    | TSum { value; region } ->
      mk_region region :: nsepseq_concat_map value.variants ~f:variant
    | TRecord { value; region } ->
      mk_region region
      :: nsepseq_concat_map value.ne_elements ~f:(fun value -> field_decl value.value)
    | TApp { value; region } -> mk_region region :: type_constr_arg (snd value)
    | TFun { value; region } ->
      let left, _arrow, right = value in
      type_expr left @ type_expr right
    | TPar { value; region } -> mk_region region :: type_expr value.inside
    | TVar _ | TString _ | TInt _ -> []
    | TModA { value; region } -> type_expr value.field
    | TArg _ -> []
  and type_constr_arg = function
    | CArg texp -> type_expr texp
    | CArgTuple { value; region } -> nsepseq_concat_map value.inside ~f:type_expr
  and variant { value; region } =
    mk_region region
    :: value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.arg
  (* Expression *)
  and expr = function
    | ECase { value; region } -> mk_region region :: case value
    | ECond { value; region } -> mk_region region :: cond_expr value
    | EAnnot { value; region } -> annot_expr value.inside
    | ELogic value -> logic_expr value
    | EArith value -> arith_expr value
    | EString value -> string_expr value
    | EList value -> list_expr value
    | EConstr { value; region } -> value_map ~f:expr ~default:[] (snd value)
    | ERecord { value; region } ->
      mk_region region
      :: nsepseq_concat_map value.ne_elements ~f:(fun value -> field_assign value.value)
    | EProj _ -> []
    | EModA { value; region } -> expr value.field
    | EUpdate { value; region } ->
      mk_region region
      :: nsepseq_concat_map value.updates.value.ne_elements ~f:(fun value ->
             field_path_assignment value.value)
    | ECall { value; region } -> expr (fst value) @ nseq_concat_map (snd value) ~f:expr
    | ETuple { value; region } -> nsepseq_concat_map value ~f:expr
    | EPar { value; region } -> mk_region region :: expr value.inside
    (* FIXME: LIGO provides no body ranges for the definition of a ... in ... *)
    | ELetIn { value; region } -> let_binding value.binding @ expr value.body
    | ELetMutIn { value; region } -> let_binding value.binding @ expr value.body
    | EAssign { value; region } -> expr value.expr
    | ETypeIn { value; region } -> type_decl value.type_decl @ expr value.body
    | EModIn { value; region } -> module_decl value.mod_decl @ expr value.body
    | EModAlias { value; region } -> module_alias value.mod_alias @ expr value.body
    | EFun { value; region } -> mk_region region :: fun_expr value
    | ESeq { value; region } ->
      mk_region region :: sepseq_concat_map value.elements ~f:expr
    | ECodeInj { value; region } -> mk_region region :: expr value.code
    | ERevApp { value; region } -> bin_op value
    | EWhile { value; region } -> mk_region region :: while_loop value
    | EFor { value; region } -> mk_region region :: for_loop value
    | EForIn { value; region } -> mk_region region :: for_in_loop value
    | EVar _ | EBytes _ | EUnit _ -> []
  and case value = expr value.expr @ cases value.cases
  and cases { value; region } = nsepseq_concat_map value ~f:case_clause
  and case_clause { value; region } =
    mk_region region :: (pattern value.pattern @ expr value.rhs)
  and cond_expr value =
    expr value.test
    @ expr value.ifso
    @ value_map ~f:(fun (_, exp) -> expr exp) ~default:[] value.ifnot
  and annot_expr (exp, _colon, texp) = expr exp @ type_expr texp
  and bin_op value = expr value.arg1 @ expr value.arg2
  and logic_expr = function
    | BoolExpr value -> bool_expr value
    | CompExpr value -> comp_expr value
  and bool_expr = function
    | Or { value; region } -> bin_op value
    | And { value; region } -> bin_op value
    | Not { value; region } -> expr value.arg
  and comp_expr = function
    | Lt { value; region } -> bin_op value
    | Leq { value; region } -> bin_op value
    | Gt { value; region } -> bin_op value
    | Geq { value; region } -> bin_op value
    | Equal { value; region } -> bin_op value
    | Neq { value; region } -> bin_op value
  and arith_expr = function
    | Add { value; region } -> bin_op value
    | Sub { value; region } -> bin_op value
    | Mult { value; region } -> bin_op value
    | Div { value; region } -> bin_op value
    | Mod { value; region } -> bin_op value
    | Land { value; region } -> bin_op value
    | Lor { value; region } -> bin_op value
    | Lxor { value; region } -> bin_op value
    | Lsl { value; region } -> bin_op value
    | Lsr { value; region } -> bin_op value
    | Neg { value; region } -> expr value.arg
    | Int _ | Nat _ | Mutez _ -> []
  and fun_expr value =
    nseq_concat_map value.binders ~f:pattern
    @ value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.rhs_type
    @ expr value.body
  and string_expr = function
    | Cat { value; region } -> bin_op value
    | String _ | Verbatim _ -> []
  and list_expr = function
    | ECons { value; region } -> bin_op value
    | EListComp { value; region } -> sepseq_concat_map value.elements ~f:expr
  and field_assign = function
    | Property value -> field_assign_property value
    | Punned_property _ -> []
  and field_assign_property value = expr value.field_expr
  and field_path_assignment = function
    | Path_property value -> field_path_assignment_property value
    | Path_punned_property _ -> []
  and field_path_assignment_property value = expr value.field_expr
  and while_loop value = expr value.cond @ loop_body value.body
  and for_loop value = expr value.bound1 @ expr value.bound2 @ loop_body value.body
  and for_in_loop value =
    pattern value.pattern @ expr value.collection @ loop_body value.body
  and loop_body value =
    value_map ~f:(nsepseq_concat_map ~f:expr) ~default:[] value.seq_expr
  and field_decl value = type_expr value.field_type
  (* Pattern *)
  and pattern = function
    | PConstr { value; region } -> value_map ~f:pattern ~default:[] (snd value)
    | PList value -> list_pattern value
    | PTuple { value; region } -> nsepseq_concat_map ~f:pattern value
    | PPar { value; region } -> mk_region region :: pattern value.inside
    | PRecord { value; region } ->
      mk_region region
      :: nsepseq_concat_map ~f:(fun value -> field_pattern value.value) value.ne_elements
    | PTyped { value; region } -> typed_pattern value
    | PUnit _ | PVar _ | PInt _ | PNat _ | PBytes _ | PString _ | PVerbatim _ -> []
  and list_pattern = function
    | PListComp { value; region } ->
      mk_region region :: sepseq_concat_map ~f:pattern value.elements
    | PCons { value; region } ->
      let left, _cons, right = value in
      pattern left @ pattern right
  and field_pattern value = pattern value.pattern
  and typed_pattern value = pattern value.pattern @ type_expr value.type_expr
  (* Declaration *)
  and type_decl value = type_expr value.type_expr
  and module_decl value = declaration_list value.module_
  and let_decl (_, _, value, _) = let_binding value
  and let_binding value =
    nseq_concat_map value.binders ~f:pattern
    @ value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.rhs_type
    @ expr value.let_rhs
  and declaration = function
    | Let { value; region } -> mk_region region :: let_decl value
    | TypeDecl { value; region } -> mk_region region :: type_decl value
    | ModuleDecl { value; region } -> mk_region region :: module_decl value
    | ModuleAlias { value; region } -> mk_region region :: module_alias value
    | Directive _ -> []
  in
  Some (declaration_list cst)


let folding_range_jsligo : Cst.Jsligo.t -> FoldingRange.t list option =
 fun cst ->
  let open Cst.Jsligo in
  (* General *)
  let rec statement_list value = nseq_concat_map value.statements ~f:toplevel_statement
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
    | SExpr value -> expr value
    | SCond { value; region } -> mk_region region :: cond_statement value
    | SReturn { value; region } ->
      mk_region region :: value_map ~f:expr ~default:[] value.expr
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
    @ value_map ~f:(fun (_, value) -> statement value) ~default:[] value.ifnot
  and while_statement value = expr value.expr @ statement value.statement
  and switch value = expr value.expr @ nseq_concat_map value.cases ~f:switch_case
  (* FIXME: LIGO doesn't provide range for switch case *)
  and switch_case = function
    | Switch_case value ->
      expr value.expr @ value_map ~f:statements ~default:[] value.statements
    | Switch_default_case value -> value_map ~f:statements ~default:[] value.statements
  and import = function
    | Import_rename _ | Import_all_as _ | Import_selected _ -> []
  and for_of value = expr value.expr @ statement value.statement
  (* Pattern *)
  and pattern = function
    | PAssign { value; region } -> expr value.value
    | PDestruct { value; region } -> val_binding value.target
    | PObject { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:pattern
    | PArray { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:pattern
    | PRest _ | PVar _ | PConstr _ -> []
  and val_binding { value; region } =
    mk_region region
    :: (pattern value.binders
       @ value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.lhs_type
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
    | TApp { value; region } -> nsepseq_concat_map ~f:type_expr (snd value).value.inside
    | TFun { value; region } ->
      let left, _arrow, right = value in
      let args = nsepseq_concat_map ~f:(fun arg -> type_expr arg.type_expr) left.inside in
      args @ type_expr right
    | TPar { value; region } -> mk_region region :: type_expr value.inside
    | TVar _ | TString _ | TInt _ -> []
    | TModA { value; region } -> type_expr value.field
    | TDisc value -> nsepseq_concat_map value ~f:obj_type
  and variant value =
    let tuple = value.tuple in
    mk_region tuple.region :: variant_comp tuple.value.inside
  and variant_comp value =
    value_map
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
    | EModA { value; region } -> expr value.field
    | ELogic value -> logic_expr value
    | EArith value -> arith_expr value
    | ECall { value; region } -> expr (fst value) @ arguments (snd value)
    | EArray { value; region } -> sepseq_concat_map value.inside ~f:array_item
    | EObject { value; region } ->
      mk_region region :: nsepseq_concat_map value.inside ~f:property
    | EString value -> string_expr value
    | EProj { value; region } -> expr value.expr
    | EAssign (left, _op, right) -> expr left @ expr right
    | EConstr { value; region } -> value_map ~f:expr ~default:[] (snd value)
    | EAnnot { value; region } -> annot_expr value
    | ECodeInj { value; region } -> mk_region region :: expr value.code
    | ETernary { value; region } ->
      mk_region region :: (expr value.condition @ expr value.truthy @ expr value.falsy)
    | EVar _ | EBytes _ | EUnit _ -> []
  and fun_expr value =
    expr value.parameters
    @ value_map ~f:(fun (_, texp) -> type_expr texp) ~default:[] value.lhs_type
    @ body value.body
  and bin_op value = expr value.arg1 @ expr value.arg2
  and logic_expr = function
    | BoolExpr value -> bool_expr value
    | CompExpr value -> comp_expr value
  and bool_expr = function
    | Or { value; region } -> bin_op value
    | And { value; region } -> bin_op value
    | Not { value; region } -> expr value.arg
  and comp_expr = function
    | Lt { value; region } -> bin_op value
    | Leq { value; region } -> bin_op value
    | Gt { value; region } -> bin_op value
    | Geq { value; region } -> bin_op value
    | Equal { value; region } -> bin_op value
    | Neq { value; region } -> bin_op value
  and arith_expr = function
    | Add { value; region } -> bin_op value
    | Sub { value; region } -> bin_op value
    | Mult { value; region } -> bin_op value
    | Div { value; region } -> bin_op value
    | Mod { value; region } -> bin_op value
    | Neg { value; region } -> expr value.arg
    | Int _ -> []
  and string_expr = function
    | String _ | Verbatim _ -> []
  and annot_expr (exp, _as, texp) = expr exp @ type_expr texp
  and body = function
    | FunctionBody { value; region } -> mk_region region :: statements value.inside
    | ExpressionBody value -> expr value
  and array_item = function
    | Expr_entry value -> expr value
    | Rest_entry { value; region } -> expr value.expr
  and property = function
    | Punned_property { value; region } -> expr value
    | Property { value; region } -> expr value.name @ expr value.value
    | Property_rest { value; region } -> expr value.expr
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
  | Utils.CameLIGO_cst cst -> Handler.return @@ folding_range_cameligo cst
  | Utils.JsLIGO_cst cst -> Handler.return @@ folding_range_jsligo cst
