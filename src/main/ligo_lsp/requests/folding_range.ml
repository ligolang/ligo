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


let folding_range_jsligo : Cst.Jsligo.t -> FoldingRange.t list option = fun cst -> None

let on_req_folding_range : DocumentUri.t -> FoldingRange.t list option Handler.t =
 fun uri ->
  let file_path = DocumentUri.to_path uri in
  let syntax = Syntax.of_ext_opt @@ Some (Caml.Filename.extension file_path) in
  let buffer = Ligo_compile.Utils.buffer_from_path file_path in
  let trace = Simple_utils.Trace.to_option in
  let ranges =
    match syntax with
    | None -> None
    | Some CameLIGO ->
      let cst = trace @@ Parsing.Cameligo.parse_file buffer file_path in
      Option.bind cst folding_range_cameligo
    | Some JsLIGO ->
      let cst = trace @@ Parsing.Jsligo.parse_file buffer file_path in
      Option.bind cst folding_range_jsligo
  in
  (*
  let format_hover ppf h =
    Format.fprintf ppf "%s" (Yojson.Safe.to_string @@ FoldingRange.yojson_of_t h)
  in
  let@ () =
    send_debug_msg
      (Format.asprintf
         "Ranges: %a"
         (Simple_utils.PP_helpers.option (Simple_utils.PP_helpers.list format_hover))
         ranges)
  in
  *)
  Handler.return ranges
