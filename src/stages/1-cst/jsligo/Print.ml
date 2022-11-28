(* PRINTING THE CST *)

(* This module produces an arborescent, textual representation of a
   subset of the Concrete Abstract Tree (CST). It aims at a readable
   format with the most relevant nodes, with source locations. This
   functionality is most useful when testing the parser, for example,
   checking that a particular node corresponding to an operator has
   the expected associativity with the same kind, or the expected
   priority over another. *)

[@@@coverage exclude_file]

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils = Simple_utils.Utils
module Region = Simple_utils.Region
open! Region (* TODO: Remove *)

(* Internal dependencies *)

module Tree = Cst_shared.Tree

type state = Tree.state
(*type label = Tree.label*)

open CST (* THE ONLY GLOBAL OPENING *)

(* UTILITIES *)

let sprintf = Printf.sprintf

(*type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq*)

let compact state (region : Region.t) = region#compact ~offsets:state#offsets state#mode

(** {1 Pretty-printing the AST} *)

let print_ident state { value = name; region } =
  let reg = compact state region in
  let node = sprintf "%s%s (%s)\n" state#pad_path name reg in
  Buffer.add_string state#buffer node


let print_node state name =
  let node = sprintf "%s%s\n" state#pad_path name in
  Buffer.add_string state#buffer node


let print_string state { value = name; region } =
  let reg = compact state region in
  let node = sprintf "%s%S (%s)\n" state#pad_path name reg in
  Buffer.add_string state#buffer node


(* TODO *)
let _print_verbatim state { value = name; region } =
  let reg = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path name reg in
  Buffer.add_string state#buffer node


let print_loc_node state name region = print_ident state { value = name; region }

let rec print_cst state { statements; _ } =
  let statements = Utils.nseq_to_list statements in
  let apply len rank = print_toplevel_statement (state#pad len rank) in
  print_node state "<ast>";
  List.iteri ~f:(List.length statements |> apply) statements


and print_toplevel_statement state = function
  | TopLevel (stmt, _) -> print_statement state stmt
  | Directive dir ->
    let region, string = Directive.project dir in
    print_loc_node state "Directive" region;
    print_node state string


and print_statement state = function
  | SBlock { value = { inside; _ }; region } ->
    print_loc_node state "SBlock" region;
    let statements = Utils.nsepseq_to_list inside in
    let apply len rank = print_statement (state#pad len rank) in
    List.iteri ~f:(List.length statements |> apply) statements
  | SExpr e ->
    print_node state "SExpr";
    print_expr (state#pad 1 0) e
  | SCond { value; region } ->
    print_loc_node state "SCond" region;
    print_cond_statement state value
  | SReturn { value = { expr; _ }; region } ->
    print_loc_node state "SReturn" region;
    (match expr with
    | Some e -> print_expr (state#pad 1 0) e
    | None -> ())
  | SLet stmt -> print_let_stmt state stmt
  | SConst stmt -> print_const_stmt state stmt
  | SType { value; region } ->
    print_loc_node state "SType" region;
    print_type_decl state value
  | SSwitch { value; region } ->
    print_loc_node state "SSwitch" region;
    print_switch_statement state value
  | SBreak kwd_break -> print_loc_node state "SBreak" kwd_break#region
  | SNamespace { value; region } ->
    print_loc_node state "SNamespace" region;
    print_namespace state value
  | SExport { value; region } ->
    print_loc_node state "SExport" region;
    print_statement state (snd value)
  | SImport { value; region } ->
    print_loc_node state "SImport" region;
    print_import state value
  | SForOf { value; region } ->
    print_loc_node state "SForOf" region;
    print_for_of state value
  | SWhile { value; region } ->
    print_loc_node state "SWhile" region;
    print_while state value


and print_let_stmt state (node : let_decl reg) =
  let ({ attributes; bindings; _ } : let_decl) = node.value in
  let val_bindings = Utils.nsepseq_to_list bindings in
  if not (List.is_empty attributes) then print_attributes state attributes;
  print_loc_node state "SLet" node.region;
  let len = List.length val_bindings in
  let apply rank = print_val_binding (state#pad len rank) in
  List.iteri ~f:apply val_bindings


and print_const_stmt state (node : const_decl reg) =
  let ({ attributes; bindings; _ } : const_decl) = node.value in
  let val_bindings = Utils.nsepseq_to_list bindings in
  if not (List.is_empty attributes) then print_attributes state attributes;
  print_loc_node state "SConst" node.region;
  let apply len rank = print_val_binding (state#pad len rank) in
  List.iteri ~f:(List.length val_bindings |> apply) val_bindings


and print_for_of state { index; expr; statement; _ } =
  print_ident state index;
  print_expr state expr;
  print_statement state statement


and print_while state { expr; statement; _ } =
  print_expr state expr;
  print_statement state statement


and print_import state import =
  match import with
  | Import_rename { alias; module_path; _ } ->
    print_node state "<alias>";
    print_ident state alias;
    let items = Utils.nsepseq_to_list module_path in
    let f p = print_ident state p in
    List.iter ~f items
  | Import_all_as { alias; module_path; _ } ->
    print_node state "<*>";
    print_ident state alias;
    print_string state module_path
  | Import_selected { imported; module_path; _ } ->
    print_node state "<selected>";
    let imported = Utils.nsepseq_to_list imported.value.inside in
    let apply len rank = print_ident (state#pad len rank) in
    List.iteri ~f:(List.length imported |> apply) imported;
    print_string state module_path


and print_namespace state (n, name, { value = { inside = statements; _ }; _ }, attributes)
  =
  print_loc_node state "<namespace>" n#region;
  print_attributes state attributes;
  print_ident state name;
  let statements = Utils.nsepseq_to_list statements in
  let apply len rank = print_statement (state#pad len rank) in
  List.iteri ~f:(List.length statements |> apply) statements


and print_switch_statement state node =
  let { expr; cases; _ } = node in
  print_node state "<expr>";
  print_expr (state#pad 1 0) expr;
  let cases = Utils.nseq_to_list cases in
  let length = List.length cases + 1 in
  let apply len rank = print_case (state#pad len (rank + 1)) in
  List.iteri ~f:(apply length) cases


and print_case state = function
  | Switch_case { expr; statements; _ } ->
    print_node state "<case>";
    print_expr state expr;
    (match statements with
    | Some statements ->
      let statements = Utils.nsepseq_to_list statements in
      let apply len rank = print_statement (state#pad len rank) in
      List.iteri ~f:(List.length statements |> apply) statements
    | None -> ())
  | Switch_default_case { statements; _ } ->
    print_node state "<default>";
    (match statements with
    | Some statements ->
      let statements = Utils.nsepseq_to_list statements in
      let apply len rank = print_statement (state#pad len rank) in
      List.iteri ~f:(List.length statements |> apply) statements
    | None -> ())


and print_val_binding state (node : val_binding reg) =
  let { binders; lhs_type; expr; _ } = node.value in
  let fields = if Option.is_none lhs_type then 2 else 3 in
  let arity = 0 in
  print_node state "<binding>";
  print_pattern (state#pad fields arity) binders;
  let arity =
    match lhs_type with
    | Some (_, type_expr) ->
      let state = state#pad fields (arity + 1) in
      print_node state "<lhs type>";
      print_type_expr (state#pad 1 0) type_expr;
      arity + 1
    | None -> arity
  in
  let state = state#pad fields (arity + 1) in
  print_node state "<expr>";
  print_expr (state#pad 1 0) expr


and print_pvar state (node : var_pattern reg) =
  let { variable; attributes } = node.value in
  if List.is_empty attributes
  then print_ident state variable
  else (
    print_node state "PVar";
    print_ident (state#pad 2 0) variable;
    print_attributes (state#pad 2 1) attributes)


and print_pattern state = function
  | PRest { value = { rest; _ }; region } ->
    print_loc_node state "<rest>" region;
    print_ident (state#pad 1 0) rest
  | PAssign { value = { property; value; _ }; region } ->
    print_loc_node state "<assign>" region;
    print_ident (state#pad 1 0) property;
    print_expr (state#pad 1 0) value
  | PVar v -> print_pvar state v
  | PConstr v ->
    print_node state "<constr>";
    print_ident (state#pad 1 0) v
  | PDestruct { value = { property; target; _ }; region } ->
    print_loc_node state "<destruct>" region;
    print_ident (state#pad 1 0) property;
    print_val_binding state target
  | PObject { value = { inside; _ }; region } ->
    print_loc_node state "<object>" region;
    let properties = Utils.nsepseq_to_list inside in
    let apply len rank = print_pattern (state#pad len rank) in
    List.iteri ~f:(List.length properties |> apply) properties
  | PArray { value = { inside; _ }; region } ->
    print_loc_node state "<array>" region;
    let items = Utils.nsepseq_to_list inside in
    let apply len rank = print_pattern (state#pad len rank) in
    List.iteri ~f:(List.length items |> apply) items


and print_type_decl state decl =
  print_ident (state#pad 2 0) decl.name;
  print_type_expr (state#pad 2 1) decl.type_expr


and print_ne_injection : 'a. (state -> 'a -> unit) -> state -> 'a ne_injection -> unit =
 fun printer state inj ->
  let ne_elements = Utils.nsepseq_to_list inj.ne_elements in
  let length = List.length ne_elements in
  let arity = if List.is_empty inj.attributes then length else length + 1
  and apply len rank = printer (state#pad len rank) in
  List.iteri ~f:(apply arity) ne_elements;
  let state = state#pad arity (arity - 1) in
  if not (List.is_empty inj.attributes) then print_attributes state inj.attributes


and print_bytes state { value = lexeme, hex; region } =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node (state#pad 2 1) (Hex.show hex)


and print_int state { value = lexeme, z; region } =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node (state#pad 2 1) (Z.to_string z)


and print_expr state = function
  | EFun { value; region } ->
    print_loc_node state "EFun" region;
    print_fun_expr state value
  | EPar { value = { inside; _ }; region } ->
    print_loc_node state "EPar" region;
    print_expr (state#pad 1 0) inside
  | ESeq { value; region } ->
    print_loc_node state "ESeq" region;
    let exprs = Utils.nsepseq_to_list value in
    let apply len rank = print_expr (state#pad len rank) in
    List.iteri ~f:(List.length exprs |> apply) exprs
  | EAssign (lhs, _, rhs) ->
    print_node state "EAssign";
    print_expr (state#pad 1 0) lhs;
    print_expr (state#pad 1 0) rhs
  | EVar v ->
    print_node state "EVar";
    print_ident (state#pad 1 0) v
  | ELogic e_logic ->
    print_node state "ELogic";
    print_e_logic (state#pad 1 0) e_logic
  | EArith e_arith ->
    print_node state "EArith";
    print_arith_expr (state#pad 1 0) e_arith
  | ECall { value; region } ->
    print_loc_node state "ECall" region;
    print_fun_call (state#pad 1 0) value
  | EBytes b ->
    print_node state "EBytes";
    print_bytes state b
  | EArray { value = { inside; _ }; region } ->
    print_loc_node state "EArray" region;
    (match inside with
    | Some inside ->
      let items = Utils.nsepseq_to_list inside in
      let apply len rank = print_array_item (state#pad len rank) in
      List.iteri ~f:(List.length items |> apply) items
    | None -> print_loc_node state "<empty>" region)
  | EConstr e_constr ->
    print_node state "EConstr";
    print_constr_expr (state#pad 1 0) e_constr
  | EObject { value = { inside; _ }; region } ->
    print_loc_node state "EObject" region;
    let properties = Utils.nsepseq_to_list inside in
    let apply len rank = print_property (state#pad len rank) in
    List.iteri ~f:(List.length properties |> apply) properties
  | EString e_string ->
    print_node state "EString";
    print_string_expr (state#pad 1 0) e_string
  | EProj { value = { expr; selection }; region } ->
    print_loc_node state "EProj" region;
    (* let state = state#pad 2 0 in *)
    print_expr (state#pad 2 0) expr;
    (match selection with
    | FieldName { value = { value; _ }; region } ->
      let state = state#pad 2 1 in
      print_loc_node state "<fieldname>" region;
      print_ident (state#pad 1 0) value
    | Component { value = { inside; _ }; region } ->
      let state = state#pad 2 1 in
      print_loc_node state "<component>" region;
      print_expr (state#pad 1 0) inside)
  | EModA { value; region } ->
    print_loc_node state "EModA" region;
    print_module_access print_expr state value
  | EAnnot { value; region } ->
    print_loc_node state "EAnnot" region;
    print_annotated state value
  | EUnit { region; _ } -> print_loc_node state "EUnit" region
  | ECodeInj { value; region } ->
    print_loc_node state "ECodeInj" region;
    print_code_inj state value
  | ETernary { value; region } ->
    print_loc_node state "ETernary" region;
    print_ternary state value


and print_constr_expr state (node : (constr * expr option) reg) =
  let constr, expr_opt = node.value in
  print_loc_node state "EConstr" node.region;
  match expr_opt with
  | None -> print_ident (state#pad 1 0) constr
  | Some expr ->
    print_ident (state#pad 2 0) constr;
    print_expr (state#pad 2 1) expr


and print_array_item state = function
  | Expr_entry e ->
    print_node state "<expr>";
    print_expr (state#pad 1 0) e
  | Rest_entry { value; region } ->
    print_loc_node state "<rest>" region;
    print_expr (state#pad 1 0) value.expr


and print_property state = function
  | Punned_property { value; region } ->
    print_loc_node state "<punned property>" region;
    print_expr state value
  | Property { value = { name; value; _ }; region } ->
    print_loc_node state "<property>" region;
    print_expr (state#pad 2 0) name;
    print_expr (state#pad 2 1) value
  | Property_rest { value = { expr; _ }; region } ->
    print_loc_node state "<property rest>" region;
    print_expr state expr


and print_fun_expr state node =
  let { parameters; lhs_type; body; _ } = node in
  let fields = if Option.is_none lhs_type then 2 else 3 in
  let () =
    let state = state#pad fields 0 in
    print_node state "<parameters>";
    print_expr (state#pad 1 0) parameters
  in
  let () =
    match lhs_type with
    | None -> ()
    | Some (_, type_expr) ->
      let state = state#pad fields 1 in
      print_node state "<lhs type>";
      print_type_expr (state#pad 1 0) type_expr
  in
  let () =
    let state = state#pad fields (fields - 1) in
    match body with
    | FunctionBody { value = { inside; _ }; region } ->
      let statements = Utils.nsepseq_to_list inside in
      let apply len rank = print_statement (state#pad len rank) in
      print_loc_node state "<function_body>" region;
      List.iteri ~f:(List.length statements |> apply) statements
    | ExpressionBody e_body ->
      print_node state "<expression body>";
      print_expr (state#pad 1 0) e_body
  in
  ()


and print_code_inj state rc =
  let () =
    let state = state#pad 2 0 in
    print_node state "<language>";
    print_string (state#pad 1 0) rc.language
  in
  let () =
    let state = state#pad 2 1 in
    print_node state "<code>";
    print_expr (state#pad 1 0) rc.code
  in
  ()


and print_tuple_expr state (node : (expr, comma) Utils.nsepseq reg) =
  let exprs = Utils.nsepseq_to_list node.value in
  let length = List.length exprs in
  let apply len rank = print_expr (state#pad len rank) in
  List.iteri ~f:(apply length) exprs


and print_arguments state = function
  | Multiple x ->
    let ({ inside; _ } : (expr, comma) Utils.nsepseq par) = x.value in
    print_tuple_expr state { value = inside; region = x.region }
  | Unit x -> print_loc_node state "Unit" x.region


and print_fun_call state (fun_expr, args) =
  let arity =
    match args with
    | Unit _ -> 0
    | Multiple xs -> List.length (Utils.nsepseq_to_list xs.value.inside)
  in
  print_expr (state#pad (1 + arity) 0) fun_expr;
  print_arguments state args


and print_string_expr state = function
  | String s ->
    print_node state "String";
    print_string (state#pad 1 0) s
  | Verbatim v ->
    print_node state "Verbatim";
    print_string (state#pad 1 0) v


and print_arith_expr state = function
  | Add { value; region } -> print_bin_op "Add" region state value
  | Sub { value; region } -> print_bin_op "Sub" region state value
  | Mult { value; region } -> print_bin_op "Mult" region state value
  | Div { value; region } -> print_bin_op "Div" region state value
  | Mod { value; region } -> print_bin_op "Mod" region state value
  | Neg { value; region } ->
    print_loc_node state "Neg" region;
    print_expr (state#pad 1 0) value.arg
  | Int i ->
    print_node state "Int";
    print_int state i


and print_e_logic state = function
  | BoolExpr e ->
    print_node state "BoolExpr";
    print_bool_expr (state#pad 1 0) e
  | CompExpr e ->
    print_node state "CompExpr";
    print_comp_expr (state#pad 1 0) e


and print_bool_expr state = function
  | Or { value; region } -> print_bin_op "Or" region state value
  | And { value; region } -> print_bin_op "And" region state value
  | Not { value; _ } ->
    print_node state "Not";
    print_expr (state#pad 1 0) value.arg


and print_comp_expr state = function
  | Lt { value; region } -> print_bin_op "Lt" region state value
  | Leq { value; region } -> print_bin_op "Leq" region state value
  | Gt { value; region } -> print_bin_op "Gt" region state value
  | Geq { value; region } -> print_bin_op "Geq" region state value
  | Equal { value; region } -> print_bin_op "Equal" region state value
  | Neq { value; region } -> print_bin_op "Neq" region state value


and print_bin_op node region state op =
  print_loc_node state node region;
  print_expr (state#pad 2 0) op.arg1;
  print_expr (state#pad 2 1) op.arg2


and print_annotated state annot =
  let expr, _, t_expr = annot in
  print_expr (state#pad 2 0) expr;
  print_type_expr (state#pad 2 1) t_expr


and print_ternary state ternary =
  let state = state#pad 3 0 in
  print_node state "<condition>";
  print_expr (state#pad 1 0) ternary.condition;
  let state = state#pad 3 1 in
  print_node state "<truthy>";
  print_expr (state#pad 1 0) ternary.truthy;
  let state = state#pad 3 2 in
  print_node state "<falsy>";
  print_expr (state#pad 1 0) @@ ternary.falsy


and print_cond_statement state (cond : cond_statement) =
  let () =
    let state = state#pad 3 0 in
    print_node state "<condition>";
    print_expr (state#pad 1 0) cond.test.inside
  in
  let () =
    let state = state#pad 3 1 in
    print_node state "<true>";
    print_statement (state#pad 1 0) cond.ifso
  in
  let () =
    match cond.ifnot with
    | Some ifnot ->
      let state = state#pad 3 2 in
      print_node state "<false>";
      print_statement (state#pad 1 0) @@ snd ifnot
    | None -> ()
  in
  ()


and print_type_expr state = function
  | TProd { inside; attributes } ->
    let { value; region } = inside in
    print_attributes state attributes;
    print_loc_node state "TProd" region;
    print_cartesian state value
  | TSum { value; region } ->
    print_loc_node state "TSum" region;
    print_sum_type state value
  | TObject { value; region } ->
    print_loc_node state "TObject" region;
    print_ne_injection print_field_decl state value
  | TApp { value = name, tuple; region } ->
    print_loc_node state "TApp" region;
    print_ident (state#pad 1 0) name;
    print_type_tuple (state#pad 2 1) tuple
  | TFun { value; region } ->
    print_loc_node state "TFun" region;
    let state = state#pad 0 1 in
    let apply len rank = print_type_expr (state#pad len rank) in
    let args, _, range = value in
    print_fun_type_args state args;
    print_loc_node state "<result>" region;
    List.iteri ~f:(apply 2) [ range ]
  | TPar { value = { inside; _ }; region } ->
    print_loc_node state "TPar" region;
    print_type_expr (state#pad 1 0) inside
  | TVar v ->
    print_node state "TVar";
    print_ident (state#pad 1 0) v
  | TString s ->
    print_node state "TString";
    print_string (state#pad 1 0) s
  | TModA { value; region } ->
    print_loc_node state "TModA" region;
    print_module_access print_type_expr state value
  | TInt s ->
    print_node state "TInt";
    print_int (state#pad 1 0) s
  | TDisc u ->
    print_node state "TDisc";
    let objects = Utils.nsepseq_to_list u in
    let apply len rank (v : field_decl reg ne_injection reg) =
      print_ne_injection print_field_decl (state#pad len rank) v.value
    in
    List.iteri ~f:(List.length objects |> apply) objects


and print_module_access : type a. (state -> a -> unit) -> state -> a module_access -> unit
  =
 fun f state ma ->
  print_ident (state#pad 2 0) ma.module_name;
  f (state#pad 2 1) ma.field


and print_fun_type_arg state (node : fun_type_arg) =
  let ({ name; type_expr; _ } : fun_type_arg) = node in
  print_ident state name;
  let state = state#pad 1 0 in
  print_type_expr state type_expr


and print_fun_type_args state { inside; _ } =
  print_node state "<parameters>";
  let fun_type_args = Utils.nsepseq_to_list inside in
  let apply len rank = print_fun_type_arg (state#pad len rank) in
  List.iteri ~f:(List.length fun_type_args |> apply) fun_type_args


and print_sum_type state { variants; attributes; _ } =
  let variants = Utils.nsepseq_to_list variants.value in
  let arity = List.length variants in
  let arity = if List.is_empty attributes then arity else arity + 1 in
  let apply arity rank variant =
    let state = state#pad arity rank in
    print_variant state variant
  in
  let () = List.iteri ~f:(apply arity) variants in
  if not (List.is_empty attributes)
  then (
    let state = state#pad arity (arity - 1) in
    print_attributes state attributes)


and print_variant state (node : variant reg) =
  let { attributes; tuple; _ } = node.value in
  let arity = if List.is_empty attributes then 0 else 1 in
  let { constr; params } = tuple.value.inside in
  let params =
    match params with
    | None -> []
    | Some (_, seq) -> Utils.nsepseq_to_list seq
  in
  let arity = if List.is_empty params then arity else arity + 1 in
  let rank = 0 in
  let () = print_ident state constr in
  let rank =
    match params with
    | [] -> rank
    | components ->
      let apply len rank = print_type_expr (state#pad len rank) in
      List.iteri ~f:(List.length components |> apply) components;
      rank + 1
  in
  let () =
    if not (List.is_empty attributes)
    then print_attributes (state#pad arity rank) attributes
  in
  ()


and print_type_tuple state { value; _ } =
  let components = Utils.nsepseq_to_list value.inside in
  let apply len rank = print_type_expr (state#pad len rank) in
  List.iteri ~f:(List.length components |> apply) components


and print_attributes state attributes =
  print_node state "<attributes>";
  let length = List.length attributes in
  let apply len rank = print_ident (state#pad len rank) in
  List.iteri ~f:(apply length) attributes


and print_field_decl state { value; _ } =
  let arity = if List.is_empty value.attributes then 1 else 2 in
  print_ident state value.field_name;
  print_type_expr (state#pad arity 0) value.field_type;
  if not (List.is_empty value.attributes)
  then print_attributes (state#pad arity 1) value.attributes


and print_cartesian state { inside; _ } =
  let t_exprs = Utils.nsepseq_to_list inside in
  let arity = List.length t_exprs in
  let apply len rank = print_type_expr (state#pad len rank) in
  List.iteri ~f:(apply arity) t_exprs


(* PRINTING (client-slide) *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

let print_to_buffer state cst =
  print_cst state cst;
  state#buffer


let print_to_string state cst = Buffer.contents (print_to_buffer state cst)

let print_pattern_to_string state pattern =
  print_pattern state pattern;
  Buffer.contents state#buffer


(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
let pattern_to_string = print_pattern_to_string
