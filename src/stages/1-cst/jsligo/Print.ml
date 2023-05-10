(* PRINTING THE CST *)

[@@@coverage exclude_file]

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Internal dependencies *)

module Attr = Lexing_shared.Attr
module Tree = Cst_shared.Tree

open CST (* THE ONLY GLOBAL OPENING *)

(* UTILITIES *)

type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq
type ('a, 'sep)  sepseq = ('a, 'sep) Utils.sepseq

let print_attribute state (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  match val_opt with
    None ->
      Tree.(make_unary state "<attribute>" make_node key)
  | Some String value ->
      let children = Tree.[
        mk_child make_node key;
        mk_child make_node (Printf.sprintf "%S" value)]
      in Tree.make state "<attribute>" children
  | Some Ident value ->
      let children = Tree.[
        mk_child make_node key;
        mk_child make_node value]
      in Tree.make state "<attribute>" children

let mk_children_attr (node : Attr.t wrap list) =
  Tree.mk_children_list print_attribute ~root:"<attributes>" node

(* Preprocessing directives *)

let print_Directive state (node : Directive.t) =
  let region, string = Directive.project node in
  Tree.make_unary state "Directive" Tree.make_node ~region string

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  Tree.of_nseq state "<cst>" print_toplevel_statement node.statements

(* TOP-LEVEL *)

and print_toplevel_statement state = function
  TopLevel  s -> print_TopLevel  state s
| Directive s -> print_Directive state s

and print_TopLevel state (stmt, _) = print_statement state stmt

(* STATEMENTS *)

and print_statement state = function
  SBlock     s -> print_SBlock     state s
| SExpr      s -> print_SExpr      state s
| SCond      s -> print_SCond      state s
| SReturn    s -> print_SReturn    state s
| SLet       s -> print_SLet       state s
| SConst     s -> print_SConst     state s
| SType      s -> print_SType      state s
| SSwitch    s -> print_SSwitch    state s
| SBreak     s -> print_SBreak     state s
| SNamespace s -> print_SNamespace state s
| SExport    s -> print_SExport    state s
| SImport    s -> print_SImport    state s
| SForOf     s -> print_SForOf     state s
| SWhile     s -> print_SWhile     state s

(* Blocks *)

and print_SBlock state (node: (statement, semi) nsepseq braces reg) =
  let Region.{region; value} = node in
  Tree.of_nsepseq state ~region "SBlock" print_statement value.inside

(* Expression as a statement *)

and print_SExpr state (node: attributes * expr) =
  let attributes, expr = node in
  let children = mk_children_attr attributes
               @ [Tree.mk_child print_expr expr]
  in Tree.make state "SExpr" children

(* Conditional statement *)

and print_SCond state (node: cond_statement reg) =
  let Region.{value; region} = node in

  let print_ifso state (node: statement) =
    Tree.make_unary state "<true>" print_statement node

  and print_ifnot state (node: kwd_else * statement) =
    Tree.make_unary state "<false>" print_statement (snd node) in

  let children = Tree.[
    mk_child     print_expr  value.test.inside;
    mk_child     print_ifso  value.ifso;
    mk_child_opt print_ifnot value.ifnot]
  in Tree.make ~region state "SCond" children

(* Return statement *)

and print_SReturn state (node: return reg) =
  let Region.{value; region} = node in
  let children = Tree.[mk_child_opt print_expr value.expr]
  in Tree.make ~region state "SReturn" children

(* Value declaration *)

and print_SLet state (node: let_decl reg) =
  let Region.{region; value} = node in
  let children = mk_children_bindings value.bindings
               @ mk_children_attr     value.attributes
  in Tree.make state ~region "SLet" children

and mk_children_bindings (node: (val_binding reg, comma) nsepseq) =
  Tree.mk_children_nsepseq print_val_binding node

and print_val_binding state (node: val_binding reg) =
  let Region.{region; value} = node in

  let print_binders state (node: pattern) =
    Tree.make_unary state "<binders>" print_pattern node

  and print_type_params state (node: type_generics) =
    let Region.{region; value} = node in
    let seq = value.inside in
    Tree.(of_nsepseq state ~region "<parameters>" make_literal seq)

  and print_rhs state (node: expr) =
    Tree.make_unary state "<rhs>" print_expr node in

  let children = Tree.[
    mk_child     print_binders         value.binders;
    mk_child_opt print_type_params     value.type_params;
    mk_child_opt print_type_annotation value.lhs_type;
    mk_child     print_rhs             value.expr]
  in Tree.make state ~region "<binding>" children

and print_type_annotation state (_, type_expr) =
  Tree.make_unary state "<type>" print_type_expr type_expr

(* Constant declaration *)

and print_SConst state (node: const_decl reg) =
  let Region.{region; value} = node in
  let children = mk_children_bindings value.bindings
               @ mk_children_attr     value.attributes
  in Tree.make state ~region "SConst" children

(* Type declaration *)

and print_SType state (node: type_decl reg) =
  let Region.{value; region} = node in

  let print_type_vars state (node: (type_var, comma) nsepseq chevrons reg) =
    let Region.{region; value} = node in
    let seq = value.inside in
    Tree.(of_nsepseq state ~region "<parameters>" make_literal seq) in

  let children = Tree.[
    mk_child     make_literal    value.name;
    mk_child_opt print_type_vars value.params;
    mk_child     print_type_expr value.type_expr]
  @ mk_children_attr value.attributes
  in Tree.make state ~region "SType" children

(* Switch statement *)

and print_SSwitch state (node: switch reg) =
  let Region.{value; region} = node in

  let print_subject_expr state (node: expr) =
    Tree.make_unary state "<subject>" print_expr node in

  let children = Tree.(mk_child print_subject_expr value.expr
                       :: mk_children_nseq print_case value.cases)
  in Tree.make state ~region "SSwitch" children

and print_case state = function
  Switch_case         c -> print_Switch_case         state c
| Switch_default_case c -> print_Switch_default_case state c

and print_Switch_case state (node: switch_case) =
  let children = Tree.[
    mk_child     print_expr       node.expr;
    mk_child_opt print_statements node.statements]
  in Tree.make state "Switch_case" children

and print_statements state (node: statements) =
  Tree.of_nsepseq state "<statements>" print_statement node

and print_Switch_default_case state (node: switch_default_case) =
  Tree.of_sepseq state "Switch_default_case" print_statement node.statements

and print_SBreak state (node: kwd_break) =
  Tree.make_node ~region:node#region state "SBreak"

(* Namespaces *)

and print_SNamespace state (node: namespace_statement reg) =
  let Region.{value; region} = node in
  let _, module_name, statements, attributes = value in
  let statements = statements.value.inside in
  let children = Tree.[
    mk_child print_namespace  module_name;
    mk_child print_statements statements]
  @ mk_children_attr attributes
  in Tree.make ~region state "SNamespace" children

and print_namespace state (node: module_name) =
  Tree.(make_unary state "<namespace>" make_literal node)

(* Export statements *)

and print_SExport state (node: (kwd_export * statement) reg) =
  let Region.{value; region} = node in
  Tree.make_unary ~region state "SExport" print_statement (snd value)

(* Import statements *)

and print_SImport state (node: import reg) =
  let Region.{value; region} = node in
  Tree.make_unary ~region state "SImport" print_import value

and print_import state = function
  Import_rename   i -> print_Import_rename   state i
| Import_all_as   i -> print_Import_all_as   state i
| Import_selected i -> print_Import_selected state i

and print_Import_rename state (node: import_rename) =
  let children = Tree.[
    mk_child make_literal      node.kwd_import;
    mk_child print_alias       node.alias;
    mk_child print_module_path node.module_path]
  in Tree.make state "Import_rename" children

and print_alias state (node: module_name) =
  Tree.(make_unary state "<alias>" make_literal node)

and print_module_path state (node: (module_name, dot) nsepseq) =
  Tree.(of_nsepseq state "<module path>" make_literal node)

and print_Import_all_as state (node: import_all_as) =
  let children = Tree.[
    mk_child print_alias    node.alias;
    mk_child print_dir_path node.module_path]
  in Tree.make state "Import_all_as" children

and print_dir_path state (node: string wrap) =
  Tree.(make_unary state "<module path>" make_literal node)

and print_Import_selected state (node: import_selected) =
  let children = Tree.[
    mk_child print_imported node.imported;
    mk_child print_dir_path node.module_path]
  in Tree.make state "Import_selected" children

and print_imported state (node: (field_name, comma) nsepseq braces reg) =
  let Region.{region; value} = node in
  Tree.(of_nsepseq state ~region "<module path>"
          make_literal value.inside)

(* For-loops *)

and print_SForOf state (node: for_of reg) =
  let Region.{value; region} = node in

  let print_index_kind state (node: index_kind) =
    match node with
      `Let   kwd_let   -> Tree.make_literal state kwd_let
    | `Const kwd_const -> Tree.make_literal state kwd_const in

  let children = Tree.[
    mk_child print_index_kind value.index_kind;
    mk_child make_literal     value.index;
    mk_child print_expr       value.expr;
    mk_child print_statement  value.statement]
  in Tree.make state ~region "SForOf" children

(* While-loops *)

and print_SWhile state (node: while_stmt reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr      value.expr;
    mk_child print_statement value.statement]
  in Tree.make state ~region "SWhile" children

(* PATTERNS *)

and print_pattern state = function
  PRest     p -> print_PRest     state p
| PAssign   p -> print_PAssign   state p
| PVar      p -> print_PVar      state p
| PConstr   p -> print_PConstr   state p
| PDestruct p -> print_PDestruct state p
| PObject   p -> print_PObject   state p
| PArray    p -> print_PArray    state p

(* Rest pattern *)

and print_PRest state (node: rest_pattern reg) =
  Tree.(make_unary state "PRest" make_literal node.value.rest)

(* Assignment pattern *)

and print_PAssign state (node: assign_pattern reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child make_literal value.property;
    mk_child print_expr   value.value]
  in Tree.make state ~region "PAssign" children

(* Pattern variable *)

and print_PVar state (node: var_pattern reg) =
  let Region.{region; value} = node in
  let children =
    Tree.(mk_child make_literal value.variable)
  :: mk_children_attr value.attributes
  in Tree.make state ~region "PVar" children

(* Data constructor as a pattern *)

and print_PConstr state (node: variable) =
  Tree.(make_unary state "PConstr" make_literal node)

(* Destructuring pattern *)

and print_PDestruct state (node: destruct reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child make_literal      value.property;
    mk_child print_val_binding value.target]
  in Tree.make state ~region "PDestruct" children

and print_PObject state (node: object_pattern) =
  let Region.{region; value} = node in
  Tree.of_nsepseq state ~region "PObject" print_pattern value.inside

and print_PArray state (node: array_pattern) =
  let Region.{region; value} = node in
  Tree.of_nsepseq state ~region "PArray" print_pattern value.inside

(* TYPES *)

and print_type_expr state = function
  TProd      t -> print_TProd      state t
| TSum       t -> print_TSum       state t
| TObject    t -> print_TObject    state t
| TApp       t -> print_TApp       state t
| TFun       t -> print_TFun       state t
| TPar       t -> print_TPar       state t
| TVar       t -> print_TVar       state t
| TString    t -> print_TString    state t
| TModA      t -> print_TModA      state t
| TInt       t -> print_TInt       state t
| TDisc      t -> print_TDisc      state t
| TParameter t -> print_TParameter state t

(* Product types *)

and print_TProd state (node: cartesian) =
  let Region.{region; value} = node.inside in
  let children =
    Tree.mk_children_nsepseq print_type_expr value.inside
    @ mk_children_attr node.attributes
  in Tree.make state ~region "TProd" children

(* Variant types *)

and print_TSum state (node: sum_type reg) =
  let Region.{region; value} = node in
  let children =
    Tree.mk_children_nsepseq print_variant value.variants.value
  @ mk_children_attr value.attributes
  in Tree.make state ~region "TSum" children

and print_variant state (node: variant reg) =
  let children_attr = mk_children_attr node.value.attributes in
  let components = node.value.tuple.value.inside in
  let ctor = components.constr
  and args = components.params in
  let mk_tree = Tree.make state ~region:ctor#region ctor#payload in
  match args with
    None -> mk_tree children_attr
  | Some (_, args) ->
      let children =
        Tree.mk_children_nsepseq print_type_expr args @ children_attr
      in mk_tree children

(* Object types *)

and print_TObject state (node: obj_type) =
  let Region.{value; region} = node in
  let children =
    Tree.mk_children_nsepseq print_field_decl value.ne_elements
    @ mk_children_attr value.attributes
  in Tree.make state ~region "TObject" children

and print_field_decl state (node: field_decl reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child make_literal    value.field_name;
    mk_child print_type_expr value.field_type]
  @ mk_children_attr value.attributes
  in Tree.make state ~region "<field>" children

(* Application of type constructors *)

and print_TApp state (node: (type_constr * type_params) reg) =
  let Region.{region; value} = node in
  let ctor, params = value in
  let args = Tree.mk_children_nsepseq ~root:"<arguments>"
               print_type_expr params.value.inside in
  let children = Tree.(mk_child make_literal ctor :: args)
  in Tree.make state ~region "TApp" children

(* Functional type *)

and print_TFun state (node: (fun_type_args * arrow * type_expr) reg) =
  let Region.{value; region} = node in
  let domain, _, codomain = value in

  let print_fun_type_arg state (node: fun_type_arg) =
    let children = Tree.[
        mk_child make_literal    node.name;
        mk_child print_type_expr node.type_expr]
    in Tree.make state "<parameter>" children in

  let print_fun_type_args state (node: (fun_type_arg, comma) nsepseq par) =
    Tree.of_nsepseq state "<parameters>" print_fun_type_arg node.inside

  and print_codomain state (node: type_expr) =
    Tree.make_unary state "<result>" print_type_expr node in

  let children = Tree.[
    mk_child print_fun_type_args domain;
    mk_child print_codomain      codomain]
  in Tree.make state ~region "TFun" children

(* Parenthesised type *)

and print_TPar state (node: type_expr par reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "TPar" print_type_expr value.inside

(* Type variable *)

and print_TVar state (node: variable) =
  Tree.(make_unary state "TVar" make_literal node)

(* Type string *)

and print_TString state (node: lexeme wrap) =
  Tree.(make_unary state "TString" make_string node)

(* Type from module path *)

and print_TModA state (node: type_expr module_access reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child make_literal    value.module_name;
    mk_child print_type_expr value.field]
  in Tree.make state ~region "TModA" children

(* Integer type *)

and print_TInt state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "TInt" state node

(* Discriminated unions *)

and print_TDisc state (node: (obj_type, vbar) nsepseq) =
  Tree.of_nsepseq state "TDisc" print_obj_type node

and print_obj_type state (node: obj_type) =
  let Region.{value; region} = node in
  let children =
    Tree.mk_children_nsepseq print_field_decl value.ne_elements
    @ mk_children_attr value.attributes
  in Tree.make state ~region "<object type>" children

(* Parameter of *)

and print_TParameter state (node: (module_name, dot) nsepseq reg) =
  let Region.{value; region} = node in
  Tree.(of_nsepseq state ~region "TParameter" make_literal value)

(* EXPRESSIONS *)

and print_expr state = function
  EAnnot    e -> print_EAnnot    state e
| EArith    e -> print_EArith    state e
| EArray    e -> print_EArray    state e
| EAssign   e -> print_EAssign   state e
| EBytes    e -> print_EBytes    state e
| ECall     e -> print_ECall     state e
| ECodeInj  e -> print_ECodeInj  state e
| EConstr   e -> print_EConstr   state e
| EContract e -> print_EContract state e
| EFun      e -> print_EFun      state e
| ELogic    e -> print_ELogic    state e
| EModA     e -> print_EModA     state e
| EObject   e -> print_EObject   state e
| EPar      e -> print_EPar      state e
| EProj     e -> print_EProj     state e
| ESeq      e -> print_ESeq      state e
| EString   e -> print_EString   state e
| ETernary  e -> print_ETernary  state e
| EUnit     e -> print_EUnit     state e
| EVar      e -> print_EVar      state e

(* Functional expressions *)

and print_EFun state (node: fun_expr reg) =
  let Region.{value; region} = node in

  let print_fun_param state (node: expr) =
    Tree.make_unary state "<parameters>" print_expr node

  and print_FunctionBody state (node: statements braces reg) =
    let Region.{region; value} = node in
    Tree.of_nsepseq state ~region "FunctionBody" print_statement value.inside

  and print_ExpressionBody state (node: expr) =
    Tree.make_unary state "ExpressionBody" print_expr node in

  let print_body state = function
    FunctionBody   e -> print_FunctionBody   state e
  | ExpressionBody e -> print_ExpressionBody state e in

  let children = Tree.[
    mk_child     print_fun_param       value.parameters;
    mk_child_opt print_type_annotation value.lhs_type;
    mk_child     print_body            value.body]
  in Tree.make state ~region "EFun" children

(* Parenthesised expressions *)

and print_EPar state (node: expr par reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "EPar" print_expr value.inside

(* Sequence expressions *)

and print_ESeq state (node: (expr, comma) nsepseq reg) =
  let Region.{value; region} = node in
  Tree.of_nsepseq state ~region "ESeq" print_expr value

(* Value variables *)

and print_EVar state (node: variable) =
  Tree.(make_unary state "EVar" make_literal node)

(* Qualified values (through a module path) *)

and print_EModA state (node: expr module_access reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child make_literal value.module_name;
    mk_child print_expr   value.field]
  in Tree.make state ~region "EModA" children

(* Boolean expressions *)

and print_ELogic state = function
  BoolExpr e -> print_BoolExpr state e
| CompExpr e -> print_CompExpr state e

(* Boolean operators *)

and print_BoolExpr state = function
  Or  e -> print_Or  state e
| And e -> print_And state e
| Not e -> print_Not state e

and print_Or state (node: bool_or bin_op reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in Tree.make state ~region "Or" children

and print_And state (node: bool_and bin_op reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in Tree.make state ~region "And" children

and print_Not state (node: negate un_op reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "Not" print_expr value.arg

(* Boolean comparisons *)

and print_CompExpr state = function
  Lt    e -> print_Lt    state e
| Leq   e -> print_Leq   state e
| Gt    e -> print_Gt    state e
| Geq   e -> print_Geq   state e
| Equal e -> print_Equal state e
| Neq   e -> print_Neq   state e

and print_bin_op :
  type a.Tree.root -> Tree.state -> a bin_op reg -> unit =
  fun root state node ->
    let Region.{region; value} = node in
    let children = Tree.[
      mk_child print_expr value.arg1;
      mk_child print_expr value.arg2]
    in Tree.make state ~region root children

and print_Lt state (node: lt bin_op reg) =
  print_bin_op "Lt" state node

and print_Leq state (node: leq bin_op reg) =
  print_bin_op "Leq" state node

and print_Gt state (node: gt bin_op reg) =
  print_bin_op "Gt" state node

and print_Geq state (node: geq bin_op reg) =
  print_bin_op "Geq" state node

and print_Equal state (node: equal_cmp bin_op reg) =
  print_bin_op "Equal" state node

and print_Neq state (node: neq bin_op reg) =
  print_bin_op "Neq" state node

(* Arithmetic expressions *)

and print_EArith state = function
  Add  e -> print_Add  state e
| Sub  e -> print_Sub  state e
| Mult e -> print_Mult state e
| Div  e -> print_Div  state e
| Mod  e -> print_Mod  state e
| Neg  e -> print_Neg  state e
| Int  e -> print_Int  state e

and print_Add state (node: plus bin_op reg) =
  print_bin_op "Add" state node

and print_Sub state (node: minus bin_op reg) =
  print_bin_op "Sub" state node

and print_Mult state (node: times bin_op reg) =
  print_bin_op "Mult" state node

and print_Div state (node: slash bin_op reg) =
  print_bin_op "Div" state node

and print_Mod state (node: modulo bin_op reg) =
  print_bin_op "Mod" state node

and print_Neg state (node: minus un_op reg) =
  print_un_op "Neg" state node

and print_un_op :
  type a.Tree.root -> Tree.state -> a un_op reg -> unit =
  fun root state node ->
    let Region.{region; value} = node in
    let children = Tree.[mk_child print_expr value.arg]
    in Tree.make state ~region root children

and print_Int state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "Int" state node

(* Function calls *)

and print_ECall state (node: (expr * arguments) reg) =
  let Region.{value; region} = node in
  let func, args = value in

  let print_Multiple state (node: (expr, comma) nsepseq par reg) =
    let Region.{region; value} = node in
    Tree.of_nsepseq state ~region "Multiple" print_expr value.inside

  and print_Unit state (node: the_unit reg) =
    Tree.make_node state ~region:node.region "Unit" in

  let print_arguments state = function
      Multiple e -> print_Multiple state e
    | Unit     e -> print_Unit     state e in

  let children = Tree.[
    mk_child print_expr      func;
    mk_child print_arguments args]
  in Tree.make state ~region "ECall" children

(* Byte values *)

and print_EBytes state (node: (lexeme * Hex.t) wrap) =
  Tree.make_bytes "EBytes" state node

(* Arrays *)

and print_EArray state (node: (array_item, comma) sepseq brackets reg) =
  let Region.{region; value} = node in
  Tree.of_sepseq state ~region "EArray" print_array_item value.inside

and print_array_item state = function
  Expr_entry e -> print_Expr_entry state e
| Rest_entry e -> print_Rest_entry state e

and print_Expr_entry state (node: expr) =
  Tree.make_unary state "Expr_entry" print_expr node

and print_Rest_entry state (node: array_item_rest reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "Rest_entry" print_expr value.expr

(* Object expressions *)

and print_EObject state (node: object_expr) =
  let Region.{region; value} = node in
  Tree.of_nsepseq state ~region "EObject" print_property value.inside

and print_property state = function
  Punned_property e -> print_Punned_property state e
| Property        e -> print_Property        state e
| Property_rest   e -> print_Property_rest   state e

and print_Punned_property state (node: expr reg) =
  let Region.{value; region} = node in
  Tree.make_unary state ~region "Punned_property" print_expr value

and print_Property state (node: property2 reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child print_expr value.name;
    mk_child print_expr value.value]
  in Tree.make state ~region "Property" children

and print_Property_rest state (node: property_rest reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "Property_rest" print_expr value.expr

(* String expressions *)

and print_EString state = function
  String   e -> Tree.(make_unary state "String" make_string e)
| Verbatim e -> Tree.(make_unary state "Verbatim" make_verbatim e)

(* Projection *)

and print_EProj state (node: projection reg) =
  let Region.{region; value} = node in

  let print_proj_field state (node: expr) =
    Tree.make_unary state "<field name>"print_expr node

  and print_FieldName state (node: selection_field_name reg) =
    let Region.{region; value} = node in
    Tree.(make_unary state ~region "FieldName" make_literal value.value)

  and print_Component state (node: expr brackets reg) =
    let Region.{region; value} = node in
    Tree.make_unary state ~region "Component" print_expr value.inside in

  let print_proj_select state = function
    FieldName e -> print_FieldName state e
  | Component e -> print_Component state e in

  let children = Tree.[
    mk_child print_proj_field  value.expr;
    mk_child print_proj_select value.selection]
  in Tree.make state ~region "EProj" children

(* Assignment expression *)

and print_EAssign state (node: expr * operator reg * expr) =
  let arg1, op, arg2 = node in
  let op_txt : string =
    match op.value with
      Eq                           -> "Eq"
    | Assignment_operator Times_eq -> "Times_eq"
    | Assignment_operator Div_eq   -> "Div_eq"
    | Assignment_operator Min_eq   -> "Min_eq"
    | Assignment_operator Plus_eq  -> "Plus_eq"
    | Assignment_operator Mod_eq   -> "Mod_eq" in
  let children = Tree.[
    mk_child make_node  op_txt;
    mk_child print_expr arg1;
    mk_child print_expr arg2]
  in Tree.make state "EAssign" children

(* Constructor applications *)

and print_EConstr state (node: (constr * expr option) reg) =
  let Region.{region; value} = node in
  let ctor, arg_opt = value in
  let children = Tree.[
    mk_child     make_literal ctor;
    mk_child_opt print_expr   arg_opt]
  in Tree.make state ~region "EConstr" children

(* Contract *)

and print_EContract state (node: (module_name, dot) nsepseq reg) =
  let Region.{region; value} = node in
  Tree.(of_nsepseq state ~region "EContract" make_literal value)

(* Annotated expressions *)

and print_EAnnot state (node: annot_expr reg) =
  let Region.{value; region} = node in
  let expr, _, type_expr = value in
  let children = Tree.[
    mk_child print_expr expr;
    mk_child print_type_expr type_expr]
  in Tree.make state ~region "EAnnot" children

(* Unit value *)

and print_EUnit state (node: the_unit reg) =
  Tree.make_node state ~region:node.region "EUnit"

(* Code injection *)

and print_ECodeInj state (node: code_inj reg) =
  let Region.{value; region} = node in
  Tree.make_unary state ~region "ECodeInj" print_expr value.code

(* Ternary conditional operator *)

and print_ETernary state (node: ternary reg) =
  let Region.{value; region} = node in

  let print_truthy state (node: expr) =
    Tree.make_unary state "<true>" print_expr node

  and print_falsy state (node: expr) =
    Tree.make_unary state "<false>" print_expr node in

  let children = Tree.[
    mk_child print_expr   value.condition;
    mk_child print_truthy value.truthy;
    mk_child print_falsy  value.falsy]
  in Tree.make state ~region "ETernary" children

(* PRINTING (client-slide) *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

let print_to_buffer state cst =
  print_cst state cst; Tree.to_buffer state

let print_to_string state cst =
  Buffer.contents (print_to_buffer state cst)

let print_pattern_to_string state pattern =
  print_pattern state pattern;
  Buffer.contents (Tree.to_buffer state)

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
let pattern_to_string = print_pattern_to_string
