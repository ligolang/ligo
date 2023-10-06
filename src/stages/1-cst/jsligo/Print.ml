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

let (<@) = Utils.(<@)

type ('a,'sep) sep_or_term = ('a,'sep) Utils.sep_or_term

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

(* Preprocessing directives as statements *)

let print_S_Directive state (node : Directive.t) =
  let region, string = Directive.project node in
  Tree.(make_unary state "S_Directive" make_node ~region string)

(* Printing variables *)

let print_variable state = function
  Var node -> Tree.make_literal state node
| Esc node ->
    Tree.make_node ~region:node#region state ("@" ^ node#payload)

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  Tree.of_nseq state "<cst>" (fun state -> print_statement state <@ fst)
               node.statements

(* DECLARATIONS *)

and print_declaration state = function
  D_Fun       d -> print_D_Fun       state d
| D_Import    d -> print_D_Import    state d
| D_Interface d -> print_D_Interface state d
| D_Namespace d -> print_D_Namespace state d
| D_Type      d -> print_D_Type      state d
| D_Value     d -> print_D_Value     state d

(* Function declaration *)

and print_D_Fun state (node: fun_decl reg) =
  let Region.{value; region} = node in
  let {kwd_function=_; fun_name; generics; parameters;
       rhs_type; fun_body} = value in
  let children = Tree.[
    mk_child     print_variable      fun_name;
    mk_child_opt print_generics      generics;
    mk_child     print_fun_params    parameters;
    mk_child_opt print_rhs_type      rhs_type;
    mk_child     print_decl_fun_body fun_body]
  in Tree.make ~region state "D_Fun" children

and print_fun_params state (node: fun_params) =
  let Region.{value; region} = node in
  Tree.of_sep_or_term ~region state "<parameters>" print_pattern value.inside

and print_decl_fun_body state (node : statements braces) =
  Tree.of_nseq state "<body>"
    (fun state -> print_statement state <@ fst) node.value.inside

(* Import declaration *)

and print_D_Import state (node: import_decl) =
  Tree.make_unary state "D_Import" print_import_decl node

and print_import_decl state = function
  ImportAlias d -> print_ImportAlias state d
| ImportAllAs d -> print_ImportAllAs state d
| ImportFrom  d -> print_ImportFrom  state d

and print_ImportAlias state (node: import_alias reg) =
  let Region.{value; region} = node in
  let {kwd_import=_; alias; equal=_; namespace_path} = value in
  let children = Tree.[
    mk_child print_alias               alias;
    mk_child print_namespace_selection namespace_path]
  in Tree.make ~region state "ImportAlias" children

and print_alias state (node: namespace_name) =
  Tree.(make_unary state "<alias>" make_literal node)

and print_namespace_path :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a namespace_path reg -> unit =
  fun print root state {value; region} ->
    let children = Tree.(
        mk_children_nsepseq make_literal value.namespace_path
      @ [Tree.mk_child print value.property])
    in Tree.make state root ~region children

and print_ImportAllAs state (node: import_all_as reg) =
  let Region.{value; region} = node in
  let {kwd_import=_; times=_; kwd_as=_; alias;
       kwd_from=_; file_path} = value in
  let children = Tree.[
    mk_child print_alias     alias;
    mk_child print_file_path file_path]
  in Tree.make ~region state "ImportAllAs" children

and print_file_path state (node: string wrap) =
  Tree.(make_unary state "<file path>" make_literal node)

and print_ImportFrom state (node: import_from reg) =
  let Region.{value; region} = node in
  let {kwd_import=_; imported; kwd_from=_; file_path} = value in
  let children = Tree.[
    mk_child print_imported  imported;
    mk_child print_file_path file_path]
  in Tree.make ~region state "ImportFrom" children

and print_imported state (node: (property_name, comma) sep_or_term braces) =
  let Region.{region; value} = node in
  Tree.(of_sep_or_term state ~region "<namespace path>"
                       print_variable value.inside)

(* Interface declaration *)

and print_D_Interface state (node : interface_decl reg) =
  let Region.{region; value} = node in
  let {kwd_interface=_; intf_name; intf_body} = value in
  let children = Tree.(mk_child make_literal intf_name)
                 :: mk_children_intf_body intf_body
  in Tree.make ~region state "D_Interface" children

and mk_children_intf_body (node: intf_body) =
  Tree.mk_children_sep_or_term print_intf_entry node.value.inside

and print_intf_entry state = function
  I_Attr  e -> print_I_Attr  state e
| I_Type  e -> print_I_Type  state e
| I_Const e -> print_I_Const state e

and print_I_Attr state (node : attribute * intf_entry) =
  let attribute, entry = node in
  let children = Tree.[
    mk_child print_attribute  attribute;
    mk_child print_intf_entry entry]
  in Tree.make state "I_Attr" children

and print_I_Type state (node : intf_type reg) =
  let Region.{region; value} = node in
  let {kwd_type=_; type_name; type_rhs} = value in
  let children = Tree.[
    mk_child     print_variable type_name;
    mk_child_opt print_type_rhs type_rhs]
  in Tree.make ~region state "I_Type" children

and print_type_rhs state (node : equal * type_expr) =
  print_type_expr state (snd node)

and print_I_Const state (node : intf_const reg) =
  let Region.{value; region} = node in
  let {kwd_const=_; const_name; const_type} = value in
  let children = Tree.[
    mk_child print_variable        const_name;
    mk_child print_type_annotation const_type]
  in Tree.make ~region state "I_Const" children

(* Modules *)

and print_D_Namespace state (node: namespace_decl reg) =
  let Region.{value; region} = node in
  let {kwd_namespace=_; namespace_name; namespace_type; namespace_body} =
    value in
  let children = Tree.[
    mk_child     print_namespace_name namespace_name;
    mk_child_opt print_interface      namespace_type;
    mk_child     print_statements     namespace_body.value.inside]
  in Tree.make ~region state "D_Namespace" children

and print_namespace_name state (node: namespace_name) =
  Tree.(make_unary state "<namespace>" make_literal node)

and print_interface state (node: interface) =
  let Region.{value; region} = node in
  let _kwd_implements, intf_expr = value in
  Tree.make_unary ~region state "<interface>" print_intf_expr intf_expr

and print_intf_expr state = function
  I_Body i -> print_I_Body state i
| I_Path i -> print_I_Path state i

and print_I_Body state (node: intf_body) =
  Tree.make state "I_Body" (mk_children_intf_body node)

and print_I_Path state = print_namespace_selection state

and print_namespace_selection state = function
  M_Path  p -> print_M_Path  state p
| M_Alias p -> print_M_Alias state p

and print_M_Path state (node: namespace_name namespace_path reg) =
  print_namespace_path Tree.make_literal "M_Path" state node

and print_M_Alias state (node: namespace_name) =
  Tree.(make_unary state "M_Alias" make_literal node)

(* Type declaration *)

and print_D_Type state (node: type_decl reg) =
  let Region.{value; region} = node in
  let {kwd_type=_; generics; name; eq=_; type_expr} = value in
  let children = Tree.[
    mk_child     print_variable  name;
    mk_child_opt print_generics  generics;
    mk_child     print_type_expr type_expr]
  in Tree.make state ~region "D_Type" children

(* Value declaration *)

and print_D_Value state (node: value_decl reg) =
  let Region.{region; value} = node in
  let {kind; bindings} = value in
  let children = Tree.mk_child print_var_kind kind
                 :: mk_children_bindings bindings
  in Tree.make state ~region "D_Value" children

and print_var_kind state = function
  `Let   kwd_let   -> Tree.make_literal state kwd_let
| `Const kwd_const -> Tree.make_literal state kwd_const

and mk_children_bindings (node: (val_binding reg, comma) Utils.nsepseq) =
  Tree.mk_children_nsepseq print_val_binding node

and print_val_binding state (node: val_binding reg) =
  let Region.{region; value} = node in
  let {pattern; generics; rhs_type; eq=_; rhs_expr} = value in

  let children = Tree.[
    mk_child     print_binders         pattern;
    mk_child_opt print_generics        generics;
    mk_child_opt print_type_annotation rhs_type;
    mk_child     print_rhs             rhs_expr]
  in Tree.make state ~region "<binding>" children

and print_rhs state (node: expr) =
  Tree.make_unary state "<rhs>" print_expr node

and print_binders state (node: pattern) =
  Tree.make_unary state "<binders>" print_pattern node

and print_generics state (node: generics) =
  let Region.{region; value} = node in
  let seq = value.inside in
  Tree.(of_sep_or_term state ~region "<type vars>" print_variable seq)

and print_type_annotation state (node : type_annotation) =
  Tree.make_unary state "<type>" print_type_expr (snd node)

(* TYPE EXPRESSIONS *)

and print_type_expr state = function
  T_App         t -> print_T_App          state t
| T_Array       t -> print_T_Array        state t
| T_Attr        t -> print_T_Attr         state t
| T_Fun         t -> print_T_Fun          state t
| T_Int         t -> print_T_Int          state t
| T_NamePath    t -> print_T_NamePath     state t
| T_Object      t -> print_T_Object       state t
| T_Par         t -> print_T_Par          state t
| T_ParameterOf  t -> print_T_ParameterOf state t
| T_String      t -> print_T_String       state t
| T_Union       t -> print_T_Union        state t
| T_Var         t -> print_T_Var          state t
| T_Variant     t -> print_T_Variant      state t

(* Application of type constructors *)

and print_T_App state (node: (type_expr * type_ctor_args) reg) =
  let Region.{region; value} = node in
  let type_ctor, args = value in
  let args = args.value.inside in
  let children = Tree.(
    mk_child print_type_expr type_ctor
    :: mk_children_nsep_or_term ~root:"<arguments>" print_type_expr args)
  in Tree.make state ~region "T_App" children

(* Attributed type expression *)

and print_T_Attr state (node : attribute * type_expr) =
  let attribute, type_expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_type_expr type_expr]
  in Tree.make state "T_Attr"children

(* Array types *)

and print_T_Array state (node : array_type) =
  let Region.{value; region} = node in
  Tree.of_nsep_or_term ~region state "T_Array" print_type_expr value.inside

(* Functional types *)

and print_T_Fun state (node : fun_type) =
  let Region.{value; region} = node in
  let params, _, codomain = value in
  let children =
    Tree.mk_children_sep_or_term print_fun_type_param params.value.inside
  @ [Tree.mk_child print_codomain codomain]
  in Tree.make state "T_Fun" ~region children

and print_fun_type_param state (node: fun_type_param reg) =
  let Region.{region; value} = node in
  let pattern, type_annotation = value in
  let children = Tree.[
    mk_child print_pattern         pattern;
    mk_child print_type_annotation type_annotation]
  in Tree.make ~region state "<parameter>" children

and print_codomain state (node: type_expr) =
  Tree.make_unary state "<codomain>" print_type_expr node

(* The integer type *)

and print_T_Int state (node : int_literal) =
  Tree.make_int "T_Int" state node

(* Namespace paths in type expressions *)

and print_T_NamePath state (node : type_expr namespace_path reg) =
  print_namespace_path print_type_expr "T_NamePath" state node

(* Parenthesised type expressions *)

and print_T_Par state (node : type_expr par) =
  Tree.make_unary state "T_Par" print_type_expr node.value.inside

(* Type parameter *)

and print_T_ParameterOf state (node : parameter_of_type reg) =
  let {kwd_parameter_of=_; namespace_path} = node.value in
  Tree.make_unary state "T_ParameterOf" print_namespace_selection
                  namespace_path

and print_T_Object state (node : type_expr _object) =
  print_object print_type_expr "T_Object" state node

and print_object :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a _object -> unit =
  fun print root state {value; region} ->
    Tree.of_sep_or_term ~region state root (print_property print) value.inside

and print_property :
  'a.'a Tree.printer -> Tree.state -> 'a property reg -> unit =
  fun print_rhs state property ->
    let Region.{value; region} = property in
    let {attributes; property_id; property_rhs} = value in
    let print_rhs state = print_rhs state <@ snd in
    let children = Tree.[
      mk_child     print_property_id property_id;
      mk_child_opt print_rhs      property_rhs]
    @ mk_children_attr attributes
    in Tree.make ~region state "<property>" children

and print_property_id state = function
  F_Name i -> print_F_Name state i
| F_Int  i -> print_F_Int  state i
| F_Str  i -> print_F_Str  state i

and print_F_Name state (node: property_name) =
  Tree.(make_unary state "F_Name" print_variable node)

and print_F_Int state (node: int_literal) =
  Tree.make_int "F_Int" state node

and print_F_Str state (node: string_literal) =
  Tree.make_string "F_Str" state node

(* Type string *)

and print_T_String state (node: string_literal) =
  Tree.make_string "T_String" state node

(* Discriminated unions *)

and print_T_Union state (node: union_type) =
  let Region.{region; value} = node
  and print = print_object print_type_expr "<subset>" in
  Tree.of_nsep_or_pref ~region state "T_Union" print value

(* Type variable *)

and print_T_Var state (node : variable) =
  Tree.(make_unary state "T_Var" print_variable node)

(* Variant types *)

and print_T_Variant state (node : variant_type) =
  let Region.{value; region} = node in
  Tree.of_nsep_or_pref ~region state "T_Variant"
                       (print_variant_kind print_type_expr) value

and print_variant_kind : 'a. 'a Tree.printer -> 'a variant_kind Tree.printer =
 fun printer state node ->
  match node with
    Variant   node -> print_variant printer state node
  | Bracketed node -> print_bracketed_variant printer state node
  | Legacy    node -> print_legacy_variant printer state node

and print_variant : 'a. 'a Tree.printer -> 'a variant reg Tree.printer =
 fun printer state node ->
  let ({attributes; tuple} : 'a variant) = node.value in
  let _, app = tuple in
  let ctor, ctor_params =
    match app with
      ZeroArg ctor -> ctor, []
    | MultArg (ctor, brackets) ->
        let app = brackets.value.inside in
        match app with
          `Sep p -> ctor, Utils.nsepseq_to_list p
        | `Term ctor_params ->
             ctor, List.map ~f:fst @@ Utils.nseq_to_list ctor_params
  in
  let children =
    Tree.mk_child print_ctor_app_kind ctor ::
    Tree.mk_children_list printer ~root:"<parameters>" ctor_params
    @ mk_children_attr attributes
  in Tree.make ~region:node.region state "<variant>" children

and print_ctor_app_kind state = function
  CtorStr c ->
    Tree.(make_unary ~region:c#region state "CtorStr" make_literal c)
| CtorName c ->
    Tree.(make_unary ~region:c#region state "CtorName" make_literal c)

and print_bracketed_variant : 'a. 'a Tree.printer -> 'a bracketed_variant reg Tree.printer =
 fun printer state node ->
  let {attributes; sharp = _; tuple} = node.value in
  let {ctor; args} : 'a bracketed_variant_args = tuple.value.inside in
  let ctor_params =
    Option.value_map ~default:[] ~f:(Utils.sep_or_term_to_list <@ snd) args
  in
  let children =
    Tree.mk_child printer ctor ::
    Tree.mk_children_list printer ~root:"<parameters>" ctor_params
    @ mk_children_attr attributes
  in Tree.make ~region:node.region state "<bracketed_variant>" children

and print_legacy_variant : 'a. 'a Tree.printer -> 'a legacy_variant reg Tree.printer =
 fun printer state node ->
  let {attributes; tuple} = node.value in
  let {ctor; args} = tuple.value.inside in
  let args = List.map ~f:snd args in
  let children =
    Tree.mk_child (Tree.make_node ~region:ctor#region) ctor#payload ::
    Tree.mk_children_list printer ~root:"<args>" args
    @ mk_children_attr attributes
  in Tree.make ~region:node.region state "<legacy_variant>" children

(* PATTERNS *)

and print_pattern state = function
  P_Attr     p -> print_P_Attr     state p
| P_Array    p -> print_P_Array    state p
| P_Bytes    p -> print_P_Bytes    state p
| P_CtorApp  p -> print_P_CtorApp  state p
| P_False    p -> print_P_False    state p
| P_Int      p -> print_P_Int      state p
| P_Mutez    p -> print_P_Mutez    state p
| P_NamePath p -> print_P_NamePath state p
| P_Nat      p -> print_P_Nat      state p
| P_Object   p -> print_P_Object   state p
| P_String   p -> print_P_String   state p
| P_True     p -> print_P_True     state p
| P_Typed    p -> print_P_Typed    state p
| P_Var      p -> print_P_Var      state p
| P_Verbatim p -> print_P_Verbatim state p

(* Attributes patterns *)

and print_P_Attr state (node : attribute * pattern) =
  let attribute, pattern = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_pattern   pattern]
  in Tree.make state "P_Attr" children

(* Bytes as literals in patterns *)

and print_P_Bytes state (node: (lexeme * Hex.t) wrap) =
  Tree.make_bytes "P_Bytes" state node

(* Application of data constructors in patterns *)

and print_P_CtorApp state (node: pattern variant_kind) =
  print_variant_kind print_pattern state node

(* "false" as a pattern *)

and print_P_False state (node : kwd_false) =
  Tree.make_node ~region:node#region state "P_False"

(* Integers in patterns *)

and print_P_Int state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "P_Int" state node

(* Mutez in patterns *)

and print_P_Mutez state (node : (lexeme * Int64.t) wrap) =
  Tree.make_mutez "P_Mutez" state node

(* Qualified patterns *)

and print_P_NamePath state (node : pattern namespace_path reg) =
  print_namespace_path print_pattern "P_NamePath" state node

(* Natural numbers in patterns *)

and print_P_Nat state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "P_Nat" state node

(* Record patterns *)

and print_P_Object state (node: pattern _object) =
  let Region.{value; region} = node in
  let print = print_property print_pattern in
  Tree.of_sep_or_term ~region state "P_Object" print value.inside

(* String literals as patterns *)

and print_P_String state (node : lexeme wrap) =
  Tree.make_string "P_String" state node

(* "true" as an expression *)

and print_P_True state (node : kwd_true) =
  Tree.make_node ~region:node#region state "P_True"

(* Tuple patterns *)

and print_P_Array state (node: pattern _array) =
  print_array print_pattern "P_Array" state node

and print_array :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a _array -> unit =
  fun print root state brackets ->
    let Region.{region; value} = brackets in
    let elements = value.inside in
    Tree.of_sep_or_term ~region state root (print_element print) elements

and print_element :
  'a.'a Tree.printer -> Tree.state -> 'a element -> unit =
  fun print state -> function
    None, element -> print state element
  | Some _ellipsis, element -> Tree.make_unary state "..." print element

(* Typed pattern (function parameter) *)

and print_P_Typed state (node : typed_pattern reg) =
  let Region.{value; region} = node in
  let pattern, (_, type_expr) = value in
  let children = Tree.[
    mk_child print_pattern   pattern;
    mk_child print_type_expr type_expr]
  in Tree.make state ~region "P_Typed" children

(* A pattern variable *)

and print_P_Var state (node : variable) =
  Tree.(make_unary state "P_Var" print_variable node)

(* A verbatim string as a pattern *)

and print_P_Verbatim state (node : lexeme wrap) =
  Tree.make_verbatim "P_Verbatim" state node

(* EXPRESSIONS *)

and print_expr state = function
  E_Add        e -> print_E_Add        state e
| E_AddEq      e -> print_E_AddEq      state e
| E_And        e -> print_E_And        state e
| E_App        e -> print_E_App        state e
| E_Array      e -> print_E_Array      state e
| E_ArrowFun   e -> print_E_ArrowFun   state e
| E_Assign     e -> print_E_Assign     state e
| E_Attr       e -> print_E_Attr       state e
| E_BitAnd     e -> print_E_BitAnd     state e
| E_BitAndEq   e -> print_E_BitAndEq   state e
| E_BitNeg     e -> print_E_BitNeg     state e
| E_BitOr      e -> print_E_BitOr      state e
| E_BitOrEq    e -> print_E_BitOrEq    state e
| E_BitSl      e -> print_E_BitSl      state e
| E_BitSlEq    e -> print_E_BitSlEq    state e
| E_BitSr      e -> print_E_BitSr      state e
| E_BitSrEq    e -> print_E_BitSrEq    state e
| E_BitXor     e -> print_E_BitXor     state e
| E_BitXorEq   e -> print_E_BitXorEq   state e
| E_Bytes      e -> print_E_Bytes      state e
| E_CodeInj    e -> print_E_CodeInj    state e
| E_ContractOf e -> print_E_ContractOf state e
| E_CtorApp    e -> print_E_CtorApp    state e
| E_Div        e -> print_E_Div        state e
| E_DivEq      e -> print_E_DivEq      state e
| E_Do         e -> print_E_Do         state e
| E_Equal      e -> print_E_Equal      state e
| E_False      e -> print_E_False      state e
| E_Function   e -> print_E_Function   state e
| E_Geq        e -> print_E_Geq        state e
| E_Gt         e -> print_E_Gt         state e
| E_Int        e -> print_E_Int        state e
| E_Leq        e -> print_E_Leq        state e
| E_Lt         e -> print_E_Lt         state e
| E_Match      e -> print_E_Match      state e
| E_Mult       e -> print_E_Mult       state e
| E_MultEq     e -> print_E_MultEq     state e
| E_Mutez      e -> print_E_Mutez      state e
| E_NamePath   e -> print_E_NamePath   state e
| E_Nat        e -> print_E_Nat        state e
| E_Neg        e -> print_E_Neg        state e
| E_Neq        e -> print_E_Neq        state e
| E_Not        e -> print_E_Not        state e
| E_Object     e -> print_E_Object     state e
| E_Or         e -> print_E_Or         state e
| E_Par        e -> print_E_Par        state e
| E_PostDecr   e -> print_E_PostDecr   state e
| E_PostIncr   e -> print_E_PostIncr   state e
| E_PreDecr    e -> print_E_PreDecr    state e
| E_PreIncr    e -> print_E_PreIncr    state e
| E_Proj       e -> print_E_Proj       state e
| E_Rem        e -> print_E_Rem        state e
| E_RemEq      e -> print_E_RemEq      state e
| E_String     e -> print_E_String     state e
| E_Sub        e -> print_E_Sub        state e
| E_SubEq      e -> print_E_SubEq      state e
| E_Ternary    e -> print_E_Ternary    state e
| E_True       e -> print_E_True       state e
| E_Typed      e -> print_E_Typed      state e
| E_Update     e -> print_E_Update     state e
| E_Var        e -> print_E_Var        state e
| E_Verbatim   e -> print_E_Verbatim   state e
| E_Xor        e -> print_E_Xor        state e

(* Arithmetic addition *)

and print_E_Add state (node : plus bin_op reg) =
  print_bin_op state "E_Add" node

and print_bin_op state root (node : 'op bin_op reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in Tree.make state root ~region children

(* Addition & assignment *)

and print_E_AddEq state (node: plus_eq bin_op reg) =
  print_bin_op state "E_AddPlus" node

(* Boolean conjunction *)

and print_E_And state (node : bool_and bin_op reg) =
  print_bin_op state "E_And" node

(* Data constructor application or function call *)

and print_E_App state (node : (expr * arguments) reg) =
  let Region.{value; region} = node in
  let fun_or_ctor, args = value
  and mk_func state =
    Tree.make_unary state "<fun or ctor>" print_expr
  and mk_args state (node : arguments) =
    let Region.{region; value} = node in
    match value.inside with
      None -> Tree.make_node ~region state "()"
    | Some args -> Tree.of_nsepseq state "<arguments>" print_expr args
  in
  let children = Tree.[
    mk_child mk_func fun_or_ctor;
    mk_child mk_args args]
  in Tree.make state "E_App" ~region children

(* Assignment *)

and print_E_Assign state (node: equal bin_op reg) =
  print_bin_op state "E_Assign" node

(* Attributed expressions *)

and print_E_Attr state (node : attribute * expr) =
  let attribute, expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_expr      expr]
  in Tree.make state "E_Attr" children

(* Bitwise conjunction *)

and print_E_BitAnd state (node : bit_and bin_op reg) =
  print_bin_op state "E_BitAnd" node

(* Bitwise conjunction & Assignment *)

and print_E_BitAndEq state (node : bit_and_eq bin_op reg) =
  print_bin_op state "E_BitAndEq" node

(* Bitwise negation *)

and print_E_BitNeg state (node : bit_neg un_op reg) =
  print_un_op state "E_Neg" node

(* Bitwise disjunction *)

and print_E_BitOr state (node : bit_or bin_op reg) =
  print_bin_op state "E_Or" node

(* Bitwise disjunction & Assignment *)

and print_E_BitOrEq state (node : bit_or_eq bin_op reg) =
  print_bin_op state "E_OrEq" node

(* Bitwise left shift *)

and print_E_BitSl state (node : bit_sl bin_op reg) =
  print_bin_op state "E_BitSl" node

(* Bitwise left shift & Assignment *)

and print_E_BitSlEq state (node : bit_sl_eq bin_op reg) =
  print_bin_op state "E_BitSlEq" node

(* Bitwise right shift *)

and print_E_BitSr state (node : bit_sr bin_op reg) =
  print_bin_op state "E_BitSr" node

(* Bitwise right shift & Assignment *)

and print_E_BitSrEq state (node : bit_sr_eq bin_op reg) =
  print_bin_op state "E_BitSrEq" node

(* Bitwise exclusive disjunction *)

and print_E_BitXor state (node : bit_xor bin_op reg) =
  print_bin_op state "E_BitXor" node

(* Bitwise exclusive disjunction *)

and print_E_BitXorEq state (node : bit_xor_eq bin_op reg) =
  print_bin_op state "E_BitXorEq" node

(* Bytes as expressions *)

and print_E_Bytes state (node : (lexeme * Hex.t) wrap) =
  Tree.make_bytes "E_Bytes" state node

(* Code Injection *)

and print_E_CodeInj state (node : code_inj reg) =
  let Region.{value; region} = node in
  let {language; code} = value in
  let children = Tree.[
    mk_child print_language language;
    mk_child print_code     code]
  in Tree.make state "E_CodeInj" ~region children

and print_language state (node : language) =
  Tree.(make_unary state "<language>" make_literal node)

and print_code state (node : expr) =
  Tree.make_unary state "<code>" print_expr node

(* Contract of expression *)

and print_E_ContractOf state (node: contract_of_expr reg) =
  let {kwd_contract_of=_; namespace_path} = node.value in
  let path = namespace_path.value.inside in
  Tree.make_unary state "E_ContractOf" print_namespace_selection path

(* Application of data constructor as expressions *)

and print_E_CtorApp state (node : expr variant_kind) =
  print_variant_kind print_expr state node

(* The Euclidean quotient *)

and print_E_Div state (node : slash bin_op reg) =
  print_bin_op state "E_Div" node

(* Euclidean quotient & assignment *)

and print_E_DivEq state (node: div_eq bin_op reg) =
  print_bin_op state "E_DivEq" node

(* Do expressions *)

and print_E_Do state (node : do_expr reg) =
  let Region.{region; value} = node in
  let {kwd_do=_; statements} = value in
  let stmts = statements.value.inside in
  Tree.of_nseq ~region state "E_Do"
               (fun state -> print_statement state <@ fst) stmts

(* Equality *)

and print_E_Equal state (node : equal_cmp bin_op reg) =
  print_bin_op state "E_Equal" node

(* "false" as an expression *)

and print_E_False state (node : kwd_false) =
  Tree.make_node ~region:node#region state "E_False"

(* Functional expressions introduced with the keyword "function" *)

and print_E_Function state (node : function_expr reg) =
  let Region.{value; region} = node in
  let {kwd_function=_; generics; parameters; rhs_type; fun_body} = value in
  let children = Tree.[
    mk_child_opt print_generics         generics;
    mk_child     print_arrow_fun_params parameters;
    mk_child_opt print_rhs_type         rhs_type;
    mk_child     print_fun_body         fun_body]
  in Tree.make ~region state "E_Function" children

and print_rhs_type state (node: type_annotation) =
  Tree.make_unary state "<rhs type>" print_type_expr (snd node)

and print_arrow_fun_params state = function
  ParParams  p -> print_ParParams  state p
| NakedParam p -> print_NakedParam state p

and print_ParParams state (node: (pattern, comma) sep_or_term par) =
  let Region.{value; region} = node in
  Tree.of_sep_or_term ~region state "ParParams" print_pattern value.inside

and print_NakedParam state (node: pattern) =
  Tree.(make_unary state "NakedParam" print_pattern node)

and print_fun_body state = function
  StmtBody b -> print_StmtBody state b
| ExprBody b -> print_ExprBody state b

and print_StmtBody state (node: statements braces) =
  Tree.of_nseq state "StmtBody"
               (fun state -> print_statement state <@ fst) node.value.inside

and print_ExprBody state (node: expr) =
  Tree.make_unary state "ExprBody" print_expr node

(* Greater or Equal *)

and print_E_Geq state (node : geq bin_op reg) =
  print_bin_op state "E_Geq" node

(* Greater Than *)

and print_E_Gt state (node : gt bin_op reg) =
  print_bin_op state "E_Gt" node

(* Integer literals as expressions *)

and print_E_Int state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "E_Int" state node

(* Lower or Equal *)

and print_E_Leq state (node : leq bin_op reg) =
  print_bin_op state "E_Leq" node

(* Lower Than *)

and print_E_Lt state (node : lt bin_op reg) =
  print_bin_op state "E_lt" node

(* Pattern matching *)

and print_E_Match state (node : match_expr reg) =
  let Region.{value; region} = node in
  let {kwd_match=_; subject; clauses} = value in
  let subject = subject.value.inside
  and clauses = clauses.value.inside in
  let children = Tree.[
    mk_child print_subject       subject;
    mk_child print_match_clauses clauses]
  in Tree.make state ~region "E_Match" children

and print_match_clauses state = function
  AllClauses    c -> print_AllClauses    state c
| DefaultClause c -> print_DefaultClause state c

and print_AllClauses state (node : all_match_clauses) =
  let normal_clauses, default_clause_opt = node in
  let children = Tree.(
    mk_children_nseq print_match_clause normal_clauses
    @ [mk_child_opt print_match_default default_clause_opt])
  in Tree.make state "AllClauses" children

and print_DefaultClause state (node : match_default reg) =
  print_match_default state node

and print_match_clause state (node : match_clause reg) =
  let Region.{value; region} = node in
  let {kwd_when=_; filter; colon=_; clause_expr} = value in
  let children = Tree.[
    mk_child print_pattern  filter.value.inside;
    mk_child print_expr     clause_expr]
  in Tree.make ~region state "<clause>" children

and print_match_default state (node : match_default reg) =
  let Region.{value; region} = node in
  let {kwd_default=_; colon=_; default_expr} = value in
  Tree.make_unary state ~region "DefaultClause" print_expr default_expr

(* Arithmetic modulo *)

and print_E_Rem state (node : remainder bin_op reg) =
  print_bin_op state "E_Rem" node

(* Arithmetic modulo & assignment *)

and print_E_RemEq state (node: rem_eq bin_op reg) =
  print_bin_op state "E_RemEq" node

(* Qualified expression *)

and print_E_NamePath state (node : expr namespace_path reg) =
  print_namespace_path print_expr "E_NamePath" state node

(* Multiplication *)

and print_E_Mult state (node : times bin_op reg) =
  print_bin_op state "E_Mult" node

(* Mutez literals *)

and print_E_Mutez state (node : (lexeme * Int64.t) wrap) =
  Tree.make_mutez "E_Mutez" state node

(* Natural numbers *)

and print_E_Nat state (node : (lexeme * Z.t) wrap) =
  Tree.make_nat "E_Nat" state node

(* Arithmetic negation *)

and print_E_Neg state (node : minus un_op reg) =
  print_un_op state "E_Neg" node

and print_un_op state root (node : 'op un_op reg) =
  let Region.{value; region} = node in
  Tree.make_unary state root ~region print_expr value.arg

(* Inequality *)

and print_E_Neq state (node : neq bin_op reg) =
  print_bin_op state "E_Neq" node

(* Logical negation *)

and print_E_Not state (node : bool_neg un_op reg) =
  print_un_op state "E_Not" node

(* Logical disjunction *)

and print_E_Or state (node : bool_or bin_op reg) =
  print_bin_op state "E_Or" node

(* Parenthesised expressions *)

and print_E_Par state (node : expr par) =
  let Region.{value; region} = node in
  Tree.make_unary state "E_Par" ~region print_expr value.inside

(* Increments & decrements *)

and print_E_PostDecr state (node : decrement un_op reg) =
  print_un_op state "E_PostDecr" node

and print_E_PostIncr state (node: increment un_op reg) =
  print_un_op state "E_PostIncr" node

and print_E_PreDecr state (node: decrement un_op reg) =
  print_un_op state "E_PreDecr" node

and print_E_PreIncr state (node: increment un_op reg) =
  print_un_op state "E_PreIncr" node

(* Projections *)

and print_E_Proj state (node : projection reg) =
  let Region.{value; region} = node in
  let {object_or_array; property_path} = value in
  let children = Tree.(
       mk_child         print_expr      object_or_array
    :: mk_children_nseq print_selection property_path)
  in Tree.make state ~region "E_Proj" children

and print_selection state = function
  PropertyName s -> print_PropertyName state s
| PropertyStr  s -> print_PropertyStr  state s
| Component    s -> print_Component    state s

and print_PropertyName state (node : dot * property_name) =
  Tree.make_unary state "PropertyName" print_variable (snd node)

and print_PropertyStr state (node : string_literal brackets) =
  Tree.(make_string "PropertyStr" state node.value.inside)

and print_Component state (node : int_literal brackets) =
  Tree.(make_int "Component" state node.value.inside)

(* Record expressions *)

and print_E_Object state (node: expr _object) =
  print_object print_expr "E_Object" state node

(* Strings as expressions *)

and print_E_String state (node: lexeme wrap) =
  Tree.make_string "E_String" state node

(* Arithmetic subtraction *)

and print_E_Sub state (node: minus bin_op reg) =
  print_bin_op state "E_Sub" node

(* Subtraction & assignment *)

and print_E_SubEq state (node: minus_eq bin_op reg) =
  print_bin_op state "E_SubEq" node

(* Ternary conditional expression *)

and print_E_Ternary state (node: ternary reg) =
  let Region.{value; region} = node in
  let {condition; qmark=_; truthy; colon=_; falsy} = value in
  let print_truthy state (node: expr) =
    Tree.make_unary state "<true>" print_expr node
  and print_falsy state (node: expr) =
    Tree.make_unary state "<false>" print_expr node in
  let children = Tree.[
    mk_child print_expr   condition;
    mk_child print_truthy truthy;
    mk_child print_falsy  falsy]
  in Tree.make state ~region "E_Ternary" children

(* Multiplication & Assignment *)

and print_E_MultEq state (node: times_eq bin_op reg) =
  print_bin_op state "E_MultEq" node

(* "true" as an expression *)

and print_E_True state (node : kwd_true) =
  Tree.make_node ~region:node#region state "E_True"

(* Tuple of expressions *)

and print_E_Array state (node: expr _array) =
  print_array print_expr "E_Array" state node

(* Arrow functions *)

and print_E_ArrowFun state (node : arrow_fun_expr reg) =
  let Region.{value; region} = node in
  let {generics; parameters; rhs_type; arrow=_; fun_body} = value in
  let children = Tree.[
    mk_child_opt print_generics         generics;
    mk_child     print_arrow_fun_params parameters;
    mk_child_opt print_rhs_type         rhs_type;
    mk_child     print_fun_body         fun_body]
  in Tree.make ~region state "E_ArrowFun" children

(* Typed expressions *)

and print_E_Typed state (node: typed_expr reg) =
  let Region.{value; region} = node in
  let expr, _, type_expr = value in
  let children = Tree.[
    mk_child print_expr      expr;
    mk_child print_type_expr type_expr]
  in Tree.make state ~region "E_Typed" children

(* Record functional updates *)

and print_E_Update state (node: update_expr braces) =
  let Region.{region; value} = node in
  let {ellipsis=_; _object; sep=_; updates} = value.inside in
  let print_updates state (node: (expr property reg, semi) sep_or_term) =
    let print = print_property print_expr in
    Tree.of_sep_or_term state "<updates>" print node in
  let children = Tree.[
    mk_child print_expr    _object;
    mk_child print_updates updates]
  in Tree.make state ~region "E_Update" children

(* Variables denoting expressions *)

and print_E_Var state (node: variable) =
  Tree.(make_unary state "E_Var" print_variable node)

(* Verbatim strings as expressions *)

and print_E_Verbatim state (node: verbatim_literal) =
  Tree.make_verbatim "E_Verbatim" state node

(* Logical exclusive disjunction *)

and print_E_Xor state (node : bool_xor bin_op reg) =
  print_bin_op state "E_Xor" node

(* STATEMENTS *)

and print_statement state = function
  S_Attr      s -> print_S_Attr      state s
| S_Block     s -> print_S_Block     state s
| S_Break     s -> print_S_Break     state s
| S_Continue  s -> print_S_Continue  state s
| S_Decl      s -> print_S_Decl      state s
| S_Directive s -> print_S_Directive state s
| S_Export    s -> print_S_Export    state s
| S_Expr      s -> print_S_Expr      state s
| S_For       s -> print_S_For       state s
| S_ForOf     s -> print_S_ForOf     state s
| S_If        s -> print_S_If        state s
| S_Return    s -> print_S_Return    state s
| S_Switch    s -> print_S_Switch    state s
| S_While     s -> print_S_While     state s

(* Attributed statement *)

and print_S_Attr state (node: attribute * statement) =
  let attribute, stmt = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_statement stmt]
  in Tree.make state "S_Attr" children

(* Block of statements *)

and print_S_Block state (node: statements braces) =
  let Region.{region; value} = node in
  let stmts = value.inside in
  Tree.of_nseq ~region state "S_Block"
               (fun state -> print_statement state <@ fst) stmts

(* Break statement *)

and print_S_Break state (node: kwd_break) =
  Tree.make_node ~region:node#region state "S_Break"

(* Continue statement *)

and print_S_Continue state (node : kwd_continue) =
  Tree.make_node ~region:node#region state "S_Continue"

(* Conditional statement *)

and print_S_If state (node: if_stmt reg) =
  let Region.{value; region} = node in
  let {kwd_if=_; test; if_so; if_not} = value in

  let print_if_so state (node: statement * semi option) =
    Tree.make_unary state "<true>" print_statement (fst node)

  and print_if_not state (node: kwd_else * statement) =
    Tree.make_unary state "<false>" print_statement (snd node) in

  let children = Tree.[
    mk_child     print_expr   test.value.inside;
    mk_child     print_if_so  if_so;
    mk_child_opt print_if_not if_not]
  in Tree.make ~region state "S_If" children

(* Declaration as a statement *)

and print_S_Decl state (node: declaration) =
  Tree.make_unary state "S_Decl" print_declaration node

(* Export statement *)

and print_S_Export state (node: export_stmt reg) =
  let Region.{value; region} = node in
  Tree.make_unary ~region state "S_Export" print_declaration (snd value)

(* Expression as a statement *)

and print_S_Expr state (node: expr) =
  Tree.make_unary state "S_Expr" print_expr node

(* For-loop statement *)

and print_S_For state (node: for_stmt reg) =
  let Region.{value; region} = node in
  let {kwd_for=_; range; for_body} : for_stmt = value in
  let range = range.value.inside in
  let {initialiser; semi1=_; condition; semi2=_; afterthought} = range in
  let print_initialiser state = Tree.make_unary state "<init>" print_statement
  and print_cond state = Tree.make_unary state "<cond>" print_expr in
  let open Tree in
  let children = [
    mk_child_opt print_initialiser initialiser;
    mk_child_opt print_cond        condition]
  @ mk_children_nsepseq_opt ~root:"<afterthought>" print_expr afterthought
  @ [mk_child_opt print_loop_body for_body]
  in make state ~region "S_For" children

and print_loop_body state = Tree.make_unary state "<body>" print_statement

(* ForOf-loop statement *)

and print_S_ForOf state (node: for_of_stmt reg) =
  let Region.{value; region} = node in
  let {kwd_for=_; range; for_of_body} = value in
  let range = range.value.inside in
  let {index_kind; index; kwd_of=_; expr} = range in
  let print_index state = function
    `Let _,   var -> Tree.(make_unary state "let"   print_variable var)
  | `Const _, var -> Tree.(make_unary state "const" print_variable var) in
  let children = Tree.[
    mk_child print_index     (index_kind, index);
    mk_child print_expr      expr;
    mk_child print_loop_body for_of_body]
  in Tree.make ~region state "S_ForOf" children

(* Return statement *)

and print_S_Return state (node: return_stmt reg) =
  let Region.{value; region} = node in
  match snd value with
    None -> Tree.make_node ~region state "S_Return"
  | Some e -> Tree.make_unary ~region state "S_Return" print_expr e

(* Switch statement *)

and print_S_Switch state (node: switch_stmt reg) =
  let Region.{region; value} = node in
  let {kwd_switch=_; subject; cases} = value in
  let subject = subject.value.inside
  and cases   = cases.value.inside in
  let children = Tree.[
    mk_child print_subject subject;
    mk_child print_cases   cases]
  in Tree.make state ~region "S_Switch" children

and print_subject state (node: expr) =
  Tree.make_unary state "<subject>" print_expr node

and print_cases state = function
  AllCases c -> print_AllCases state c
| Default  c -> print_Default  state c

and print_AllCases state (node: all_cases) =
  let normal_cases, default_case_opt = node in
  let children = Tree.(
    mk_children_nseq print_case normal_cases
    @ [mk_child_opt print_switch_default default_case_opt])
  in Tree.make state "AllCases" children

and print_case state (node: switch_case reg) =
  let Region.{value; region} = node in
  let {kwd_case=_; expr; colon=_; case_body} = value in
  let children = Tree.[
    mk_child     print_expr       expr;
    mk_child_opt print_statements case_body]
  in Tree.make ~region state "<case>" children

and print_Default state (node: switch_default reg) =
  print_switch_default state node

and print_switch_default state (node: switch_default reg) =
  let Region.{value; region} = node in
  let {kwd_default=_; colon=_; default_body} = value in
  match default_body with
    None -> Tree.make_node ~region state "<empty default>"
  | Some stmts ->
      Tree.make_unary ~region state "Default" print_statements stmts

and print_statements state (node: statements) =
  Tree.of_nseq state "<statements>"
               (fun state -> print_statement state <@ fst) node

(* While-loop statement *)

and print_S_While state (node: while_stmt reg) =
  let Region.{region; value} = node in
  let {kwd_while=_; invariant; while_body} = value in
  let children = Tree.[
    mk_child print_expr      invariant.value.inside;
    mk_child print_statement while_body]
  in Tree.make state ~region "S_While" children

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
