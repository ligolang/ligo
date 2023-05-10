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

let print_attribute state (node: Attr.t wrap) =
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

let print_D_Directive state (node: Directive.t) =
  let region, string = Directive.project node in
  Tree.(make_unary state "D_Directive" make_node ~region string)

(* PRINTING THE CST *)

(* The names of the printing functions are all prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration] prints values of type [declaration]; it can
       also be the type in a region, like [val print_variant : state
       -> variant reg -> unit], instead of [val print_variant : state
       -> variant -> unit];

     * the name of a CST constructor, for example, [print_E_Int],
       meaning that the argument of the constructor [CST.E_Int] is
       printed.

   Another design pattern we used here was to make all pattern
   matching on CST constructors a simple routing function, that is,
   devoid of logic. For example:

   and print_type_expr state = function
     T_App     t -> print_T_App     state t
   | T_Cart    t -> print_T_Cart    state t
   ...

   This means that those functions can be ignored by the maintainers
   if they know the data constructor.

   Guideline: When destructuring a value [v] of type [Region.t], use
   the following order: [let {value; region} = v in ...].

   The higher-order printers take as first argument the printer for
   the subtree(s), instead of the state first. For example,

   [val print_case_clause :
     'a.'a Tree.printer -> Tree.root -> Tree.state
        -> 'a case_clause reg -> unit]

   except for the printers from [Tree], like [Tree.make_unary], which
   are always qualified. *)

let rec print_cst state (node : cst) =
  Tree.of_nseq state "<cst>" print_declaration node.decl

(* DECLARATIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_D_Const] comes before [print_D_Fun]. *)

and print_declaration state = function
  D_Attr      d -> print_D_Attr      state d
| D_Const     d -> print_D_Const     state d
| D_Directive d -> print_D_Directive state d
| D_Fun       d -> print_D_Fun       state d
| D_Module    d -> print_D_Module    state d
| D_Type      d -> print_D_Type      state d

(* Attributed declaration *)

and print_D_Attr state (node: (attribute * declaration) reg) =
  let attribute, declaration = node.value in
  let children = Tree.[
    mk_child print_attribute   attribute;
    mk_child print_declaration declaration]
  in Tree.make state "D_Attr" children

(* Constant declarations *)

and print_D_Const state (node: const_decl reg) =
  let node = node.value in
  let children = Tree.[
    mk_child     print_pattern         node.pattern;
    mk_child_opt print_type_params     node.type_params;
    mk_child_opt print_type_annotation node.const_type;
    mk_child     print_expr            node.init]
  in Tree.make state "D_Const" children

and print_type_annotation state (_, type_expr) =
  Tree.make_unary state "<type>" print_type_expr type_expr

(* Function declarations *)

and print_D_Fun state (node: fun_decl reg) =
  let node = node.value in
  let children = Tree.[
    mk_child_opt print_recursive   node.kwd_recursive;
    mk_child     make_literal      node.fun_name;
    mk_child_opt print_type_params node.type_params;
    mk_child     print_parameters  node.parameters;
    mk_child_opt print_ret_type    node.ret_type;
    mk_child     print_ret_expr    node.return]
  in Tree.make state "D_Fun" children

and print_recursive state (node : lexeme wrap) =
  Tree.make_literal state node

and print_type_params state (node : type_params chevrons reg) =
  Tree.(of_nsepseq state "<type parameters>" make_literal node.value.inside)

and print_parameters state (node : parameters) =
  Tree.of_sepseq state "<parameters>" print_param_decl node.value.inside

and print_param_decl state (node: param_decl reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child     print_param_kind      value.param_kind;
    mk_child     print_pattern         value.pattern;
    mk_child_opt print_type_annotation value.param_type]
  in Tree.make state "<parameter>" ~region children

and print_param_kind state = function
  `Var   kwd_var   -> Tree.make_literal state kwd_var
| `Const kwd_const -> Tree.make_literal state kwd_const

and print_ret_type state (_, type_expr) =
  Tree.make_unary state "<return type>" print_type_expr type_expr

and print_ret_expr state (node : expr) =
  Tree.make_unary state "<return expression>" print_expr node

(* Module declarations *)

and print_D_Module state (node: module_decl reg) =
  let node = node.value in
  let children = Tree.[
    mk_child make_literal      node.name;
    mk_child print_module_expr node.module_expr]
  in Tree.make state "D_Module" children

and print_module_expr state (node: module_expr) =
  match node with
    M_Body e -> print_M_Body state e
  | M_Path e -> print_M_Path state e
  | M_Var  e -> print_M_Var  state e

and print_M_Body state (node: module_body reg) =
  let decl = node.value.declarations in
  Tree.make_unary state "M_Body" print_declarations decl

and print_M_Path state (node: module_name module_path reg) =
  print_module_path Tree.make_literal "M_Path" state node

and print_M_Var state (node: module_name) =
  Tree.(make_unary state "M_Var" make_literal node)

and print_declarations state (node: declarations) =
  Tree.of_nseq state "<declarations>" print_declaration node

(* Type declarations *)

and print_D_Type state (node: type_decl reg) =
  let node = node.value in
  let print_type_expr state =
    Tree.make_unary state "<type>" print_type_expr in
  let children = Tree.[
    mk_child     make_literal    node.name;
    mk_child_opt print_type_vars node.params;
    mk_child     print_type_expr node.type_expr]
  in Tree.make state "D_Type" children

and print_type_vars state (node: variable tuple) =
  Tree.(of_nsepseq state "<type variables>" make_literal node.value.inside)

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_T_App] comes before [print_T_Fun]. *)

and print_type_expr state = function
  T_App     t -> print_T_App     state t
| T_Attr    t -> print_T_Attr    state t
| T_Cart    t -> print_T_Cart    state t
| T_Fun     t -> print_T_Fun     state t
| T_Int     t -> print_T_Int     state t
| T_ModPath t -> print_T_ModPath state t
| T_Par     t -> print_T_Par     state t
| T_Record  t -> print_T_Record  state t
| T_String  t -> print_T_String  state t
| T_Sum     t -> print_T_Sum     state t
| T_Var     t -> print_T_Var     state t

(* Application of type constructors *)

and print_T_App state (node: (type_expr * type_tuple) reg) =
  let Region.{value; region} = node in
  let type_expr, tuple = value in
  let children = Tree.[
    mk_child print_type_expr  type_expr;
    mk_child print_type_tuple tuple]
  in Tree.make state "T_App" ~region children

and print_type_tuple state (node: type_expr tuple) =
  Tree.of_nsepseq state "<type arguments>" print_type_expr node.value.inside

(* Attributed type expression *)

and print_T_Attr state (node: attribute * type_expr) =
  let attribute, type_expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_type_expr type_expr]
  in Tree.make state "T_Attr"children

(* Cartesian products *)

and print_T_Cart state (node: cartesian) =
  let first, sep, others = node.value in
  let seq = Utils.nsepseq_cons first sep others in
  Tree.of_nsepseq state "T_Cart" print_type_expr seq

(* Functional types *)

and print_T_Fun state (node: (type_expr * arrow * type_expr) reg) =
  let Region.{value; region} = node in
  let domain, _, codomain = value in
  let children = Tree.[
    mk_child print_type_expr domain;
    mk_child print_type_expr codomain]
  in Tree.make state "T_Fun" ~region children

(* The integer type *)

and print_T_Int state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "T_Int" state node

(* Module paths in type expressions *)

and print_T_ModPath state (node: type_expr module_path reg) =
  print_module_path print_type_expr "T_ModPath" state node

and print_module_path :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a module_path reg -> unit =
  fun print root state {value; region} ->
  let children = Tree.(
    mk_children_nsepseq make_literal value.module_path
    @ [Tree.mk_child print value.field])
  in Tree.make state root ~region children

(* Parenthesised type expressions *)

and print_T_Par state (node: type_expr par reg) =
  Tree.make_unary state "T_Par" print_type_expr node.value.inside

(* Record types *)

and print_T_Record state (node: field_decl reg compound reg) =
  print_compound print_field_decl "T_Record" state node

and print_compound :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a compound reg -> unit =
  fun print root state {value; region} ->
    Tree.of_sepseq state ~region root print value.elements

and print_field_decl state (node: field_decl reg) =
  let Region.{value; region} = node in
  let children = Tree.mk_child_opt print_type_annotation value.field_type
                 :: mk_children_attr value.attributes
  and root = value.field_name#payload in
  Tree.make state root ~region children

(* The string type *)

and print_T_String state (node: lexeme wrap) =
  Tree.(make_unary state "T_String" make_string node)

(* Sum types *)

and print_T_Sum state (node: sum_type reg) =
  let Region.{region; value} = node in
  Tree.of_nsepseq state ~region "T_Sum" print_variant value.variants

and print_variant state (node: variant reg) =
  let node     = node.value in
  let children = Tree.mk_child_opt print_of_type_expr node.ctor_args
                 :: mk_children_attr node.attributes in
  let region = node.ctor#region
  and root   = node.ctor#payload in
  Tree.make state root ~region children

and print_of_type_expr state (_, type_expr) =
  print_type_expr state type_expr

(* A type variable *)

and print_T_Var state (node: variable) =
  Tree.(make_unary state "T_Var" make_literal node)


(* STATEMENTS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_S_Instr] comes before [print_S_Decl]. *)

and print_statement state = function
  S_Attr    i -> print_S_Attr    state i
| S_Decl    i -> print_S_Decl    state i
| S_Instr   i -> print_S_Instr   state i
| S_VarDecl i -> print_S_VarDecl state i

and print_S_Attr state (node: attribute * statement) =
  let attribute, statement = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_statement statement]
  in Tree.make state "S_Attr" children

and print_S_Decl state (node: declaration) =
  Tree.make_unary state "S_Decl" print_declaration node

and print_S_Instr state (node: instruction) =
  Tree.make_unary state "S_Instr" print_instruction node

and print_S_VarDecl state (node: var_decl reg) =
  let node = node.value in
  let children = Tree.[
    mk_child     print_pattern         node.pattern;
    mk_child_opt print_type_params     node.type_params;
    mk_child_opt print_type_annotation node.var_type;
    mk_child     print_expr            node.init]
  in Tree.make state "S_VarDecl" children

(* INSTRUCTIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Assign] comes before [print_I_Call]. *)

and print_instruction state = function
  I_Assign i -> print_I_Assign state i
| I_Call   i -> print_I_Call   state i
| I_Case   i -> print_I_Case   state i
| I_Cond   i -> print_I_Cond   state i
| I_For    i -> print_I_For    state i
| I_ForIn  i -> print_I_ForIn  state i
| I_Patch  i -> print_I_Patch  state i
| I_Remove i -> print_I_Remove state i
| I_Skip   i -> print_I_Skip   state i
| I_While  i -> print_I_While  state i

(* Assignments *)

and print_I_Assign state (node: assignment reg) =
  let Region.{value; region} = node
  and print_lhs state = Tree.make_unary state "<lhs>" print_expr
  and print_rhs state = Tree.make_unary state "<rhs>" print_expr in
  let children = Tree.[
    mk_child print_lhs value.lhs;
    mk_child print_rhs value.rhs]
  in Tree.make state "I_Assign" ~region children

(* Procedure calls *)

and print_I_Call state (node: call) =
  print_call state "I_Call" node

and print_call state root (node: call) =
  let Region.{value; region} = node in
  let func, args = value

  and mk_func state =
    Tree.make_unary state "<function>" print_expr

  and mk_args state (node: (expr, comma) Utils.sepseq par reg) =
    Tree.of_sepseq state "<arguments>" print_expr node.value.inside in

  let children = Tree.[
    mk_child mk_func func;
    mk_child mk_args args]
  in Tree.make state root ~region children

(* Case instructions *)

and print_I_Case state (node: test_clause case reg) =
  print_case print_test_clause "I_Case" state node

and print_case :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a case reg -> unit =
  fun print root state  {value; region} ->
    let print_case_test state =
      Tree.make_unary state "<condition>" print_expr in
    let cases =
      Tree.mk_children_nsepseq (print_case_clause print) value.cases in
    let children =
      Tree.mk_child print_case_test value.expr :: cases
    in Tree.make state root ~region children

and print_case_clause :
  'a.'a Tree.printer -> Tree.state -> 'a case_clause reg -> unit =
  fun print state {value; _} ->
    let print_clause_pattern state =
      Tree.make_unary state "<pattern>" print_pattern in
    let children = Tree.[
      mk_child print_clause_pattern value.pattern;
      mk_child print                value.rhs]
    in Tree.make state "<clause>" children

and print_test_clause state = function
  ClauseInstr c -> print_ClauseInstr state c
| ClauseBlock c -> print_ClauseBlock state c

and print_ClauseInstr state (node: instruction) =
  Tree.make_unary state "ClauseInstr" print_instruction node

and print_ClauseBlock state (node: block reg) =
  Tree.make_unary state "ClauseBlock" print_block node

and print_block state (node: block reg) =
  Tree.of_nsepseq state "<block>" print_statement node.value.statements

(* Conditional instructions *)

and print_I_Cond state (node: test_clause conditional reg) =
  print_conditional state "I_Cond"
    ~if_so:print_test_clause ~if_not:print_test_clause node

and print_conditional :
  'branch.
  Tree.state ->
  Tree.root ->
  if_so:('branch Tree.printer) ->
  if_not:('branch Tree.printer) ->
  'branch conditional reg ->
  unit =
  fun state root ~if_so:print_if_so ~if_not:print_if_not node ->
    let Region.{value; region} = node in
    let print_cond state =
      Tree.make_unary state "<condition>" print_expr
    and print_then state =
      Tree.make_unary state "<true>" print_if_so
    and print_else state (_, if_not) =
      Tree.make_unary state "<false>" print_if_not if_not in
    let children = Tree.[
      mk_child     print_cond value.test;
      mk_child     print_then value.if_so;
      mk_child_opt print_else value.if_not]
    in Tree.make state root ~region children

(* Iterations on integer intervals *)

and print_I_For state (node: for_int reg) =
  let Region.{value; region} = node in

  let print_init state (index, init : variable * expr) =
    let children = Tree.[
      mk_child make_literal index;
      mk_child print_expr init]
    in Tree.make state "<init>" children

  and print_bound state =
    Tree.make_unary state "<bound>" print_expr

  and print_step state (_, expr) =
    Tree.make_unary state "<step>" print_expr expr in

  let children = Tree.[
    mk_child     print_init  (value.index, value.init);
    mk_child     print_bound value.bound;
    mk_child_opt print_step  value.step;
    mk_child     print_block value.block]

  in Tree.make state "I_For" ~region children

(* Iterations over collections (maps, sets and lists) *)

and print_I_ForIn state = function
  ForMap       f -> print_ForMap       state f
| ForSetOrList f -> print_ForSetOrList state f

and print_ForMap state (node: for_map reg) =
  let Region.{value; region} = node in
  let print_binding state (source, _arrow, image) =
    let children = Tree.[
      mk_child make_literal source;
      mk_child make_literal image]
    in Tree.make state "<binding>" children
  and print_collection state =
    Tree.make_unary state "<collection>" print_expr in
  let children = Tree.[
    mk_child     print_binding    value.binding;
    mk_child     print_collection value.collection;
    mk_child     print_block      value.block]
  in Tree.make state "I_ForIn" ~region children

and print_ForSetOrList state (node: for_set_or_list reg) =
  let Region.{value; region} = node in

  let print_collection state =
    Tree.make_unary state "<collection>" print_expr

  and print_kind state = function
    `Set kwd_set   -> Tree.make_literal state kwd_set
  | `List kwd_list -> Tree.make_literal state kwd_list in

  let children = Tree.[
    mk_child make_literal value.var;
    mk_child print_kind         value.for_kind;
    mk_child print_collection   value.collection;
    mk_child print_block        value.block]

  in Tree.make state "I_ForIn" ~region children

(* Patches *)

and print_I_Patch state (node: patch reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.collection;
    mk_child print_expr value.patch] in
  Tree.make state "I_Patch" ~region children

(* Removal from sets and maps *)

and print_I_Remove state (node: removal reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.item;
    mk_child print_expr value.collection] in
  Tree.make state "I_Remove" ~region children

(* Skipping (non-operation) *)

and print_I_Skip state wrap =
  Tree.make_node ~region:wrap#region state "I_Skip"

(* While loops *)

and print_I_While state (node: while_loop reg) =
  let children = Tree.[
    mk_child print_cond  node.value.cond;
    mk_child print_block node.value.block]
  in Tree.make state "<while>" children

and print_cond state =
  Tree.make_unary state "<condition>" print_expr

and print_block_expr state =
  Tree.make_unary state "<expr>" print_expr


(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Bytes] comes before [print_P_Cons]. *)

and print_pattern state = function
  P_App      p -> print_P_App      state p
| P_Attr     p -> print_P_Attr     state p
| P_Bytes    p -> print_P_Bytes    state p
| P_Cons     p -> print_P_Cons     state p
| P_Ctor     p -> print_P_Ctor     state p
| P_Int      p -> print_P_Int      state p
| P_List     p -> print_P_List     state p
| P_ModPath  p -> print_P_ModPath  state p
| P_Mutez    p -> print_P_Mutez    state p
| P_Nat      p -> print_P_Nat      state p
| P_Nil      p -> print_P_Nil      state p
| P_Par      p -> print_P_Par      state p
| P_Record   p -> print_P_Record   state p
| P_String   p -> print_P_String   state p
| P_Tuple    p -> print_P_Tuple    state p
| P_Typed    p -> print_P_Typed    state p
| P_Var      p -> print_P_Var      state p
| P_Verbatim p -> print_P_Verbatim state p

(* A constructor application (or constant constructor) in patterns *)

and print_P_App state (node: (pattern * pattern tuple option) reg) =
  let Region.{value; region} = node in
  let pattern, tuple_opt = value in
  let children = Tree.[
    mk_child     print_pattern                   pattern;
    mk_child_opt (print_ctor_args print_pattern) tuple_opt]
  in Tree.make state "P_App" ~region children

and print_ctor_args :
  'a.'a Tree.printer -> Tree.state -> ('a, comma) nsepseq par reg -> unit =
  fun print state node ->
    Tree.of_nsepseq state "<arguments>" print node.value.inside

(* Attributes patterns *)

and print_P_Attr state (node: attribute * pattern) =
  let attribute, pattern = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_pattern   pattern]
  in Tree.make state "P_Attr" children

(* Bytes as literals in patterns *)

and print_P_Bytes state (node: (lexeme * Hex.t) wrap) =
  Tree.make_bytes "P_Bytes" state node

(* List consing in patterns *)

and print_P_Cons state (node: (pattern * sharp * pattern) reg) =
  let Region.{value; region} = node in
  let head, _, tail = value in
  let children = Tree.[
    mk_child print_pattern head;
    mk_child print_pattern tail]
  in Tree.make state "P_Cons" ~region children

(* Data constructors as patterns *)

and print_P_Ctor state (node: ctor) =
  let region = node#region in
  Tree.(make_unary ~region state "P_Ctor" make_literal node)

(* Integers in patterns *)

and print_P_Int state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "P_Int" state node

(* Module paths in patterns *)

and print_P_ModPath state (node : pattern module_path reg) =
  print_module_path print_pattern "P_ModPath" state node

(* Patterns of lists by extension *)

and print_P_List state (node: pattern compound reg) =
  print_compound print_pattern "P_List" state node

(* Mutez in patterns *)

and print_P_Mutez state (node: (lexeme * Int64.t) wrap) =
  Tree.make_mutez "P_Mutez" state node

(* Natural numbers in patterns *)

and print_P_Nat state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "P_Nat" state node

(* The pattern for the empty list *)

and print_P_Nil state (node: kwd_nil) =
  Tree.make_node ~region:node#region state "P_Nil"

(* Parenthesised patterns *)

and print_P_Par state (node: pattern par reg) =
  Tree.make_unary state "P_Par" print_pattern node.value.inside

(* Record patterns *)

and print_P_Record state (node: record_pattern) =
  print_compound print_field_pattern "P_Record" state node

and print_field_pattern state (node: field_pattern reg) =
  let print_lhs state =
    Tree.make_unary state "<lhs>" print_pattern
  and print_rhs state =
    Tree.make_unary state "<rhs>" print_pattern
  in print_field state ~lhs:print_lhs ~rhs:print_rhs node

and print_field :
  'lhs 'rhs.Tree.state ->
  lhs:'lhs Tree.printer ->
  rhs:'rhs Tree.printer ->
  ('lhs, 'rhs) field reg ->
  unit =
  fun state ~lhs:print_lhs ~rhs:print_rhs {value; region} ->
    match value with
      Punned {pun; attributes} ->
        let children = Tree.mk_child print_lhs pun
                       :: mk_children_attr attributes
        in Tree.make state "<punned field>" ~region children
    | Complete {field_lhs; field_lens; field_rhs; attributes} ->
        let children = Tree.[
          mk_child print_lhs  field_lhs;
          mk_child print_lens field_lens;
          mk_child print_rhs  field_rhs]
        @ mk_children_attr attributes
        in Tree.make state "<field>" ~region children

and print_lens state (node: field_lens) =
  match node with
    Lens_Id   l
  | Lens_Add  l
  | Lens_Sub  l
  | Lens_Mult l
  | Lens_Div  l
  | Lens_Fun  l -> Tree.make_literal state l

(* String literals as patterns *)

and print_P_String state (node: lexeme wrap) =
  Tree.(make_unary state "P_String" make_string node)

(* The pattern matching a tuple *)

and print_P_Tuple state (node: pattern tuple) =
  let Region.{value; region} = node in
  Tree.of_nsepseq state ~region "P_Tuple" print_pattern value.inside

(* Typed pattern *)

and print_P_Typed state (node: typed_pattern reg) =
  let Region.{value; region} = node in
  let {pattern; type_annot} = value in
  let children = Tree.[
    mk_child print_pattern         pattern;
    mk_child print_type_annotation type_annot]
  in Tree.make state "P_Typed" ~region children

(* A pattern variable *)

and print_P_Var state (node: variable) =
  Tree.(make_unary state "P_Var" make_literal node)

(* A verbatim string in patterns *)

and print_P_Verbatim state (node: lexeme wrap) =
  Tree.(make_unary state "P_Verbatim" make_string node)

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_E_Add] comes before [print_E_And]. *)

and print_expr state = function
  E_Add       e -> print_E_Add       state e
| E_And       e -> print_E_And       state e
| E_App       e -> print_E_App       state e
| E_Attr      e -> print_E_Attr      state e
| E_BigMap    e -> print_E_BigMap    state e
| E_Block     e -> print_E_Block     state e
| E_Bytes     e -> print_E_Bytes     state e
| E_Case      e -> print_E_Case      state e
| E_Cat       e -> print_E_Cat       state e
| E_CodeInj   e -> print_E_CodeInj   state e
| E_Ctor      e -> print_E_Ctor      state e
| E_Cond      e -> print_E_Cond      state e
| E_Cons      e -> print_E_Cons      state e
| E_Div       e -> print_E_Div       state e
| E_Equal     e -> print_E_Equal     state e
| E_Fun       e -> print_E_Fun       state e
| E_Geq       e -> print_E_Geq       state e
| E_Gt        e -> print_E_Gt        state e
| E_Int       e -> print_E_Int       state e
| E_Leq       e -> print_E_Leq       state e
| E_List      e -> print_E_List      state e
| E_Lt        e -> print_E_Lt        state e
| E_Map       e -> print_E_Map       state e
| E_MapLookup e -> print_E_MapLookup state e
| E_Mod       e -> print_E_Mod       state e
| E_ModPath   e -> print_E_ModPath   state e
| E_Mult      e -> print_E_Mult      state e
| E_Mutez     e -> print_E_Mutez     state e
| E_Nat       e -> print_E_Nat       state e
| E_Neg       e -> print_E_Neg       state e
| E_Neq       e -> print_E_Neq       state e
| E_Nil       e -> print_E_Nil       state e
| E_Not       e -> print_E_Not       state e
| E_Or        e -> print_E_Or        state e
| E_Par       e -> print_E_Par       state e
| E_Proj      e -> print_E_Proj      state e
| E_Record    e -> print_E_Record    state e
| E_Set       e -> print_E_Set       state e
| E_SetMem    e -> print_E_SetMem    state e
| E_String    e -> print_E_String    state e
| E_Sub       e -> print_E_Sub       state e
| E_Tuple     e -> print_E_Tuple     state e
| E_Typed     e -> print_E_Typed     state e
| E_Update    e -> print_E_Update    state e
| E_Var       e -> print_E_Var       state e
| E_Verbatim  e -> print_E_Verbatim  state e

(* Arithmetic addition *)

and print_E_Add state (node: plus bin_op reg) =
  print_bin_op state "E_Add" node

and print_bin_op state root (node: 'op bin_op reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in Tree.make state root ~region children

(* Boolean conjunction *)

and print_E_And state (node: kwd_and bin_op reg) =
  print_bin_op state "E_And" node

(* Constructor application (or constant constructor) as expressions *)

and print_E_App state (node : call) =
  print_call state "E_App" node

(* Attributed expressions *)

and print_E_Attr state (node: attribute * expr) =
  let attribute, expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_expr      expr]
  in Tree.make state "E_Attr" children

(* Big maps defined intensionally *)

and print_E_BigMap state (node: binding reg compound reg) =
  print_compound (print_binding "<binding>") "E_BigMap" state node

and print_binding root state (node: binding reg) =
  let Region.{value; region} = node in
  let print_key state   = Tree.make_unary state "<key>"   print_expr
  and print_value state = Tree.make_unary state "<value>" print_expr in
  let children = Tree.[
    mk_child print_key   value.key;
    mk_child print_value value.value]
  in Tree.make state root ~region children

(* Block expressions *)

and print_E_Block state (node: block_with reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_block      value.block;
    mk_child print_block_expr value.expr]
  in Tree.make state "E_Block" ~region children

(* Bytes as expressions *)

and print_E_Bytes state (node: (lexeme * Hex.t) wrap) =
  Tree.make_bytes "E_Bytes" state node

(* Case expressions *)

and print_E_Case state (node: expr case reg) =
  print_case print_expr "E_Case" state node

(* String catenation *)

and print_E_Cat state (node: caret bin_op reg) =
  print_bin_op state "E_Cat" node

(* Code Injection *)

and print_E_CodeInj state (node: code_inj reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_language value.language;
    mk_child print_code     value.code]
  in Tree.make state "E_CodeInj" ~region children

and print_language state (node: language) =
  Tree.(make_unary state "<language>" make_node node#payload.value)

and print_code state (node: expr) =
  Tree.make_unary state "<code>" print_expr node

(* Conditional expressions *)

and print_E_Cond state (node: expr conditional reg) =
  print_conditional state "E_Cond"
    ~if_so:print_expr ~if_not:print_expr node

(* Consing (that is, pushing an item on top of a stack/list) *)

and print_E_Cons state (node: sharp bin_op reg) =
  print_bin_op state "E_Cons" node

(* Data constructor as expressions *)

and print_E_Ctor state (node: ctor) =
  let region = node#region in
  Tree.(make_unary ~region state "E_Ctor" make_literal node)

(* The Euclidean quotient *)

and print_E_Div state (node: slash bin_op reg) =
  print_bin_op state "E_Div" node

(* Equality *)

and print_E_Equal state (node: equal bin_op reg) =
  print_bin_op state "E_Equal" node

(* Functional expressions *)

and print_E_Fun state (node: fun_expr reg) =
  let node = node.value in
  let children = Tree.[
    mk_child_opt print_type_params node.type_params;
    mk_child     print_parameters  node.parameters;
    mk_child_opt print_ret_type    node.ret_type;
    mk_child     print_ret_expr    node.return]
  in Tree.make state "E_Fun" children

(* Greater or Equal *)

and print_E_Geq state (node: geq bin_op reg) =
  print_bin_op state "E_Geq" node

(* Greater Than *)

and print_E_Gt state (node: gt bin_op reg) =
  print_bin_op state "E_Gt" node

(* Integer literals as expressions *)

and print_E_Int state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "E_Int" state node

(* Lower or Equal *)

and print_E_Leq state (node: leq bin_op reg) =
  print_bin_op state "E_Leq" node

(* Lists of expressions defined intensionally *)

and print_E_List state (node: expr compound reg) =
  print_compound print_expr "E_List" state node

(* Lower Than *)

and print_E_Lt state (node: lt bin_op reg) =
  print_bin_op state "E_Lt" node

(* Map expressions defined intensionally (that is, by a series of
   bindings from keys to values. *)

and print_E_Map state (node: binding reg compound reg) =
  print_compound (print_binding "<binding>") "E_Map" state node

(* Map lookup as an expression (denoting the key or a failure) *)

and print_E_MapLookup state (node: map_lookup reg) =
  print_map_lookup state "E_MapLookup" node

(* Map lookups *)

and print_map_lookup state root (node: map_lookup reg) =
  let Region.{value; region} = node in
  let print_map state = Tree.make_unary state "<map>" print_expr

  and print_key state (key : expr brackets reg) =
    Tree.make_unary state "<key>" print_expr key.value.inside in

  let print_keys state (keys : expr brackets reg Utils.nseq) =
    Tree.of_nseq state "<keys>" print_key keys in

  let children = Tree.[
    mk_child print_map  value.map;
    mk_child print_keys value.keys]

  in Tree.make state root ~region children

(* Euclidean reminder ("modulo") *)

and print_E_Mod state (node: kwd_mod bin_op reg) =
  print_bin_op state "E_Mod" node

(* Module path as an expression *)

and print_E_ModPath state (node: expr module_path reg) =
  print_module_path print_expr "E_ModPath" state node

(* Arithmetic multiplication *)

and print_E_Mult state (node: times bin_op reg) =
  print_bin_op state "E_Mult" node

(* Literal mutez as expressions *)

and print_E_Mutez state (node: (lexeme * Int64.t) wrap) =
  Tree.make_mutez "E_Mutez" state node

(* Natural numbers as expressions *)

and print_E_Nat state (node: (lexeme * Z.t) wrap) =
  Tree.make_nat "E_Nat" state node

(* Arithmetic negation *)

and print_E_Neg state (node: minus un_op reg) =
  print_un_op state "E_Neg" node

and print_un_op state root (node: 'op un_op reg) =
  let Region.{value; region} = node in
  Tree.make_unary state root ~region print_expr value.arg

(* Not Equal *)

and print_E_Neq state (node: neq bin_op reg) =
  print_bin_op state "E_Neq" node

(* The empty list as a value *)

and print_E_Nil state (node: kwd_nil) =
  Tree.make_node ~region:node#region state "E_Nil"

(* Boolean negation *)

and print_E_Not state (node: kwd_not un_op reg) =
  print_un_op state "E_Not" node

(* Boolean disjunction *)

and print_E_Or state (node: kwd_or bin_op reg) =
  print_bin_op state "E_Or" node

(* Parenthesised expression *)

and print_E_Par state (node: expr par reg) =
  let Region.{value; region} = node in
  Tree.make_unary state "E_Par" ~region print_expr value.inside

(* Projections *)

and print_E_Proj state (node: projection reg) =
  let Region.{value; region} = node in
  Tree.of_nsepseq state ~region "E_Proj" print_selection value.field_path

and print_selection state = function
  FieldName name -> print_FieldName state name
| Component comp -> print_Component state comp

and print_FieldName state (node: field_name) =
  Tree.(make_unary state "FieldName" make_literal node)

and print_Component state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "Component" state node

(* Record expression defined intensionally (that is, by listing all
   the field assignments) *)

and print_E_Record state (node: record_expr) =
  let print = print_field ~lhs:print_expr ~rhs:print_expr
  in print_compound print "E_Record" state node

(* Set expression defined intensionally (that is, by listing all the
   elements) *)

and print_E_Set state (node: expr compound reg) =
  print_compound print_expr "E_Set" state node

(* Set membership *)

and print_E_SetMem state (node: set_membership reg) =
  let Region.{value; region} = node
  and print_set state = Tree.make_unary state "<set>"     print_expr
  and print_elt state = Tree.make_unary state "<element>" print_expr in
  let children = Tree.[
    mk_child print_set value.set;
    mk_child print_elt value.element]
  in Tree.make state "E_SetMem" ~region children

(* String literals as expressions *)

and print_E_String state (node: lexeme wrap) =
  Tree.(make_unary state "E_String" make_string node)

(* Arithmetic subtraction *)

and print_E_Sub state (node: minus bin_op reg) =
  print_bin_op state "E_Sub" node

(* Tuples of expressions *)

and print_E_Tuple state (node: expr tuple) =
  Tree.of_nsepseq state "E_Tuple" print_expr node.value.inside

(* Expressions annotated with a type *)

and print_E_Typed state (node: typed_expr par reg) =
  let Region.{value; region} = node in
  let expr, annotation = value.inside in
  let children = Tree.[
    mk_child print_expr expr;
    mk_child print_type_annotation annotation]
  in Tree.make state "E_Typed" ~region children

(* Functional updates of record expressions *)

and print_E_Update state (node: update reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.structure;
    mk_child print_expr value.update]
  in Tree.make state "E_Update" ~region children

(* Expression variables *)

and print_E_Var state (node: lexeme wrap) =
  Tree.(make_unary state "E_Var" make_literal node)

(* Verbatim strings as expressions *)

and print_E_Verbatim state (node: lexeme wrap) =
  Tree.(make_unary state "E_Verbatim" make_verbatim node)


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
