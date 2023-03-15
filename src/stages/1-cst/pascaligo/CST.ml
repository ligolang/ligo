(* Concrete Syntax Tree (CST) for PascaLIGO *)

(* Disabled warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

open Utils

type lexeme = string

(* Keywords of PascaLIGO *)

(* IMPORTANT: The types are sorted alphabetically, except the generic
   [keyword]. If you add or modify some, please make sure they remain
   in order. *)

type kwd_and       = lexeme wrap
type kwd_begin     = lexeme wrap
type kwd_big_map   = lexeme wrap
type kwd_block     = lexeme wrap
type kwd_case      = lexeme wrap
type kwd_const     = lexeme wrap
type kwd_contains  = lexeme wrap
type kwd_down      = lexeme wrap
type kwd_else      = lexeme wrap
type kwd_end       = lexeme wrap
type kwd_for       = lexeme wrap
type kwd_from      = lexeme wrap
type kwd_function  = lexeme wrap
type kwd_if        = lexeme wrap
type kwd_in        = lexeme wrap
type kwd_is        = lexeme wrap
type kwd_list      = lexeme wrap
type kwd_map       = lexeme wrap
type kwd_mod       = lexeme wrap
type kwd_module    = lexeme wrap
type kwd_nil       = lexeme wrap
type kwd_not       = lexeme wrap
type kwd_of        = lexeme wrap
type kwd_or        = lexeme wrap
type kwd_patch     = lexeme wrap
type kwd_record    = lexeme wrap
type kwd_recursive = lexeme wrap
type kwd_remove    = lexeme wrap
type kwd_set       = lexeme wrap
type kwd_skip      = lexeme wrap
type kwd_step      = lexeme wrap
type kwd_then      = lexeme wrap
type kwd_to        = lexeme wrap
type kwd_type      = lexeme wrap
type kwd_var       = lexeme wrap
type kwd_while     = lexeme wrap
type kwd_with      = lexeme wrap

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow      = lexeme wrap  (* ->  *)
type assign     = lexeme wrap  (* :=  *)
type caret      = lexeme wrap  (* ^   *)
type colon      = lexeme wrap  (* :   *)
type comma      = lexeme wrap  (* ,   *)
type sharp      = lexeme wrap  (* #   *)
type dot        = lexeme wrap  (* .   *)
type equal      = lexeme wrap  (* =   *)
type geq        = lexeme wrap  (* >=  *)
type gt         = lexeme wrap  (* >   *)
type lbrace     = lexeme wrap  (* {   *)
type lbracket   = lexeme wrap  (* [   *)
type leq        = lexeme wrap  (* <=  *)
type lpar       = lexeme wrap  (* (   *)
type lt         = lexeme wrap  (* <   *)
type minus      = lexeme wrap  (* -   *)
type neq        = lexeme wrap  (* =/= *)
type plus       = lexeme wrap  (* +   *)
type rbrace     = lexeme wrap  (* }   *)
type rbracket   = lexeme wrap  (* ]   *)
type rpar       = lexeme wrap  (* )   *)
type semi       = lexeme wrap  (* ;   *)
type slash      = lexeme wrap  (* /   *)
type times      = lexeme wrap  (* *   *)
type vbar       = lexeme wrap  (* |   *)
type plus_eq    = lexeme wrap  (* +=  *)
type minus_eq   = lexeme wrap  (* -=  *)
type times_eq   = lexeme wrap  (* *=  *)
type slash_eq   = lexeme wrap  (* /=  *)
type vbar_eq    = lexeme wrap  (* |=  *)

(* End-of-File *)

type eof = lexeme wrap

(* Literals *)

type variable    = lexeme wrap
type module_name = lexeme wrap
type field_name  = lexeme wrap
type ctor        = lexeme wrap

type attribute   = Attr.t wrap
type language    = lexeme Region.reg wrap

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

(* Brackets *)

type 'a brackets = {
  lbracket : lbracket;
  inside   : 'a;
  rbracket : rbracket
}

(* Chevrons *)

type 'a chevrons = {
  lchevron : lt;
  inside   : 'a;
  rchevron : gt
}

(* CONCRETE SYNTAX TREE (CST) *)

type t = {
  decl : declarations;
  eof  : eof
}

and ast = t

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declarations = declaration nseq

and declaration =
  D_Attr      of (attribute * declaration) reg
| D_Const     of const_decl   reg
| D_Directive of Directive.t
| D_Fun       of fun_decl     reg
| D_Module    of module_decl  reg
| D_Type      of type_decl    reg

(* Constant declaration *)

and const_decl = {
  kwd_const   : kwd_const;
  pattern     : pattern;
  type_params : type_params chevrons reg option;
  const_type  : type_annotation option;
  equal       : equal;
  init        : expr;
  terminator  : semi option
}

and type_params = (variable, comma) nsepseq

and parameters = (param_decl reg, comma) sepseq par reg

and param_decl = {
  param_kind : [`Var of kwd_var | `Const of kwd_const];
  pattern    : pattern;
  param_type : type_annotation option
}

and type_annotation = colon * type_expr

(* Function declaration *)

and fun_decl = {
  kwd_recursive : kwd_recursive option;
  kwd_function  : kwd_function;
  fun_name      : variable;
  type_params   : type_params chevrons reg option;
  parameters    : parameters;
  ret_type      : type_annotation option;
  kwd_is        : kwd_is;
  return        : expr;
  terminator    : semi option
}

(* Module declaration *)

and module_decl = {
  kwd_module   : kwd_module;
  name         : module_name;
  kwd_is       : kwd_is;
  module_expr  : module_expr;
  terminator   : semi option
}

and module_expr =
  M_Body of module_body reg
| M_Path of module_name module_path reg
| M_Var  of module_name

and module_body = {
  enclosing    : block_enclosing;
  declarations : declarations
}

(* Type declaration *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : variable;
  params     : variable tuple option;
  kwd_is     : kwd_is;
  type_expr  : type_expr;
  terminator : semi option
}

and 'a tuple = ('a, comma) nsepseq par reg

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App     of (type_expr * type_tuple) reg        (*     M.t (x,y,z) *)
| T_Attr    of (attribute * type_expr)             (*          [@a] x *)
| T_Cart    of cartesian                           (*     x * (y * z) *)
| T_Fun     of (type_expr * arrow * type_expr) reg (*          x -> y *)
| T_Int     of (lexeme * Z.t) wrap                 (*              42 *)
| T_ModPath of type_expr module_path reg           (*     A.B.(x * y) *)
| T_Par     of type_expr par reg                   (*        (x -> y) *)
| T_Record  of field_decl reg compound reg (* record [a; [@a1] b : t] *)
| T_String  of lexeme wrap                         (*           "foo" *)
| T_Sum     of sum_type reg                        (* [@a] A | B of t *)
| T_Var     of variable                            (*               t *)

(* Application of type constructors *)

and type_tuple = type_expr tuple

(* Cartesian type *)

and cartesian = (type_expr * times * (type_expr,times) nsepseq) reg

(* Module paths *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* Compound constructs (lists, sets, records, maps) *)

and 'a compound = {
  kind       : lexeme wrap;
  opening    : lbracket;
  elements   : ('a, semi) sepseq;
  terminator : semi option;
  closing    : rbracket
}

(* Record types *)

and field_decl = {
  field_name : field_name;
  field_type : type_annotation option; (* Type punning if [None] *)
  attributes : attribute list
}

(* Sum types *)

and sum_type = {
  lead_vbar : vbar option;
  variants  : (variant reg, vbar) nsepseq
}

and variant = {
  ctor       : ctor;
  ctor_args  : (kwd_of * type_expr) option;
  attributes : attribute list
}

(* STATEMENTS *)

and statements = (statement, semi) nsepseq

and statement =
  S_Attr    of (attribute * statement)
| S_Decl    of declaration
| S_Instr   of instruction
| S_VarDecl of var_decl reg

(* Variable declaration (invalid at the top-level) *)

and var_decl = {
  kwd_var     : kwd_var;
  pattern     : pattern;
  type_params : type_params chevrons reg option;
  var_type    : type_annotation option;
  assign      : assign;  (* := *)
  init        : expr;
  terminator  : semi option
}

(* INSTRUCTIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and instruction =
  I_Assign of assignment reg
| I_Call   of call
| I_Case   of test_clause case reg
| I_Cond   of test_clause conditional reg
| I_For    of for_int reg
| I_ForIn  of for_in
| I_Patch  of patch reg
| I_Remove of removal reg
| I_Skip   of kwd_skip
| I_While  of while_loop reg

(* Assignment *)

and assignment = {
  lhs    : expr;
  assign : assign;
  rhs    : expr
}

(* Procedure/function call and application of data constructor *)

and call = (expr * call_args) reg

and call_args = (expr, comma) sepseq par reg

(* Case *)

and 'a case = {
  kwd_case  : kwd_case;
  expr      : expr;
  kwd_of    : kwd_of;
  opening   : lbracket;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq;
  closing   : rbracket
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and test_clause =
  ClauseInstr of instruction
| ClauseBlock of block reg

(* Blocks *)

and block = {
  enclosing  : block_enclosing;
  statements : statements;
  terminator : semi option
}

and block_enclosing =
  Braces   of kwd_block option * lbrace * rbrace
| BeginEnd of kwd_begin * kwd_end

(* General conditionals *)

and 'branch conditional = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  if_so    : 'branch;
  if_not   : (kwd_else * 'branch) option
}

(* Iteration over integer intervals *)

and for_int = {
  kwd_for : kwd_for;
  index   : variable;
  assign  : assign;
  init    : expr;
  kwd_to  : kwd_to;
  bound   : expr;
  step    : (kwd_step * expr) option; (* [1] if [None] *)
  block   : block reg
}

(* Iteration over maps, sets and lists *)

and for_in =
  ForMap       of for_map reg
| ForSetOrList of for_set_or_list reg

and for_map = {
  kwd_for    : kwd_for;
  binding    : variable * arrow * variable;
  kwd_in     : kwd_in;
  kwd_map    : kwd_map; (* Temporary *)
  collection : expr;
  block      : block reg
}

and for_set_or_list = {
  kwd_for    : kwd_for;
  var        : variable;
  kwd_in     : kwd_in;
  for_kind   : [`Set of kwd_set | `List of kwd_list]; (* Temporary *)
  collection : expr;
  block      : block reg
}

(* Patches for maps, records, and sets. *)

and patch = {
  kwd_patch  : kwd_patch;
  collection : expr;
  kwd_with   : kwd_with;
  patch_kind : patch_kind;
  patch      : expr
}

and patch_kind = [
  `Map    of kwd_map
| `Record of kwd_record
| `Set    of kwd_set
]

(* Removal from sets and maps *)

and removal = {
  kwd_remove  : kwd_remove;
  item        : expr;
  kwd_from    : kwd_from;
  remove_kind : [`Set of kwd_set | `Map of kwd_map];
  collection  : expr
}

(* General loop *)

and while_loop = {
  kwd_while : kwd_while;
  cond      : expr;
  block     : block reg
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_App      of (pattern * pattern tuple option) reg (*    C (x,y) *)
| P_Attr     of (attribute * pattern)                (*   [@var] x *)
| P_Bytes    of (lexeme * Hex.t) wrap                (*     0xFFFA *)
| P_Cons     of (pattern * sharp * pattern) reg      (*      x # y *)
| P_Ctor     of ctor                                 (*          C *)
| P_Int      of (lexeme * Z.t) wrap                  (*         42 *)
| P_List     of pattern compound reg                 (* list [4;x] *)
| P_ModPath  of pattern module_path reg              (*      M.N.x *)
| P_Mutez    of (lexeme * Int64.t) wrap              (*     5mutez *)
| P_Nat      of (lexeme * Z.t) wrap                  (*         4n *)
| P_Nil      of kwd_nil                              (*        nil *)
| P_Par      of pattern par reg                      (*     (C, 4) *)
| P_Record   of record_pattern                  (* record [x=y; z] *)
| P_String   of lexeme wrap                          (*   "string" *)
| P_Tuple    of pattern tuple                        (*     (1, x) *)
| P_Typed    of typed_pattern reg                    (*  (x : int) *)
| P_Var      of variable                             (*          x *)
| P_Verbatim of lexeme wrap                          (*    {|foo|} *)

(* Record pattern *)

and record_pattern = field_pattern reg compound reg

and field_pattern = (pattern, pattern) field

(* Record fields *)

and ('lhs, 'rhs) field =
  Punned   of 'lhs punned
| Complete of ('lhs, 'rhs) full_field

and 'lhs punned = {
  pun        : 'lhs;
  attributes : attribute list
}

and ('lhs, 'rhs) full_field = {
  field_lhs  : 'lhs;
  field_lens : field_lens;
  field_rhs  : 'rhs;
  attributes : attribute list
}

and field_lens =
  Lens_Id   of assign
| Lens_Add  of plus_eq
| Lens_Sub  of minus_eq
| Lens_Mult of times_eq
| Lens_Div  of slash_eq
| Lens_Fun  of vbar_eq

(* Typed pattern *)

and typed_pattern = {
  pattern    : pattern;
  type_annot : type_annotation
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add       of plus bin_op reg                (* x + y           *)
| E_And       of kwd_and bin_op reg             (* x and y         *)
| E_App       of call                           (* Foo (x,y)       *)
| E_Attr      of (attribute * expr)             (* [@a] (x,y)      *)
| E_BigMap    of binding reg compound reg
| E_Block     of block_with reg
| E_Bytes     of (lexeme * Hex.t) wrap          (* 0xFFFA          *)
| E_Case      of expr case reg
| E_Cat       of caret bin_op reg               (* "Hello" ^ world *)
| E_CodeInj   of code_inj reg
| E_Ctor      of ctor                           (* C               *)
| E_Cond      of expr conditional reg
| E_Cons      of sharp bin_op reg               (* head :: tail    *)
| E_Div       of slash bin_op reg               (* x / y           *)
| E_Equal     of equal bin_op reg               (* x = y           *)
| E_Fun       of fun_expr reg                   (* fun x -> x      *)
| E_Geq       of geq bin_op reg                 (* x >= y          *)
| E_Gt        of gt bin_op reg                  (* x > y           *)
| E_Int       of (lexeme * Z.t) wrap            (* 42              *)
| E_Leq       of leq bin_op reg                 (* x <= y          *)
| E_List      of expr compound reg              (* list [4;5]      *)
| E_Lt        of lt bin_op reg                  (* x < y           *)
| E_Map       of binding reg compound reg       (* map [3 -> "x"]  *)
| E_MapLookup of map_lookup reg                 (* M.m [i]         *)
| E_Mod       of kwd_mod bin_op reg             (* x mod n         *)
| E_ModPath   of expr module_path reg           (* M.N.x           *)
| E_Mult      of times bin_op reg               (* x * y           *)
| E_Mutez     of (lexeme * Int64.t) wrap        (* 5mutez          *)
| E_Nat       of (lexeme * Z.t) wrap            (* 4n              *)
| E_Neg       of minus un_op reg                (* -a              *)
| E_Neq       of neq bin_op reg                 (* x =/= y         *)
| E_Nil       of kwd_nil                        (* nil             *)
| E_Not       of kwd_not un_op reg              (* not x           *)
| E_Or        of kwd_or bin_op reg              (* x or y          *)
| E_Par       of expr par reg                   (* (x - M.y)       *)
| E_Proj      of projection reg                 (* e.x.1           *)
| E_Record    of record_expr                    (* record [x=7]    *)
| E_Set       of expr compound reg              (* set [x; 1]      *)
| E_SetMem    of set_membership reg             (* x contains y    *)
| E_String    of lexeme wrap                    (* "string"        *)
| E_Sub       of minus bin_op reg               (* a - b           *)
| E_Tuple     of expr tuple                     (* (1, x)          *)
| E_Typed     of typed_expr par reg             (* (x : int)       *)
| E_Update    of update reg                     (* x with y        *)
| E_Var       of variable                       (* x               *)
| E_Verbatim  of lexeme wrap                    (* {|foo|}         *)

(* Map binding *)

and binding = {
  key   : expr;
  arrow : arrow;
  value : expr
}

(* Block as expression *)

and block_with = {
  block    : block reg;
  kwd_with : kwd_with;
  expr     : expr
}

(* Projection *)

and projection = {
  record_or_tuple : expr;
  selector        : dot;
  field_path      : (selection, dot) nsepseq
}

and selection =
  FieldName of field_name
| Component of (lexeme * Z.t) wrap

(* Binary and unary arithmetic operators *)

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
 language : language;
 code     : expr;
 rbracket : rbracket
}

(* Functional expression *)

and fun_expr = {
  kwd_function : kwd_function;
  type_params  : type_params chevrons reg option;
  parameters   : parameters;
  ret_type     : type_annotation option;
  kwd_is       : kwd_is;
  return       : expr
}

(* Map lookup *)

and map_lookup = {
  map  : expr;
  keys : expr brackets reg nseq
}

(* Record expression *)

and record_expr = (expr, expr) field reg compound reg

(* Set membership *)

and set_membership = {
  set          : expr;
  kwd_contains : kwd_contains;
  element      : expr
}

(* Typed expression *)

and typed_expr = expr * type_annotation

(* Functional updates *)

and update = {
  structure : expr;
  kwd_with  : kwd_with;
  update    : expr
}

(* PROJECTING REGIONS *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last (to_region <@ snd) tl)

let sepseq_to_region to_region = function
      None -> Region.ghost
| Some seq -> nsepseq_to_region to_region seq

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec type_expr_to_region = function
  T_App     {region; _} -> region
| T_Attr    (_,t) -> type_expr_to_region t
| T_Cart    {region; _}
| T_Fun     {region; _} -> region
| T_Int     t -> t#region
| T_ModPath {region; _}
| T_Par     {region; _}
| T_Record  {region; _} -> region
| T_String  t -> t#region
| T_Sum     {region; _} -> region
| T_Var     t -> t#region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec expr_to_region = function
  E_Add       {region; _}
| E_And       {region; _}
| E_App       {region; _} -> region
| E_Attr      (_,e) -> expr_to_region e
| E_BigMap    {region; _}
| E_Block     {region; _} -> region
| E_Bytes     t -> t#region
| E_Case      {region; _}
| E_Cat       {region; _}
| E_CodeInj   {region; _} -> region
| E_Ctor      t -> t#region
| E_Equal     {region; _}
| E_Cond      {region; _}
| E_Cons      {region; _}
| E_Div       {region; _}
| E_Fun       {region; _}
| E_Geq       {region; _}
| E_Gt        {region; _} -> region
| E_Int       t -> t#region
| E_Leq       {region; _}
| E_List      {region; _}
| E_Lt        {region; _}
| E_Map       {region; _}
| E_MapLookup {region; _}
| E_Mod       {region; _}
| E_ModPath   {region; _}
| E_Mult      {region; _} -> region
| E_Mutez     t -> t#region
| E_Nat       t -> t#region
| E_Neg       {region; _}
| E_Neq       {region; _} -> region
| E_Nil       t -> t#region
| E_Not       {region; _}
| E_Or        {region; _}
| E_Par       {region; _}
| E_Proj      {region; _}
| E_Record    {region; _}
| E_Set       {region; _}
| E_SetMem    {region; _} -> region
| E_String    t -> t#region
| E_Sub       {region; _}
| E_Tuple     {region; _}
| E_Typed     {region; _}
| E_Update    {region; _} -> region
| E_Var       t
| E_Verbatim  t -> t#region

and module_expr_to_region = function
  M_Body {region; _}
| M_Path {region; _} -> region
| M_Var e -> e#region

and typed_expr_to_region x = x.Region.region

and record_expr_to_region x = x.Region.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let instr_to_region = function
  I_Assign {region; _}
| I_Call   {region; _}
| I_Case   {region; _}
| I_Cond   {region; _}
| I_For    {region; _}
| I_ForIn  ForMap {region; _}
| I_ForIn  ForSetOrList {region; _}
| I_Patch  {region; _}
| I_Remove {region; _} -> region
| I_Skip   t -> t#region
| I_While  {region; _} -> region

let decl_to_region = function
  D_Attr   {region; _}
| D_Const  {region; _} -> region
| D_Directive dir -> Directive.to_region dir
| D_Fun    {region; _}
| D_Module {region; _}
| D_Type   {region; _} -> region

let test_clause_to_region = function
  ClauseInstr instr -> instr_to_region instr
| ClauseBlock block -> block.Region.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec pattern_to_region = function
  P_App     {region; _} -> region
| P_Attr    (_,p) -> pattern_to_region p
| P_Bytes   t -> t#region
| P_Cons    {region; _} -> region
| P_Ctor    t -> t#region
| P_Int     t -> t#region
| P_List    {region; _}
| P_ModPath {region; _} -> region
| P_Mutez   t -> t#region
| P_Nat     t -> t#region
| P_Nil     t -> t#region
| P_Par     {region; _}
| P_Record  {region; _} -> region
| P_String  t -> t#region
| P_Tuple   {region; _}
| P_Typed   {region; _} -> region
| P_Var     t
| P_Verbatim t -> t#region

let selection_to_region = function
  FieldName name -> name#region
| Component w -> w#region

let field_lens_to_region = function
  Lens_Id x   -> x#region
| Lens_Add x  -> x#region
| Lens_Sub x  -> x#region
| Lens_Mult x -> x#region
| Lens_Div x  -> x#region
| Lens_Fun x  -> x#region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec statement_to_region = function
  S_Attr    s -> region_of_S_Attr s
| S_Decl    s -> region_of_S_Decl s
| S_Instr   s -> region_of_S_Instr s
| S_VarDecl s -> region_of_S_VarDecl s

and region_of_S_Attr (node: attribute * statement) =
  statement_to_region (snd node)

and region_of_S_Decl (node: declaration) =
  decl_to_region node

and region_of_S_Instr (node: instruction) =
  instr_to_region node

and region_of_S_VarDecl (node: var_decl reg) =
  node.region
