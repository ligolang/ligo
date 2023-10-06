(* Concrete Syntax Tree (CST) for PyLIGO *)

(* Disabled warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

module Types = Cst_shared.Types

(* Vendor dependencies *)

module Directive = Types.Directive
module Utils     = Types.Utils
module Region    = Types.Region

(* Local dependencies *)

module Wrap = Types.Wrap
module Attr = Types.Attr

(* Utilities *)

type 'a reg = 'a Types.reg
type 'payload wrap = 'payload Types.wrap

open Utils

(* Lexemes *)

type lexeme = Types.lexeme

(* Keywords of PyLIGO *)

open Types

(* IMPORTANT: The types are sorted alphabetically, except the generic
   [keyword]. If you add or modify some, please make sure they remain
   in order. *)

type kwd_and      = lexeme wrap
(*
type kwd_as       = lexeme wrap *)
type kwd_assert   = lexeme wrap
(*
type kwd_async    = lexeme wrap
type kwd_await    = lexeme wrap
type kwd_break    = lexeme wrap *)
type kwd_case     = lexeme wrap
type kwd_class    = lexeme wrap
type kwd_const    = lexeme wrap
(*
type kwd_continue = lexeme wrap *)
type kwd_def      = lexeme wrap
(*
type kwd_del      = lexeme wrap *)
type kwd_elif     = lexeme wrap
type kwd_else     = lexeme wrap
(*
type kwd_except   = lexeme wrap
type kwd_finally  = lexeme wrap *)
type kwd_for      = lexeme wrap
(*
type kwd_from     = lexeme wrap
type kwd_global   = lexeme wrap *)
type kwd_if       = lexeme wrap
type kwd_in       = lexeme wrap
(*
type kwd_is       = lexeme wrap
type kwd_import   = lexeme wrap *)
type kwd_lambda   = lexeme wrap
type kwd_match    = lexeme wrap
type kwd_module   = lexeme wrap
(*
type kwd_nonlocal = lexeme wrap *)
type kwd_not      = lexeme wrap
type kwd_or       = lexeme wrap
type kwd_pass     = lexeme wrap
(*
type kwd_raise    = lexeme wrap *)
type kwd_return   = lexeme wrap
(*
type kwd_try      = lexeme wrap *)
type kwd_type     = lexeme wrap
type kwd_var      = lexeme wrap
type kwd_while    = lexeme wrap
(*
type kwd_with     = lexeme wrap
type kwd_yield    = lexeme wrap *)

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow      = lexeme wrap  (* ->  *)
type caret      = lexeme wrap  (* ^   *)
type minus      = lexeme wrap  (* -   *)
type plus       = lexeme wrap  (* +   *)
type times      = lexeme wrap  (* *   *)
type exp        = lexeme wrap  (* **  *)
type slash2     = lexeme wrap  (* //  *)
type percent    = lexeme wrap  (* %   *)
type lpar       = lexeme wrap  (* (   *)
type rpar       = lexeme wrap  (* )   *)
type lbracket   = lexeme wrap  (* [   *)
type rbracket   = lexeme wrap  (* ]   *)
type lbrace     = lexeme wrap  (* {   *)
type rbrace     = lexeme wrap  (* }   *)
type comma      = lexeme wrap  (* ,   *)
type semi       = lexeme wrap  (* ;   *)
type vbar       = lexeme wrap  (* |   *)
type ampersand  = lexeme wrap  (* &   *)
type colon      = lexeme wrap  (* :   *)
type dot        = lexeme wrap  (* .   *)
type tilde      = lexeme wrap  (* ~   *)
type assign     = lexeme wrap  (* :=  *)
type lshift     = lexeme wrap  (* <<  *)
type rshift     = lexeme wrap  (* >>  *)
type eq         = lexeme wrap  (* =   *)
type eq2        = lexeme wrap  (* ==  *)
type ne         = lexeme wrap  (* !=  *)
type lt         = lexeme wrap  (* <   *)
type gt         = lexeme wrap  (* >   *)
type le         = lexeme wrap  (* <=  *)
type ge         = lexeme wrap  (* >=  *)
type plus_eq    = lexeme wrap  (* +=  *)
type minus_eq   = lexeme wrap  (* -=  *)
type times_eq   = lexeme wrap  (* *=  *)
type at_eq      = lexeme wrap  (* @=  *)
type slash2_eq  = lexeme wrap  (* //= *)
type percent_eq = lexeme wrap  (* %=  *)
type and_eq     = lexeme wrap  (* &=  *)
type or_eq      = lexeme wrap  (* |=  *)
type xor_eq     = lexeme wrap  (* ^=  *)
type rshift_eq  = lexeme wrap  (* >>= *)
type lshift_eq  = lexeme wrap  (* <<= *)
type exp_eq     = lexeme wrap  (* **= *)
type wild       = lexeme wrap  (* _   *)
type grave      = lexeme wrap  (* `   *)

(* End-of-File *)

type eof = lexeme wrap

(* Literals *)

type variable =
  Var of lexeme wrap (* foo  *)
| Esc of lexeme wrap (* @foo without the @ *)

type fun_name    = variable
type member_name = variable
type type_name   = variable
type module_name = variable

type class_name  = lexeme wrap
type ctor        = lexeme wrap
type language    = lexeme reg   (* Not [wrap] *)

type attribute   = Attr.t reg

type string_literal   = lexeme wrap
type int_literal      = (lexeme * Z.t) wrap
type nat_literal      = int_literal
type bytes_literal    = (lexeme * Hex.t) wrap
type mutez_literal    = (lexeme * Int64.t) wrap
type verbatim_literal = lexeme wrap

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

(* Braces *)

type 'a braces =  {
  lbrace : lbrace;
  inside : 'a;
  rbrace : rbrace;
}

(* CONCRETE SYNTAX TREE (CST) *)

type t = {
  decl : declarations;
  eof  : eof
}

and cst = t

(* STATEMENTS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declarations = declaration nseq

and declaration =
  D_Attr      of (attribute * declaration)
| D_Class     of class_decl reg
| D_Const     of const_decl reg
| D_Directive of Directive.t
| D_Fun       of fun_decl reg
| D_Type      of type_decl

(* Type declaration *)

and type_decl =
  ClassEq   of class_eq   reg
| TypeAlias of type_alias reg

and class_eq = {
  class_name   : class_name;
  superclasses : superclasses option;
  equal        : eq;
  class_type   : type_expr
}

and superclasses = (superclass reg, comma) nsepseq par reg

and superclass = {
  class_name   : class_name;
  class_params : class_params option
}

and class_params = (class_name, comma) nsepseq brackets reg

and type_alias = {
  kwd_type  : kwd_type;
  alias     : type_name;
  equal     : eq;
  type_expr : type_expr
}

(* Constant declarations *)

and const_decl = {
  kwd_const  : kwd_const;
  variable   : variable;
  const_type : type_annotation option;
  equal      : eq;
  init       : expr
}

(* Top-level function declarations *)

and fun_decl = {
  kwd_def      : kwd_def;
  fun_name     : fun_name;
  class_params : class_params option;
  fun_params   : (param_decl reg, comma) sepseq par reg;
  return_type  : (arrow * type_expr) option;
  colon        : colon;
  block        : block reg
}

and param_decl = {
  kwd_const  : kwd_const option;
  parameter  : variable;
  param_type : type_annotation option
}

and type_annotation = colon * type_expr

and block =
  Statements  of statements
| SimpleStmts of (simple_stmt,semi) nsepseq

(* Statements *)

and statements = {
  virt_begin : Region.t;
  statements : statement nseq reg;
  virt_end   : Region.t
}

and statement = (* Indented *)
  attribute list * local_stmt

and local_stmt =
  SimpleStmt   of simple_stmt
| CompoundStmt of compound_stmt

(* Simple statements *)

and simple_stmt =
  S_Assign      of assignment reg
| S_Return      of return_stmt reg
| S_ModuleAlias of module_alias reg
| S_Assert      of assert_stmt reg
| S_ProcCall    of call
| S_Pass        of kwd_pass

(* Assignments *)

and assignment = {
  lhs_assign : (expr, comma) nsepseq; (* Map lookups & members *)
  assign_sym : assign_sym;
  rhs_assign : (expr, comma) nsepseq
}

and assign_sym =
  Equal     of eq
| PlusEq    of plus_eq
| MinusEq   of minus_eq
| MultEq    of times_eq
| ModEq     of percent_eq
| IntDivEq  of slash2_eq
| AndEq     of and_eq
| OrEq      of or_eq
| XorEq     of xor_eq
| RShiftEq  of rshift_eq
| LShiftEq  of lshift_eq
| ExpEq     of exp_eq

(* Return statement *)

and return_stmt = {
  kwd_return : kwd_return;
  returned   : expr option
}

(* Module aliasing statement *)

and module_alias = {
  kwd_module   : kwd_module;
  name         : module_name;
  equal        : eq;
  module_expr  : module_name
}

(* Module paths *)

and 'a module_path = {
  module_path : module_name;
  selector    : dot;
  field       : 'a
}

(* Assertions *)

and assert_stmt = {
  kwd_assert : kwd_assert;
  condition  : expr;
  fatal_err  : (comma * expr) option
}

(* Procedure call *)

and call = (expr * call_args) reg

and call_args = (expr, comma) sepseq par reg

(* Class declarations *)

and class_decl = {
  kwd_class    : kwd_class;
  name         : class_name;
  superclasses : superclasses option;
  colon        : colon;
  class_body   : member_decls;
}

and member_decls = (attribute list * member_decl) nseq

and member_decl =
  MemberFun  of member_fun_decl reg
| MemberVars of member_vars_decl reg

and member_fun_decl = {
  kwd_def           : kwd_def;
  fun_name          : fun_name;
  member_fun_params : (member_vars_decl reg, comma) sepseq par reg;
  return_type       : (arrow * type_expr) option;
  colon             : colon;
  block             : block reg
}

and member_vars_decl = {
  kind      : kwd_const option;
  vars      : (variable, comma) nsepseq;
  vars_type : type_annotation
}

and compound_stmt =
  C_Type  of type_decl
| C_Const of const_decl reg
| C_Fun   of fun_decl   reg
| C_Var   of var_decl   reg
| C_Class of class_decl reg
| C_If    of if_stmt    reg
| C_For   of for_stmt   reg
| C_While of while_stmt reg
| C_Match of match_stmt reg

(* Variable declarations *)

and var_decl = {
  kwd_var  : kwd_var option;
  variable : variable;
  var_type : type_annotation option;
  equal    : eq;
  init     : expr
}

(* Conditional statement *)

and if_stmt = {
  kwd_if : kwd_if;
  test   : expr;
  colon  : colon;
  if_so  : block reg;
  if_not : else_or_elif option
}

and else_or_elif =
  Elif of elif_stmt  reg
| Else of else_block reg

and elif_stmt = {
  kwd_elif : kwd_elif;
  test     : expr;
  colon    : colon;
  if_so    : block reg;
  if_not   : else_or_elif option
}

and else_block = {
  kwd_else : kwd_else;
  colon    : colon;
  block    : block reg
}

(* Bounded Iterations (a.k.a. "for" loops) *)

and for_stmt = {
  kwd_for : kwd_for;
  indices : indices;
  kwd_in  : kwd_in;
  expr    : expr;
  colon   : colon;
  block   : block reg
}

and indices = (variable, comma) nsepseq

(* Unbounded Iterations (a.k.a. "while" loops *)

and while_stmt = {
  kwd_while : kwd_while;
  cond      : expr;
  colon     : colon;
  block     : block reg
}

(* Pattern matching statements *)

and match_stmt = {
  kwd_match : kwd_match;
  subject   : (expr, comma) nsepseq;
  colon     : colon;
  cases     : case_stmt nseq
}

and case_stmt = {
  kwd_case : kwd_case;
  pattern  : pattern;
  colon    : colon;
  block    : block reg
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Attr     of (attribute * pattern)    (*   [@var] x *)
| P_Bytes    of bytes_literal            (*     0xFFFA *)
| P_Ctor     of pattern ctor_call reg    (*   C(x,y=_) *)
| P_Int      of int_literal              (*         42 *)
| P_ModPath  of pattern module_path reg  (*      m.n.x *)
| P_Mutez    of mutez_literal            (*     5mutez *)
| P_Nat      of nat_literal              (*         4n *)
| P_Par      of pattern par reg          (*    (`C, 4) *)
| P_String   of string_literal           (*   "string" *)
| P_Tuple    of pattern tuple            (*     (1, x) *)
| P_Typed    of typed_pattern reg        (*  (x : int) *)
| P_Var      of variable                 (*   @x     x *)
| P_Variant  of pattern ctor_call reg    (*      `C(1) *)
| P_Verbatim of verbatim_literal         (*    {|foo|} *)

(* Tuples *)

and 'a tuple = ('a, comma) nsepseq par reg

(* Constructor call pattern *)

and 'rhs ctor_call = {
  class_path : qualified_class;
  class_args : type_tuple option;
  class_body : ('rhs member, comma) sepseq par reg
}

and qualified_class =
  ClassName of class_name
| ClassPath of class_name module_path reg

and type_tuple = (type_expr, comma) nsepseq brackets reg

and 'rhs member =
  Named of 'rhs full_member
| Value of 'rhs

and 'rhs full_member = {
  member_lhs : member_name;
  equal      : eq;
  member_rhs : 'rhs
}

(* Typed pattern *)

and typed_pattern = {
  pattern    : pattern;
  type_annot : type_annotation
}

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_Attr    of (attribute * type_expr)    (*       [@a] t *)
| T_Class   of class_type reg             (* m.C [int, D] *)
| T_Cart    of cartesian reg              (*  x * (y * z) *)
| T_Fun     of fun_type reg               (*       x -> y *)
| T_Int     of int_literal                (*           42 *)
| T_ModPath of type_expr module_path reg  (*  a.b.(x * y) *)
| T_Par     of type_expr par reg          (*          (t) *)
| T_String  of string_literal             (*          "x" *)
| T_Sum     of sum_type reg               (*     A | B[t] *)
| T_Tuple   of type_expr tuple            (*   (x, (y, z) *)
| T_Var     of variable                   (*        @x  x *)

(* Functional types *)

and fun_type = type_expr * arrow * type_expr

(* Class types *)

and class_type = {
  class_path : qualified_class;
  class_args : type_tuple option
}

(* Cartesian types *)

and cartesian = type_expr * times * (type_expr, times) nsepseq

(* Sum types *)

and sum_type = (variant reg, vbar) nsepseq

and variant = attribute list * class_type reg

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add        of plus bin_op reg       (* x + y                *)
| E_And        of kwd_and bin_op reg    (* x and y              *)
| E_Attr       of (attribute * expr)    (* [@a] (x,y)           *)
| E_BitAnd     of ampersand bin_op reg  (* x & y                *)
| E_BitNot     of tilde un_op reg       (* ~x                   *)
| E_BitOr      of vbar bin_op reg       (* x | y                *)
| E_BitXor     of caret bin_op reg      (* x ^ y                *)
| E_BitLShift  of lshift bin_op reg     (* x << 4               *)
| E_BitRShift  of rshift bin_op reg     (* x >> 4               *)
| E_Bytes      of bytes_literal         (* 0xFFFA               *)
| E_Call       of call                  (* m.f (x,y)            *)
| E_CodeInj    of code_inj reg
| E_Ctor       of expr ctor_call reg    (* Foo(Incr(x))         *)
| E_Equal      of eq2 bin_op reg        (* x == y               *)
| E_Exp        of exp bin_op reg        (* x**2                 *)
| E_Fun        of fun_expr reg          (* lambda x: x          *)
| E_Geq        of ge bin_op reg         (* x >= y               *)
| E_Gt         of gt bin_op reg         (* x > y                *)
| E_IfElse     of if_else reg           (* x if b else y        *)
| E_Int        of int_literal           (* 42                   *)
| E_IntDiv     of slash2 bin_op reg     (* 16 // 3              *)
| E_Leq        of le bin_op reg         (* x <= y               *)
| E_List       of list_expr             (* (4,5,4)              *)
| E_ListCompr  of list_comprehension    (* [x for x in e]       *)
| E_Lt         of lt bin_op reg         (* x < y                *)
| E_Map        of map_expr              (* {"a": 1}             *)
| E_MapCompr   of map_comprehension     (* {k:v for (k,v) in e} *)
| E_MapLookup  of map_lookup reg        (* m.n["a"][j]          *)
| E_Match      of match_expr reg        (* match e:             *)
| E_Mod        of percent bin_op reg    (* x % n                *)
| E_ModPath    of expr module_path reg  (* m.n.x                *)
| E_Mult       of times bin_op reg      (* x * y                *)
| E_Mutez      of mutez_literal         (* 5mutez               *)
| E_Nat        of nat_literal           (* 4n                   *)
| E_Neg        of minus un_op reg       (* -a                   *)
| E_Neq        of ne bin_op reg         (* x != y               *)
| E_Not        of kwd_not un_op reg     (* not x                *)
| E_Or         of kwd_or bin_op reg     (* x or y               *)
| E_Par        of expr par reg          (* (x - m.y)            *)
| E_Proj       of projection reg        (* e.x.1                *)
| E_Set        of set_expr              (* {x, 1}               *)
| E_SetCompr   of set_comprehension     (* {x for x in e}       *)
| E_String     of string_literal        (* "string"             *)
| E_Sub        of minus bin_op reg      (* a - b                *)
| E_Tuple      of expr tuple            (* (1, x)               *)
| E_Typed      of typed_expr par reg    (* (x : int)            *)
| E_Var        of variable              (* x  @x                *)
| E_Variant    of expr ctor_call reg    (* `Foo(Incr(x))        *)
| E_Verbatim   of verbatim_literal      (* {|foo|}              *)

(* List comprehension *)

and list_comprehension = comprehension_expr reg brackets reg

and comprehension_expr = {
  subject   : expr;
  kwd_for   : kwd_for;
  indices   : indices;
  kwd_in    : kwd_in;
  iterated  : expr;
  predicate : predicate reg option
}

and predicate = {
  kwd_if    : kwd_if;
  condition : expr
}

(* Pattern match expressions *)

and match_expr = {
  kwd_match : kwd_match;
  subject   : (expr, comma) nsepseq;
  colon     : colon;
  cases     : case_expr nseq
}

and case_expr = {
  kwd_case : kwd_case;
  pattern  : pattern;
  colon    : colon;
  expr     : expr
}

(* Map lookup *)

and map_lookup = {
  map  : expr;
  keys : expr brackets reg nseq
}

(* Projection *)

and projection = {
  object_or_tuple : expr;
  selector        : dot;
  path            : (selection, dot) nsepseq
}

and selection =
  MemberName of member_name
| Component  of int_literal

(* Typed expression *)

and typed_expr = expr * type_annotation

(* Unary and binary operators *)

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

(* Code injection *)

and code_inj = {
  language : string reg reg;
  code     : expr;
  rbracket : rbracket
}

(* If-Else expression *)

and if_else = {
  if_so    : expr;
  kwd_if   : kwd_if;
  subject  : expr;
  kwd_else : kwd_else;
  if_not   : expr
}

(* Functional expressions (lambdas) *)

and fun_expr = {
  kwd_lambda    : kwd_lambda;
  lambda_params : lambda_params option;
  colon         : colon;
  lambda_body   : expr
}

and lambda_params = (lambda_param, comma) nsepseq

and lambda_param =
  LambdaTypedParam of param_decl reg par reg
| LambdaParam      of variable

(* Typed expressions (i.e. annotated by a type expression) *)

and annot_expr = expr * colon * type_expr

(* Access to a value or type in a qualified module *)

and 'a module_access = {
  module_name : module_name;
  selector    : dot;
  field       : 'a;
}

(* Map expressions *)

and map_expr = (binding reg, comma) nsepseq braces reg

and binding = {
  key   : expr;  (* If a string, then a dictionary. *)
  colon : colon;
  value : expr
}

(* Map comprehension *)

and map_comprehension = comprehension_map_expr reg braces reg

and comprehension_map_expr = {
  subject   : binding reg;
  kwd_for   : kwd_for;
  lpar      : lpar;
  key       : variable;
  comma     : comma;
  value     : variable;
  rpar      : rpar;
  kwd_in    : kwd_in;
  iterated  : expr;
  predicate : predicate reg option
}

(* Set expressions *)

and set_expr = (expr, comma) nsepseq braces reg

(* Set comprehension *)

and set_comprehension = comprehension_expr reg braces reg

(* List expressions *)

and list_expr = (expr, comma) sepseq brackets reg

(* Conditional expressions *)

and cond_expr = {
  value_when_true  : expr;
  kwd_if           : kwd_if;
  condition        : expr;
  kwd_else         : kwd_else;
  value_when_false : expr;
}

(* Lambda expressions *)

and lambda_expr = {
  kwd_lambda : kwd_lambda;
  params     : (param_decl reg, comma) sepseq;
  colon      : colon;
  body       : expr;
}

(* PROJECTING REGIONS *)

let variable_to_region = function
  Var w | Esc w -> w#region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec type_expr_to_region = function
  T_Attr    (_, te) -> type_expr_to_region te
| T_Class   {region; _}
| T_Cart    {region; _}
| T_Fun     {region; _} -> region
| T_Int     t -> t#region
| T_ModPath {region; _}
| T_Par     {region; _} -> region
| T_String  t -> t#region
| T_Sum     {region; _}
| T_Tuple   {region; _} -> region
| T_Var     t -> variable_to_region t

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec expr_to_region = function
  E_Add        {region; _}
| E_And        {region; _} -> region
| E_Attr       e -> expr_to_region (snd e)
| E_BitAnd     {region; _}
| E_BitNot     {region; _}
| E_BitOr      {region; _}
| E_BitXor     {region; _}
| E_BitLShift  {region; _}
| E_BitRShift  {region; _} -> region
| E_Bytes      t -> t#region
| E_Call       {region; _} -> region
| E_CodeInj    {region; _}
| E_Ctor       {region; _}
| E_Equal      {region; _}
| E_Exp        {region; _}
| E_Fun        {region; _}
| E_Geq        {region; _}
| E_Gt         {region; _}
| E_IfElse     {region; _} -> region
| E_Int        t -> t#region
| E_IntDiv     {region; _}
| E_Leq        {region; _}
| E_List       {region; _}
| E_ListCompr  {region; _}
| E_Lt         {region; _}
| E_Map        {region; _}
| E_MapCompr   {region; _}
| E_MapLookup  {region; _}
| E_Match      {region; _}
| E_Mod        {region; _}
| E_ModPath    {region; _}
| E_Mult       {region; _} -> region
| E_Mutez      t -> t#region
| E_Nat        t -> t#region
| E_Neg        {region; _}
| E_Neq        {region; _}
| E_Not        {region; _}
| E_Or         {region; _}
| E_Par        {region; _}
| E_Proj       {region; _}
| E_Set        {region; _}
| E_SetCompr   {region; _} -> region
| E_String     t -> t#region
| E_Sub        {region; _}
| E_Tuple      {region; _}
| E_Typed      {region; _} -> region
| E_Var        t -> variable_to_region t
| E_Variant    {region; _} -> region
| E_Verbatim   t -> t#region

and typed_expr_to_region x = x.Region.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec pattern_to_region = function
  P_Attr     (_,p) -> pattern_to_region p
| P_Bytes    t -> t#region
| P_Ctor     {region; _} -> region
| P_Int      t -> t#region
| P_ModPath  {region; _} -> region
| P_Mutez    t -> t#region
| P_Nat      t -> t#region
| P_Par      {region; _} -> region
| P_String   t -> t#region
| P_Tuple    {region; _}
| P_Typed    {region; _} -> region
| P_Var      t -> variable_to_region t
| P_Variant  {region; _} -> region
| P_Verbatim t -> t#region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let rec declaration_to_region = function
  D_Type      ClassEq {region; _}
| D_Type      TypeAlias {region; _}
| D_Const     {region; _}
| D_Fun       {region; _}
| D_Class     {region; _} -> region
| D_Attr      (_,d) -> declaration_to_region d
| D_Directive d -> Directive.to_region d

let selection_to_region = function
  MemberName n -> variable_to_region n
| Component  w -> w#region

let qualified_class_to_region = function
  ClassName name -> name#region
| ClassPath path -> path.Region.region

let simple_stmt_to_region = function
  S_Assign      {region; _}
| S_Return      {region; _}
| S_ModuleAlias {region; _}
| S_Assert      {region; _}
| S_ProcCall    {region; _} -> region
| S_Pass        t -> t#region

let compound_stmt_to_region = function
  C_Type  ClassEq {region; _}
| C_Type  TypeAlias {region; _}
| C_Const {region; _}
| C_Fun   {region; _}
| C_Var   {region; _}
| C_Class {region; _}
| C_If    {region; _}
| C_For   {region; _}
| C_While {region; _}
| C_Match {region; _} -> region

let local_stmt_to_region = function
  SimpleStmt   stmt -> simple_stmt_to_region   stmt
| CompoundStmt stmt -> compound_stmt_to_region stmt

let member_decl_to_region = function
  MemberFun  {region; _}
| MemberVars {region; _} -> region
