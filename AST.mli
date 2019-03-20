(* Abstract Syntax Tree (AST) for LIGO *)

[@@@warning "-30"]

open Utils

(* Regions

   The AST carries all the regions where tokens have been found by the
   lexer, plus additional regions corresponding to whole subtrees
   (like entire expressions, patterns etc.). These regions are needed
   for error reporting and source-to-source transformations. To make
   these pervasive regions more legible, we define singleton types for
   the symbols, keywords etc. with suggestive names like "kwd_and"
   denoting the _region_ of the occurrence of the keyword "and".
*)

type 'a reg = 'a Region.reg

val nseq_to_region    : ('a -> Region.t) -> 'a nseq -> Region.t
val nsepseq_to_region : ('a -> Region.t) -> ('a,'sep) nsepseq -> Region.t
val sepseq_to_region  : ('a -> Region.t) -> ('a,'sep) sepseq -> Region.t

(* Keywords of LIGO *)

type kwd_begin      = Region.t
type kwd_case      = Region.t
type kwd_const      = Region.t
type kwd_down       = Region.t
type kwd_else       = Region.t
type kwd_end        = Region.t
type kwd_entrypoint = Region.t
type kwd_fail       = Region.t
type kwd_for        = Region.t
type kwd_function   = Region.t
type kwd_if         = Region.t
type kwd_in         = Region.t
type kwd_is         = Region.t
type kwd_map        = Region.t
type kwd_mod        = Region.t
type kwd_not        = Region.t
type kwd_of         = Region.t
type kwd_patch      = Region.t
type kwd_procedure  = Region.t
type kwd_record     = Region.t
type kwd_skip       = Region.t
type kwd_step       = Region.t
type kwd_storage    = Region.t
type kwd_then       = Region.t
type kwd_to         = Region.t
type kwd_type       = Region.t
type kwd_var        = Region.t
type kwd_while      = Region.t
type kwd_with       = Region.t

(* Data constructors *)

type c_False = Region.t
type c_None  = Region.t
type c_Some  = Region.t
type c_True  = Region.t
type c_Unit  = Region.t

(* Symbols *)

type semi     = Region.t  (* ";"   *)
type comma    = Region.t  (* ","   *)
type lpar     = Region.t  (* "("   *)
type rpar     = Region.t  (* ")"   *)
type lbrace   = Region.t  (* "{"   *)
type rbrace   = Region.t  (* "}"   *)
type lbracket = Region.t  (* "["   *)
type rbracket = Region.t  (* "]"   *)
type cons     = Region.t  (* "#"   *)
type vbar     = Region.t  (* "|"   *)
type arrow    = Region.t  (* "->"  *)
type assign   = Region.t  (* ":="  *)
type equal    = Region.t  (* "="   *)
type colon    = Region.t  (* ":"   *)
type bool_or  = Region.t  (* "||"  *)
type bool_and = Region.t  (* "&&"  *)
type lt       = Region.t  (* "<"   *)
type leq      = Region.t  (* "<="  *)
type gt       = Region.t  (* ">"   *)
type geq      = Region.t  (* ">="  *)
type neq      = Region.t  (* "=/=" *)
type plus     = Region.t  (* "+"   *)
type minus    = Region.t  (* "-"   *)
type slash    = Region.t  (* "/"   *)
type times    = Region.t  (* "*"   *)
type dot      = Region.t  (* "."   *)
type wild     = Region.t  (* "_"   *)
type cat      = Region.t  (* "^"   *)

(* Virtual tokens *)

type eof = Region.t

(* Literals *)

type variable   = string reg
type fun_name   = string reg
type type_name  = string reg
type field_name = string reg
type map_name   = string reg
type constr     = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

(* Brackets compounds *)

type 'a brackets = {
  lbracket : lbracket;
  inside   : 'a;
  rbracket : rbracket
}

(* Braced compounds *)

type 'a braces = {
  lbrace : lbrace;
  inside : 'a;
  rbrace : rbrace
}

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and ast = t

and declaration =
  TypeDecl    of type_decl reg
| ConstDecl   of const_decl reg
| LambdaDecl  of lambda_decl

and const_decl = {
  kwd_const  : kwd_const;
  name       : variable;
  colon      : colon;
  const_type : type_expr;
  equal      : equal;
  init       : expr;
  terminator : semi option
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  kwd_is     : kwd_is;
  type_expr  : type_expr;
  terminator : semi option
}

and type_expr =
  Prod    of cartesian
| Sum     of (variant reg, vbar) nsepseq reg
| Record  of record_type reg
| TypeApp of (type_name * type_tuple) reg
| ParType of type_expr par reg
| TAlias  of variable

and cartesian = (type_expr, times) nsepseq reg

and variant = {
  constr  : constr;
  kwd_of  : kwd_of;
  product : cartesian
}

and record_type = {
  kwd_record : kwd_record;
  fields     : field_decls;
  kwd_end    : kwd_end
}

and field_decls = (field_decl reg, semi) nsepseq

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr
}

and type_tuple = (type_expr, comma) nsepseq par reg

(* Function and procedure declarations *)

and lambda_decl =
  FunDecl   of fun_decl   reg
| ProcDecl  of proc_decl  reg
| EntryDecl of entry_decl reg

and fun_decl = {
  kwd_function : kwd_function;
  name         : variable;
  param        : parameters;
  colon        : colon;
  ret_type     : type_expr;
  kwd_is       : kwd_is;
  local_decls  : local_decl list;
  block        : block reg;
  kwd_with     : kwd_with;
  return       : expr;
  terminator   : semi option
}

and proc_decl = {
  kwd_procedure : kwd_procedure;
  name          : variable;
  param         : parameters;
  kwd_is        : kwd_is;
  local_decls   : local_decl list;
  block         : block reg;
  terminator    : semi option
}

and entry_decl = {
  kwd_entrypoint : kwd_entrypoint;
  name           : variable;
  param          : entry_params;
  colon          : colon;
  ret_type       : type_expr;
  kwd_is         : kwd_is;
  local_decls    : local_decl list;
  block          : block reg;
  kwd_with       : kwd_with;
  return         : expr;
  terminator     : semi option
}

and parameters = (param_decl, semi) nsepseq par reg

and entry_params = (entry_param_decl, semi) nsepseq par reg

and entry_param_decl =
  EntryConst of param_const reg
| EntryVar   of param_var reg
| EntryStore of storage reg

and storage = {
  kwd_storage  : kwd_storage;
  var          : variable;
  colon        : colon;
  storage_type : type_expr
}

and param_decl =
  ParamConst of param_const reg
| ParamVar   of param_var reg

and param_const = {
  kwd_const  : kwd_const;
  var        : variable;
  colon      : colon;
  param_type : type_expr
}

and param_var = {
  kwd_var    : kwd_var;
  var        : variable;
  colon      : colon;
  param_type : type_expr
}

and block = {
  opening    : kwd_begin;
  instr      : instructions;
  terminator : semi option;
  close      : kwd_end
}

and local_decl =
  LocalLam   of lambda_decl
| LocalConst of const_decl reg
| LocalVar   of var_decl reg

and var_decl = {
  kwd_var    : kwd_var;
  name       : variable;
  colon      : colon;
  var_type   : type_expr;
  assign     : assign;
  init       : expr;
  terminator : semi option
}

and instructions = (instruction, semi) nsepseq

and instruction =
  Single of single_instr
| Block  of block reg

and single_instr =
  Cond        of conditional reg
| Case        of case_instr reg
| Assign      of assignment reg
| Loop        of loop
| ProcCall    of fun_call
| Fail        of fail_instr reg
| Skip        of kwd_skip
| RecordPatch of record_patch reg
| MapPatch    of map_patch reg

and map_patch  = {
  kwd_patch : kwd_patch;
  path      : path;
  kwd_with  : kwd_with;
  map_inj   : map_injection reg
}

and map_injection = {
  opening    : kwd_map;
  bindings   : (binding reg, semi) nsepseq;
  terminator : semi option;
  close      : kwd_end
}

and binding = {
  source : expr;
  arrow  : arrow;
  image  : expr
}

and record_patch = {
  kwd_patch  : kwd_patch;
  path       : path;
  kwd_with   : kwd_with;
  record_inj : record_injection reg
}

and fail_instr = {
  kwd_fail  : kwd_fail;
  fail_expr : expr
}

and conditional = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : instruction;
  kwd_else : kwd_else;
  ifnot    : instruction
}

and case_instr = {
  kwd_case  : kwd_case;
  expr      : expr;
  kwd_of    : kwd_of;
  lead_vbar : vbar option;
  cases     : cases;
  kwd_end   : kwd_end
}

and cases = (case reg, vbar) nsepseq reg

and case = {
  pattern : pattern;
  arrow   : arrow;
  instr   : instruction
}

and assignment = {
  lhs    : lhs;
  assign : assign;
  rhs    : rhs;
}

and lhs =
  Path    of path
| MapPath of map_lookup reg

and rhs =
      Expr of expr
| NoneExpr of c_None

and loop =
  While of while_loop reg
| For   of for_loop

and while_loop = {
  kwd_while : kwd_while;
  cond      : expr;
  block     : block reg
}

and for_loop =
  ForInt     of for_int reg
| ForCollect of for_collect reg

and for_int = {
  kwd_for : kwd_for;
  assign  : var_assign reg;
  down    : kwd_down option;
  kwd_to  : kwd_to;
  bound   : expr;
  step    : (kwd_step * expr) option;
  block   : block reg
}

and var_assign = {
  name   : variable;
  assign : assign;
  expr   : expr
}

and for_collect = {
  kwd_for : kwd_for;
  var     : variable;
  bind_to : (arrow * variable) option;
  kwd_in  : kwd_in;
  expr    : expr;
  block   : block reg
}

(* Expressions *)

and expr =
  LogicExpr  of logic_expr
| ArithExpr  of arith_expr
| StringExpr of string_expr
| ListExpr   of list_expr
| SetExpr    of set_expr
| ConstrExpr of constr_expr
| RecordExpr of record_expr
| MapExpr    of map_expr
| Var        of Lexer.lexeme reg
| FunCall    of fun_call
| Bytes      of (Lexer.lexeme * MBytes.t) reg
| Unit       of c_Unit
| Tuple      of tuple
| ParExpr    of expr par reg

and map_expr =
  MapLookUp of map_lookup reg
| MapInj    of map_injection reg

and map_lookup = {
  path  : path;
  index : expr brackets reg
}

and path =
  Name       of variable
| RecordPath of record_projection reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of bool_or  bin_op reg
| And   of bool_and bin_op reg
| Not   of kwd_not   un_op reg
| False of c_False
| True  of c_True

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and comp_expr =
  Lt    of lt    bin_op reg
| Leq   of leq   bin_op reg
| Gt    of gt    bin_op reg
| Geq   of geq   bin_op reg
| Equal of equal bin_op reg
| Neq   of neq   bin_op reg

and arith_expr =
  Add  of plus    bin_op reg
| Sub  of minus   bin_op reg
| Mult of times   bin_op reg
| Div  of slash   bin_op reg
| Mod  of kwd_mod bin_op reg
| Neg  of minus    un_op reg
| Int  of (Lexer.lexeme * Z.t) reg

and string_expr =
  Cat    of cat bin_op reg
| String of Lexer.lexeme reg

and list_expr =
  Cons      of cons bin_op reg
| List      of (expr, comma) nsepseq brackets reg
| EmptyList of empty_list reg

and set_expr =
  Set       of (expr, comma) nsepseq braces reg
| EmptySet  of empty_set reg

and constr_expr =
  SomeApp   of (c_Some * arguments) reg
| NoneExpr  of none_expr reg
| ConstrApp of (constr * arguments) reg

and record_expr =
  RecordInj  of record_injection reg
| RecordProj of record_projection reg

and record_injection = {
  opening    : kwd_record;
  fields     : (field_assign reg, semi) nsepseq;
  terminator : semi option;
  close      : kwd_end
}

and field_assign = {
  field_name : field_name;
  equal      : equal;
  field_expr : expr
}

and record_projection = {
  record_name : variable;
  selector    : dot;
  field_path  : (field_name, dot) nsepseq
}

and tuple = (expr, comma) nsepseq par reg

and empty_list = typed_empty_list par

and typed_empty_list = {
  lbracket  : lbracket;
  rbracket  : rbracket;
  colon     : colon;
  list_type : type_expr
}

and empty_set = typed_empty_set par

and typed_empty_set = {
  lbrace   : lbrace;
  rbrace   : rbrace;
  colon    : colon;
  set_type : type_expr
}

and none_expr = typed_none_expr par

and typed_none_expr = {
  c_None   : c_None;
  colon    : colon;
  opt_type : type_expr
}

and fun_call = (fun_name * arguments) reg

and arguments = tuple

(* Patterns *)

and pattern =
  PCons   of (pattern, cons) nsepseq reg
| PVar    of Lexer.lexeme reg
| PWild   of wild
| PInt    of (Lexer.lexeme * Z.t) reg
| PBytes  of (Lexer.lexeme * MBytes.t) reg
| PString of Lexer.lexeme reg
| PUnit   of c_Unit
| PFalse  of c_False
| PTrue   of c_True
| PNone   of c_None
| PSome   of (c_Some * pattern par reg) reg
| PList   of list_pattern
| PTuple  of (pattern, comma) nsepseq par reg

and list_pattern =
  Sugar of (pattern, comma) sepseq brackets reg
| Raw   of (pattern * cons * pattern) par reg

(* Projecting regions *)

val type_expr_to_region  : type_expr -> Region.t
val expr_to_region       : expr -> Region.t
val instr_to_region      : instruction -> Region.t
val pattern_to_region    : pattern -> Region.t
val local_decl_to_region : local_decl -> Region.t
val path_to_region       : path -> Region.t
val lhs_to_region        : lhs -> Region.t
val rhs_to_region        : rhs -> Region.t

(* Printing *)

val print_tokens : t -> unit
