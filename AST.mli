(* Abstract Syntax Tree (AST) for Ligo *)

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

(* Keywords of Ligo *)

type kwd_begin      = Region.t
type kwd_const      = Region.t
type kwd_down       = Region.t
type kwd_if         = Region.t
type kwd_in         = Region.t
type kwd_is         = Region.t
type kwd_for        = Region.t
type kwd_function   = Region.t
type kwd_parameter  = Region.t
type kwd_storage    = Region.t
type kwd_type       = Region.t
type kwd_of         = Region.t
type kwd_operations = Region.t
type kwd_var        = Region.t
type kwd_end        = Region.t
type kwd_then       = Region.t
type kwd_else       = Region.t
type kwd_match      = Region.t
type kwd_procedure  = Region.t
type kwd_null       = Region.t
type kwd_record     = Region.t
type kwd_step       = Region.t
type kwd_to         = Region.t
type kwd_mod        = Region.t
type kwd_not        = Region.t
type kwd_while      = Region.t
type kwd_with       = Region.t

(* Data constructors *)

type c_False = Region.t
type c_None  = Region.t
type c_Some  = Region.t
type c_True  = Region.t
type c_Unit  = Region.t

(* Symbols *)

type semi     = Region.t
type comma    = Region.t
type lpar     = Region.t
type rpar     = Region.t
type lbrace   = Region.t
type rbrace   = Region.t
type lbracket = Region.t
type rbracket = Region.t
type cons     = Region.t
type vbar     = Region.t
type arrow    = Region.t
type asgnmnt  = Region.t
type equal    = Region.t
type colon    = Region.t
type bool_or  = Region.t
type bool_and = Region.t
type lt       = Region.t
type leq      = Region.t
type gt       = Region.t
type geq      = Region.t
type neq      = Region.t
type plus     = Region.t
type minus    = Region.t
type slash    = Region.t
type times    = Region.t
type dot      = Region.t
type wild     = Region.t
type cat      = Region.t

(* Virtual tokens *)

type eof = Region.t

(* Literals *)

type variable   = string reg
type fun_name   = string reg
type type_name  = string reg
type field_name = string reg
type map_name   = string reg
type constr     = string reg

(* Comma-separated non-empty lists *)

type 'a csv = ('a, comma) nsepseq

(* Bar-separated non-empty lists *)

type 'a bsv = ('a, vbar) nsepseq

(* Parentheses *)

type 'a par = (lpar * 'a * rpar) reg

(* Brackets compounds *)

type 'a brackets = (lbracket * 'a * rbracket) reg

(* Braced compounds *)

type 'a braces = (lbrace * 'a * rbrace) reg

(* The Abstract Syntax Tree *)

type t = <
  types      : type_decl list;
  parameter  : parameter_decl;
  storage    : storage_decl;
  operations : operations_decl;
  lambdas    : lambda_decl list;
  block      : block reg;
  eof        : eof
>

and ast = t

and parameter_decl = (kwd_parameter * variable * colon * type_expr) reg

and storage_decl = (kwd_storage * type_expr) reg

and operations_decl = (kwd_operations * type_expr) reg

(* Type declarations *)

and type_decl = (kwd_type * type_name * kwd_is * type_expr) reg

and type_expr =
  Prod    of cartesian
| Sum     of (variant, vbar) nsepseq reg
| Record  of record_type
| TypeApp of (type_name * type_tuple) reg
| ParType of type_expr par
| TAlias  of variable

and cartesian = (type_expr, times) nsepseq reg

and variant = (constr * kwd_of * cartesian) reg

and record_type = (kwd_record * field_decls * kwd_end) reg

and field_decls = (field_decl, semi) nsepseq

and field_decl = (variable * colon * type_expr) reg

and type_tuple = (type_name, comma) nsepseq par

(* Function and procedure declarations *)

and lambda_decl =
  FunDecl  of fun_decl reg
| ProcDecl of proc_decl reg

and fun_decl = <
  kwd_function : kwd_function;
  var          : variable;
  param        : parameters;
  colon        : colon;
  ret_type     : type_expr;
  kwd_is       : kwd_is;
  body         : block reg;
  kwd_with     : kwd_with;
  return       : expr
>

and proc_decl = <
  kwd_procedure : kwd_procedure;
  var           : variable;
  param         : parameters;
  kwd_is        : kwd_is;
  body          : block reg
>

and parameters = (param_decl, semi) nsepseq par

and param_decl = (var_kind * variable * colon * type_expr) reg

and var_kind =
  Mutable of kwd_var
| Const   of kwd_const

and block = <
  decls   : value_decls;
  opening : kwd_begin;
  instr   : instructions;
  close   : kwd_end
>

and value_decls = (var_decl reg, semi) sepseq reg

and var_decl = <
  kind   : var_kind;
  var    : variable;
  colon  : colon;
  vtype  : type_expr;
  setter : Region.t;  (* "=" or ":=" *)
  init   : expr
>

and instructions = (instruction, semi) nsepseq reg

and instruction =
  Single of single_instr
| Block  of block reg

and single_instr =
  Cond     of conditional reg
| Match    of match_instr reg
| Asgnmnt  of asgnmnt_instr
| Loop     of loop
| ProcCall of fun_call
| Null     of kwd_null

and conditional = <
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : instruction;
  kwd_else : kwd_else;
  ifnot    : instruction
>

and match_instr = <
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  cases     : cases;
  kwd_end   : kwd_end
>

and cases = (case, vbar) nsepseq reg

and case = (pattern * arrow * instruction) reg

and asgnmnt_instr = (variable * asgnmnt * expr) reg

and loop =
  While of while_loop
| For   of for_loop

and while_loop = (kwd_while * expr * block reg) reg

and for_loop =
  ForInt     of for_int reg
| ForCollect of for_collect reg

and for_int = <
  kwd_for : kwd_for;
  asgnmnt : asgnmnt_instr;
  down    : kwd_down option;
  kwd_to  : kwd_to;
  bound   : expr;
  step    : (kwd_step * expr) option;
  block   : block reg
>

and for_collect = <
  kwd_for : kwd_for;
  var     : variable;
  bind_to : (arrow * variable) option;
  kwd_in  : kwd_in;
  expr    : expr;
  block   : block reg
>

(* Expressions *)

and expr =
  Or        of (expr * bool_or * expr) reg
| And       of (expr * bool_and * expr) reg
| Lt        of (expr * lt * expr) reg
| Leq       of (expr * leq * expr) reg
| Gt        of (expr * gt * expr) reg
| Geq       of (expr * geq * expr) reg
| Equal     of (expr * equal * expr) reg
| Neq       of (expr * neq * expr) reg
| Cat       of (expr * cat * expr) reg
| Cons      of (expr * cons * expr) reg
| Add       of (expr * plus * expr) reg
| Sub       of (expr * minus * expr) reg
| Mult      of (expr * times * expr) reg
| Div       of (expr * slash * expr) reg
| Mod       of (expr * kwd_mod * expr) reg
| Neg       of (minus * expr) reg
| Not       of (kwd_not * expr) reg
| Int       of (Lexer.lexeme * Z.t) reg
| Var       of Lexer.lexeme reg
| String    of Lexer.lexeme reg
| Bytes     of (Lexer.lexeme * MBytes.t) reg
| False     of c_False
| True      of c_True
| Unit      of c_Unit
| Tuple     of tuple
| List      of (expr, comma) nsepseq brackets
| EmptyList of empty_list
| Set       of (expr, comma) nsepseq braces
| EmptySet  of empty_set
| NoneExpr  of none_expr
| FunCall   of fun_call
| ConstrApp of constr_app
| SomeApp   of (c_Some * arguments) reg
| MapLookUp of map_lookup reg
| ParExpr   of expr par

and tuple = (expr, comma) nsepseq par

and empty_list =
  (lbracket * rbracket * colon * type_expr) par

and empty_set =
  (lbrace * rbrace * colon * type_expr) par

and none_expr =
  (c_None * colon * type_expr) par

and fun_call = (fun_name * arguments) reg

and arguments = tuple

and constr_app = (constr * arguments) reg

and map_lookup = <
  map_name : variable;
  selector : dot;
  index    : expr brackets
>

(* Patterns *)

and pattern = (core_pattern, cons) nsepseq reg

and core_pattern =
  PVar    of Lexer.lexeme reg
| PWild   of wild
| PInt    of (Lexer.lexeme * Z.t) reg
| PBytes  of (Lexer.lexeme * MBytes.t) reg
| PString of Lexer.lexeme reg
| PUnit   of c_Unit
| PFalse  of c_False
| PTrue   of c_True
| PNone   of c_None
| PSome   of (c_Some * core_pattern par) reg
| PList   of list_pattern
| PTuple  of (core_pattern, comma) nsepseq par

and list_pattern =
  Sugar of (core_pattern, comma) sepseq brackets
| Raw   of (core_pattern * cons * pattern) par

(* Projecting regions *)

val type_expr_to_region : type_expr -> Region.t

val expr_to_region : expr -> Region.t

val var_kind_to_region : var_kind -> Region.t

val instr_to_region : instruction -> Region.t

val core_pattern_to_region : core_pattern -> Region.t

(* Printing *)

val print_tokens : t -> unit
