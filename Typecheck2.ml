module SMap = Map.Make(String)

open AST

type i = parse_phase
type typecheck_phase = (parse_phase type_expr *  tfalse)
type o = typecheck_phase

type te = o type_expr list SMap.t (* Type environment *)
type ve = o type_expr list SMap.t (* Value environment *)
type tve = te * ve

let id (ast : i ast) : o ast = {ast with eof = ast.eof}

(* Utilities *)

let fold_map f a l =
  let f (acc, l) elem =
    let acc', elem' = f acc elem
    in acc', (elem' :: l) in
  let last_acc, last_l = List.fold_left f (a, []) l
  in last_acc, List.rev last_l

let reg ({value;region} : 'a reg) (f : 'a -> 'b) : 'b reg = {value = f value; region}
let unreg ({value;_} : 'a reg) : 'a = value

(* Typecheck *)

let tc_type_decl (te, ve : tve) (td : i type_decl reg) : tve * o type_decl reg =
  (te, ve), (unreg td)

let tc_types (tve : tve) (types : i type_decl reg list) =
 fold_map tc_type_decl tve types

let tc_ast (tve : tve) (ast : i ast) =
  let {types;constants;parameter;storage;operations;lambdas;block;eof} = ast in
  let tve, types = tc_types tve types in
  let ast = {types;constants;parameter;storage;operations;lambdas;block;eof} in
  tve, ast

let tc_ast ast =
  let tve, ast = tc_ast (SMap.empty, SMap.empty) ast in
  let _ = tve in (* Drop the final type and value environment *)
  ast

(*
open Region
open Utils
type new_t = < ty: int > ast
and 'a ast = {
  types      : 'a type_decl reg list;
  constants  : 'a const_decl reg list;
  parameter  : 'a parameter_decl reg;
  storage    : 'a storage_decl reg;
  operations : 'a operations_decl reg;
  lambdas    : 'a lambda_decl list;
  block      : 'a block reg;
  eof        :    eof
}

and 'a parameter_decl = {
  kwd_parameter :    kwd_parameter;
  name          : 'a variable;
  colon         :    colon;
  param_type    : 'a type_expr;
  terminator    :    semi option
}

and 'a storage_decl = {
  kwd_storage :    kwd_storage;
  store_type  : 'a type_expr;
  terminator  :    semi option
}

and 'a operations_decl = {
  kwd_operations :    kwd_operations;
  op_type        : 'a type_expr;
  terminator     :    semi option
}

(* Type declarations *)

and 'a type_decl = {
  kwd_type   :    kwd_type;
  name       : 'a type_name;
  kwd_is     :    kwd_is;
  type_expr  : 'a type_expr;
  terminator :    semi option
}

and 'a type_expr =
  Prod    of 'a cartesian
| Sum     of ('a variant, vbar) nsepseq reg
| Record  of 'a record_type
| TypeApp of ('a type_name * 'a type_tuple) reg
| ParType of 'a type_expr par
| TAlias  of 'a variable

and 'a cartesian = ('a type_expr, times) nsepseq reg

and 'a variant = ('a constr * kwd_of * 'a cartesian) reg

and 'a record_type = (kwd_record * 'a field_decls * kwd_end) reg

and 'a field_decls = ('a field_decl, semi) nsepseq

and 'a field_decl = ('a variable * colon * 'a type_expr) reg

and 'a type_tuple = ('a type_name, comma) nsepseq par

(* Function and procedure declarations *)

and 'a lambda_decl =
  FunDecl  of 'a fun_decl reg
| ProcDecl of 'a proc_decl reg

and 'a fun_decl = {
  kwd_function :    kwd_function;
  name         : 'a variable;
  param        : 'a parameters;
  colon        :    colon;
  ret_type     : 'a type_expr;
  kwd_is       :    kwd_is;
  local_decls  : 'a local_decl list;
  block        : 'a block reg;
  kwd_with     :    kwd_with;
  return       : 'a expr;
  terminator   :    semi option
}

and 'a proc_decl = {
  kwd_procedure :    kwd_procedure;
  name          : 'a variable;
  param         : 'a parameters;
  kwd_is        :    kwd_is;
  local_decls   : 'a local_decl list;
  block         : 'a block reg;
  terminator    :    semi option
}

and 'a parameters = ('a param_decl, semi) nsepseq par

and 'a param_decl =
  ParamConst of 'a param_const
| ParamVar   of 'a param_var

and 'a param_const = (kwd_const * 'a variable * colon * 'a type_expr) reg

and 'a param_var = (kwd_var * 'a variable * colon * 'a type_expr) reg

and 'a block = {
  opening    :    kwd_begin;
  instr      : 'a instructions;
  terminator :    semi option;
  close      :    kwd_end
}

and 'a local_decl =
  LocalLam   of 'a lambda_decl
| LocalConst of 'a const_decl reg
| LocalVar   of 'a var_decl reg

and 'a const_decl = {
  kwd_const  :    kwd_const;
  name       : 'a variable;
  colon      :    colon;
  vtype      : 'a type_expr;
  equal      :    equal;
  init       : 'a expr;
  terminator :    semi option
}

and 'a var_decl = {
  kwd_var    :    kwd_var;
  name       : 'a variable;
  colon      :    colon;
  vtype      : 'a type_expr;
  ass        :    ass;
  init       : 'a expr;
  terminator :    semi option
}

and 'a instructions = ('a instruction, semi) nsepseq reg

and 'a instruction =
  Single of 'a single_instr
| Block  of 'a block reg

and 'a single_instr =
  Cond     of 'a conditional reg
| Match    of 'a match_instr reg
| Ass      of 'a ass_instr
| Loop     of 'a loop
| ProcCall of 'a fun_call
| Null     of kwd_null
| Fail     of (kwd_fail * 'a expr) reg

and 'a conditional = {
  kwd_if   :    kwd_if;
  test     : 'a expr;
  kwd_then :    kwd_then;
  ifso     : 'a instruction;
  kwd_else :    kwd_else;
  ifnot    : 'a instruction
}

and 'a match_instr = {
  kwd_match :    kwd_match;
  expr      : 'a expr;
  kwd_with  :    kwd_with;
  lead_vbar :    vbar option;
  cases     : 'a cases;
  kwd_end   :    kwd_end
}

and 'a cases = ('a case, vbar) nsepseq reg

and 'a case = ('a pattern * arrow * 'a instruction) reg

and 'a ass_instr = ('a variable * ass * 'a expr) reg

and 'a loop =
  While of 'a while_loop
| For   of 'a for_loop

and 'a while_loop = (kwd_while * 'a expr * 'a block reg) reg

and 'a for_loop =
  ForInt     of 'a for_int reg
| ForCollect of 'a for_collect reg

and 'a for_int = {
  kwd_for :    kwd_for;
  ass     : 'a ass_instr;
  down    :    kwd_down option;
  kwd_to  :    kwd_to;
  bound   : 'a expr;
  step    : (kwd_step * 'a expr) option;
  block   : 'a block reg
}

and 'a for_collect = {
  kwd_for :    kwd_for;
  var     : 'a variable;
  bind_to : (arrow * 'a variable) option;
  kwd_in  :    kwd_in;
  expr    : 'a expr;
  block   : 'a block reg
}

(* Expressions *)

and 'a expr =
  Or        of ('a expr * bool_or * 'a expr) reg
| And       of ('a expr * bool_and * 'a expr) reg
| Lt        of ('a expr * lt * 'a expr) reg
| Leq       of ('a expr * leq * 'a expr) reg
| Gt        of ('a expr * gt * 'a expr) reg
| Geq       of ('a expr * geq * 'a expr) reg
| Equal     of ('a expr * equal * 'a expr) reg
| Neq       of ('a expr * neq * 'a expr) reg
| Cat       of ('a expr * cat * 'a expr) reg
| Cons      of ('a expr * cons * 'a expr) reg
| Add       of ('a expr * plus * 'a expr) reg
| Sub       of ('a expr * minus * 'a expr) reg
| Mult      of ('a expr * times * 'a expr) reg
| Div       of ('a expr * slash * 'a expr) reg
| Mod       of ('a expr * kwd_mod * 'a expr) reg
| Neg       of (minus * 'a expr) reg
| Not       of (kwd_not * 'a expr) reg
| Int       of (Lexer.lexeme * Z.t) reg
| Var       of Lexer.lexeme reg
| String    of Lexer.lexeme reg
| Bytes     of (Lexer.lexeme * MBytes.t) reg
| False     of c_False
| True      of c_True
| Unit      of c_Unit
| Tuple     of 'a tuple
| List      of ('a expr, comma) nsepseq brackets
| EmptyList of 'a empty_list
| Set       of ('a expr, comma) nsepseq braces
| EmptySet  of 'a empty_set
| NoneExpr  of 'a none_expr
| FunCall   of 'a fun_call
| ConstrApp of 'a constr_app
| SomeApp   of (c_Some * 'a arguments) reg
| MapLookUp of 'a map_lookup reg
| ParExpr   of 'a expr par

and 'a tuple = ('a expr, comma) nsepseq par

and 'a empty_list =
  (lbracket * rbracket * colon * 'a type_expr) par

and 'a empty_set =
  (lbrace * rbrace * colon * 'a type_expr) par

and 'a none_expr =
  (c_None * colon * 'a type_expr) par

and 'a fun_call = ('a fun_name * 'a arguments) reg

and 'a arguments = 'a tuple

and 'a constr_app = ('a constr * 'a arguments) reg

and 'a map_lookup = {
  map_name : 'a variable;
  selector : dot;
  index    : 'a expr brackets
}

(* Patterns *)

and 'a pattern = ('a core_pattern, cons) nsepseq reg

and 'a core_pattern =
  PVar    of Lexer.lexeme reg
| PWild   of wild
| PInt    of (Lexer.lexeme * Z.t) reg
| PBytes  of (Lexer.lexeme * MBytes.t) reg
| PString of Lexer.lexeme reg
| PUnit   of c_Unit
| PFalse  of c_False
| PTrue   of c_True
| PNone   of c_None
| PSome   of (c_Some * 'a core_pattern par) reg
| PList   of 'a list_pattern
| PTuple  of ('a core_pattern, comma) nsepseq par

and 'a list_pattern =
  Sugar of ('a core_pattern, comma) sepseq brackets
| Raw   of ('a core_pattern * cons * 'a pattern) par
 *)
