(* Abstract Syntax Tree (AST) for Ligo *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-42"]

(* Utilities *)

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

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd,tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let sepseq_to_region to_region = function
      None -> Region.ghost
| Some seq -> nsepseq_to_region to_region seq

(* Keywords of Ligo *)

type kwd_begin      = Region.t
type kwd_const      = Region.t
type kwd_down       = Region.t
type kwd_fail       = Region.t
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
type ass      = Region.t
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

type 'a variable   = string reg
type 'a fun_name   = string reg
type 'a type_name  = string reg
type 'a field_name = string reg
type 'a map_name   = string reg
type 'a constr     = string reg

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

type t = < ty: unit > ast

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

(* Projecting regions *)

open! Region

let type_expr_to_region = function
  Prod    node -> node.region
| Sum     node -> node.region
| Record  node -> node.region
| TypeApp node -> node.region
| ParType node -> node.region
| TAlias  node -> node.region

let expr_to_region = function
  Or        {region; _}
| And       {region; _}
| Lt        {region; _}
| Leq       {region; _}
| Gt        {region; _}
| Geq       {region; _}
| Equal     {region; _}
| Neq       {region; _}
| Cat       {region; _}
| Cons      {region; _}
| Add       {region; _}
| Sub       {region; _}
| Mult      {region; _}
| Div       {region; _}
| Mod       {region; _}
| Neg       {region; _}
| Not       {region; _}
| Int       {region; _}
| Var       {region; _}
| String    {region; _}
| Bytes     {region; _}
| False     region
| True      region
| Unit      region
| Tuple     {region; _}
| List      {region; _}
| EmptyList {region; _}
| Set       {region; _}
| EmptySet  {region; _}
| NoneExpr  {region; _}
| FunCall   {region; _}
| ConstrApp {region; _}
| SomeApp   {region; _}
| MapLookUp {region; _}
| ParExpr   {region; _} -> region

let instr_to_region = function
  Single Cond                {region;_}
| Single Match               {region; _}
| Single Ass                 {region; _}
| Single Loop While          {region; _}
| Single Loop For ForInt     {region; _}
| Single Loop For ForCollect {region; _}
| Single ProcCall            {region; _}
| Single Null                 region
| Single Fail                {region; _}
| Block                      {region; _} -> region

let core_pattern_to_region = function
  PVar        {region; _}
| PWild        region
| PInt        {region; _}
| PBytes      {region; _}
| PString     {region; _}
| PUnit        region
| PFalse       region
| PTrue        region
| PNone        region
| PSome       {region; _}
| PList Sugar {region; _}
| PList Raw   {region; _}
| PTuple      {region; _} -> region

let local_decl_to_region = function
  LocalLam FunDecl  {region; _}
| LocalLam ProcDecl {region; _}
| LocalConst        {region; _}
| LocalVar          {region; _} -> region

(* Printing the tokens with their source regions *)

type 'a visitor = {
  ass_instr       : 'a ass_instr -> unit;
  bind_to         : (region * 'a variable) option -> unit;
  block           : 'a block reg -> unit;
  bytes           : (string * MBytes.t) reg -> unit;
  cartesian       : 'a cartesian -> unit;
  case            : 'a case -> unit;
  cases           : 'a cases -> unit;
  conditional     : 'a conditional -> unit;
  const_decl      : 'a const_decl reg -> unit;
  constr          : 'a constr -> unit;
  constr_app      : 'a constr_app -> unit;
  core_pattern    : 'a core_pattern -> unit;
  down            : region option -> unit;
  empty_list      : 'a empty_list -> unit;
  empty_set       : 'a empty_set -> unit;
  expr            : 'a expr -> unit;
  fail            : (kwd_fail * 'a expr) -> unit;
  field_decl      : 'a field_decl -> unit;
  field_decls     : 'a field_decls -> unit;
  for_collect     : 'a for_collect reg -> unit;
  for_int         : 'a for_int reg -> unit;
  for_loop        : 'a for_loop -> unit;
  fun_call        : 'a fun_call -> unit;
  fun_decl        : 'a fun_decl reg -> unit;
  instruction     : 'a instruction -> unit;
  instructions    : 'a instructions -> unit;
  int             : (string * Z.t) reg -> unit;
  lambda_decl     : 'a lambda_decl -> unit;
  list            : ('a expr, region) nsepseq brackets -> unit;
  list_pattern    : 'a list_pattern -> unit;
  loop            : 'a loop -> unit;
  map_lookup      : 'a map_lookup reg -> unit;
  match_instr     : 'a match_instr -> unit;
  none_expr       : 'a none_expr -> unit;
  nsepseq         : 'a.string -> ('a -> unit) -> ('a, region) nsepseq -> unit;
  operations_decl : 'a operations_decl reg -> unit;
  par_expr        : 'a expr par -> unit;
  par_type        : 'a type_expr par -> unit;
  param_decl      : 'a param_decl -> unit;
  parameter_decl  : 'a parameter_decl reg -> unit;
  parameters      : 'a parameters -> unit;
  param_const     : 'a param_const -> unit;
  param_var       : 'a param_var -> unit;
  pattern         : 'a pattern -> unit;
  patterns        : 'a core_pattern par -> unit;
  proc_decl       : 'a proc_decl reg -> unit;
  psome           : (region * 'a core_pattern par) reg -> unit;
  ptuple          : ('a core_pattern, region) nsepseq par -> unit;
  raw             : ('a core_pattern * region * 'a pattern) par -> unit;
  record_type     : 'a record_type -> unit;
  sepseq          : 'a.string -> ('a -> unit) -> ('a, region) sepseq -> unit;
  set             : ('a expr, region) nsepseq braces -> unit;
  single_instr    : 'a single_instr -> unit;
  some_app        : (region * 'a arguments) reg -> unit;
  step            : (region * 'a expr) option -> unit;
  storage_decl    : 'a storage_decl reg -> unit;
  string          : string reg -> unit;
  sugar           : ('a core_pattern, region) sepseq brackets -> unit;
  sum_type        : ('a variant, region) nsepseq reg -> unit;
  terminator      : semi option -> unit;
  token           : region -> string -> unit;
  tuple           : 'a arguments -> unit;
  type_app        : ('a type_name * 'a type_tuple) reg -> unit;
  type_decl       : 'a type_decl reg -> unit;
  type_expr       : 'a type_expr -> unit;
  type_tuple      : 'a type_tuple -> unit;
  local_decl      : 'a local_decl -> unit;
  local_decls     : 'a local_decl list -> unit;
  var             : 'a variable -> unit;
  var_decl        : 'a var_decl reg -> unit;
  variant         : 'a variant -> unit;
  while_loop      : 'a while_loop -> unit
}
