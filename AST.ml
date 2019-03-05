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

type 'x variable   = string reg
type 'x fun_name   = string reg
type 'x type_name  = string reg
type 'x field_name = string reg
type 'x map_name   = string reg
type 'x constr     = string reg

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

type ttrue = TTrue
type tfalse = TFalse
type ('a, 'type_expr_typecheck) gadt_if =
  Present : 'a -> ('a, ttrue) gadt_if

(* It is possible to further ensure well-typedness at the meta level
   by using the following constraint:

       type ttrue = [`True]
       type tfalse = [`False]

       type 'x x_sig = 'x
       constraint 'x = < ty:                  'ty;
                         type_expr_typecheck: [< `True | `False] >

   we could also use a single selector for type_expr, as long as
   the fields are monotonic:

       type    z = [`Z]
       type 'i s = [`S of 'i]
       type 'is type_level_int = [< `S of 'i | `Z]
       constraint 'i = 'prev type_level_int

       type     parse_phase = z
       type typecheck_phase = z s
       type   further_phase = z s s

       type 'x x_sig = 'x
       constraint 'x = < ty:                  'ty;
                         type_expr:           'type_expr >

   These schemes provide more guidance but the simple one below is
   sufficient.
 *)
type 'x x_sig = 'x
constraint 'x = < annot:               'type_annotation;
                  type_expr_typecheck: 'bool1 >

type 'x ast = {
  types      : 'x type_decl reg list;
  constants  : 'x const_decl reg list;
  parameter  : 'x parameter_decl reg;
  storage    : 'x storage_decl reg;
  operations : 'x operations_decl reg;
  lambdas    : 'x lambda_decl list;
  block      : 'x block reg;
  eof        :    eof
}
constraint 'x = 'x x_sig

and 'x parameter_decl = {
  kwd_parameter :    kwd_parameter;
  name          : 'x variable;
  colon         :    colon;
  param_type    : 'x type_expr;
  terminator    :    semi option
}
constraint 'x = 'x x_sig

and 'x storage_decl = {
  kwd_storage :    kwd_storage;
  store_type  : 'x type_expr;
  terminator  :    semi option
}
constraint 'x = 'x x_sig

and 'x operations_decl = {
  kwd_operations :    kwd_operations;
  op_type        : 'x type_expr;
  terminator     :    semi option
}
constraint 'x = 'x x_sig

(* Type declarations *)

and 'x type_decl = {
  kwd_type   :    kwd_type;
  name       : 'x type_name;
  kwd_is     :    kwd_is;
  type_expr  : 'x type_expr;
  terminator :    semi option
}
constraint 'x = 'x x_sig

and 'x type_expr =
  Prod     of ('x cartesian,                                      ttrue) gadt_if
| Sum      of (('x variant, vbar) nsepseq reg,                    ttrue) gadt_if
| Record   of ('x record_type,                                    ttrue) gadt_if
| TypeApp  of (('x type_name * 'x type_tuple) reg,                ttrue) gadt_if
| ParType  of ('x type_expr par,                                  ttrue) gadt_if
| TAlias   of ('x variable,                                       ttrue) gadt_if

| Function of (('x type_expr list) * 'x type_expr, 'type_expr_typecheck) gadt_if
| Mutable  of ('x type_expr,                       'type_expr_typecheck) gadt_if
| Unit     of (unit,                               'type_expr_typecheck) gadt_if
constraint 'x = < type_expr_typecheck: 'type_expr_typecheck;
                  .. >
constraint 'x = 'x x_sig

(*
and 'x type_expr = ('x cartesian,
                    ('x variant, vbar) nsepseq reg,
                    'x record_type,
                    ('x type_name * 'x type_tuple) reg,
                    'x type_expr par,
                    'x variable,

                    ('x type_expr list) * 'x type_expr,
                    'x type_expr,
                    unit,

                    'type_expr_parse,
                    'type_expr_typecheck) type_expr_gadt
constraint 'x = < type_expr_parse:     'type_expr_parse;
                  type_expr_typecheck: 'type_expr_typecheck;
                  .. >
constraint 'x = 'x x_sig

and ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'type_expr_parse, 'type_expr_typecheck) type_expr_gadt =
  Prod     : 'a -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, ttrue,  ttrue) type_expr_gadt
| Sum      : 'b -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, ttrue,  ttrue) type_expr_gadt
| Record   : 'c -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, ttrue,  ttrue) type_expr_gadt
| TypeApp  : 'd -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, ttrue,  ttrue) type_expr_gadt
| ParType  : 'e -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, ttrue,  ttrue) type_expr_gadt
| TAlias   : 'f -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, ttrue,  ttrue) type_expr_gadt

| Function : 'g -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, tfalse, ttrue) type_expr_gadt
| Mutable  : 'h -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, tfalse, ttrue) type_expr_gadt
| Unit     : 'i -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, tfalse, ttrue) type_expr_gadt
 *)

and 'x cartesian = ('x type_expr, times) nsepseq reg
constraint 'x = 'x x_sig

and 'x variant = ('x constr * kwd_of * 'x cartesian) reg
constraint 'x = 'x x_sig

and 'x record_type = (kwd_record * 'x field_decls * kwd_end) reg
constraint 'x = 'x x_sig

and 'x field_decls = ('x field_decl, semi) nsepseq
constraint 'x = 'x x_sig

and 'x field_decl = ('x variable * colon * 'x type_expr) reg
constraint 'x = 'x x_sig

and 'x type_tuple = ('x type_name, comma) nsepseq par
constraint 'x = 'x x_sig

(* Function and procedure declarations *)

and 'x lambda_decl =
  FunDecl  of 'x fun_decl reg
| ProcDecl of 'x proc_decl reg
constraint 'x = 'x x_sig

and 'x fun_decl = {
  kwd_function :    kwd_function;
  name         : 'x variable;
  param        : 'x parameters;
  colon        :    colon;
  ret_type     : 'x type_expr;
  kwd_is       :    kwd_is;
  local_decls  : 'x local_decl list;
  block        : 'x block reg;
  kwd_with     :    kwd_with;
  return       : 'x expr;
  terminator   :    semi option
}
constraint 'x = 'x x_sig

and 'x proc_decl = {
  kwd_procedure :    kwd_procedure;
  name          : 'x variable;
  param         : 'x parameters;
  kwd_is        :    kwd_is;
  local_decls   : 'x local_decl list;
  block         : 'x block reg;
  terminator    :    semi option
}
constraint 'x = 'x x_sig

and 'x parameters = ('x param_decl, semi) nsepseq par
constraint 'x = 'x x_sig

and 'x param_decl =
  ParamConst of 'x param_const
| ParamVar   of 'x param_var
constraint 'x = 'x x_sig

and 'x param_const = (kwd_const * 'x variable * colon * 'x type_expr) reg
constraint 'x = 'x x_sig

and 'x param_var = (kwd_var * 'x variable * colon * 'x type_expr) reg
constraint 'x = 'x x_sig

and 'x block = {
  opening    :    kwd_begin;
  instr      : 'x instructions;
  terminator :    semi option;
  close      :    kwd_end
}
constraint 'x = 'x x_sig

and 'x local_decl =
  LocalLam   of 'x lambda_decl
| LocalConst of 'x const_decl reg
| LocalVar   of 'x var_decl reg
constraint 'x = 'x x_sig

and 'x const_decl = {
  kwd_const  :    kwd_const;
  name       : 'x variable;
  colon      :    colon;
  vtype      : 'x type_expr;
  equal      :    equal;
  init       : 'x expr;
  terminator :    semi option
}
constraint 'x = 'x x_sig

and 'x var_decl = {
  kwd_var    :    kwd_var;
  name       : 'x variable;
  colon      :    colon;
  vtype      : 'x type_expr;
  ass        :    ass;
  init       : 'x expr;
  terminator :    semi option
}
constraint 'x = 'x x_sig

and 'x instructions = ('x instruction, semi) nsepseq reg
constraint 'x = 'x x_sig

and 'x instruction =
  Single of 'x single_instr
| Block  of 'x block reg
constraint 'x = 'x x_sig

and 'x single_instr =
  Cond     of 'x conditional reg
| Match    of 'x match_instr reg
| Ass      of 'x ass_instr
| Loop     of 'x loop
| ProcCall of 'x fun_call
| Null     of kwd_null
| Fail     of (kwd_fail * 'x expr) reg
constraint 'x = 'x x_sig

and 'x conditional = {
  kwd_if   :    kwd_if;
  test     : 'x expr;
  kwd_then :    kwd_then;
  ifso     : 'x instruction;
  kwd_else :    kwd_else;
  ifnot    : 'x instruction
}
constraint 'x = 'x x_sig

and 'x match_instr = {
  kwd_match :    kwd_match;
  expr      : 'x expr;
  kwd_with  :    kwd_with;
  lead_vbar :    vbar option;
  cases     : 'x cases;
  kwd_end   :    kwd_end
}
constraint 'x = 'x x_sig

and 'x cases = ('x case, vbar) nsepseq reg
constraint 'x = 'x x_sig

and 'x case = ('x pattern * arrow * 'x instruction) reg
constraint 'x = 'x x_sig

and 'x ass_instr = ('x variable * ass * 'x expr) reg
constraint 'x = 'x x_sig

and 'x loop =
  While of 'x while_loop
| For   of 'x for_loop
constraint 'x = 'x x_sig

and 'x while_loop = (kwd_while * 'x expr * 'x block reg) reg
constraint 'x = 'x x_sig

and 'x for_loop =
  ForInt     of 'x for_int reg
| ForCollect of 'x for_collect reg
constraint 'x = 'x x_sig

and 'x for_int = {
  kwd_for :    kwd_for;
  ass     : 'x ass_instr;
  down    :    kwd_down option;
  kwd_to  :    kwd_to;
  bound   : 'x expr;
  step    : (kwd_step * 'x expr) option;
  block   : 'x block reg
}
constraint 'x = 'x x_sig

and 'x for_collect = {
  kwd_for :    kwd_for;
  var     : 'x variable;
  bind_to : (arrow * 'x variable) option;
  kwd_in  :    kwd_in;
  expr    : 'x expr;
  block   : 'x block reg
}
constraint 'x = 'x x_sig

(* Expressions *)

and 'x expr =
  Or        of ('x expr * bool_or * 'x expr) reg
| And       of ('x expr * bool_and * 'x expr) reg
| Lt        of ('x expr * lt * 'x expr) reg
| Leq       of ('x expr * leq * 'x expr) reg
| Gt        of ('x expr * gt * 'x expr) reg
| Geq       of ('x expr * geq * 'x expr) reg
| Equal     of ('x expr * equal * 'x expr) reg
| Neq       of ('x expr * neq * 'x expr) reg
| Cat       of ('x expr * cat * 'x expr) reg
| Cons      of ('x expr * cons * 'x expr) reg
| Add       of ('x expr * plus * 'x expr) reg
| Sub       of ('x expr * minus * 'x expr) reg
| Mult      of ('x expr * times * 'x expr) reg
| Div       of ('x expr * slash * 'x expr) reg
| Mod       of ('x expr * kwd_mod * 'x expr) reg
| Neg       of (minus * 'x expr) reg
| Not       of (kwd_not * 'x expr) reg
| Int       of (Lexer.lexeme * Z.t) reg
| Var       of Lexer.lexeme reg
| String    of Lexer.lexeme reg
| Bytes     of (Lexer.lexeme * MBytes.t) reg
| False     of c_False
| True      of c_True
| Unit      of c_Unit
| Tuple     of 'x tuple
| List      of ('x expr, comma) nsepseq brackets
| EmptyList of 'x empty_list
| Set       of ('x expr, comma) nsepseq braces
| EmptySet  of 'x empty_set
| NoneExpr  of 'x none_expr
| FunCall   of 'x fun_call
| ConstrApp of 'x constr_app
| SomeApp   of (c_Some * 'x arguments) reg
| MapLookUp of 'x map_lookup reg
| ParExpr   of 'x expr par
constraint 'x = 'x x_sig

and 'x tuple = ('x expr, comma) nsepseq par
constraint 'x = 'x x_sig

and 'x empty_list =
  (lbracket * rbracket * colon * 'x type_expr) par
constraint 'x = 'x x_sig

and 'x empty_set =
  (lbrace * rbrace * colon * 'x type_expr) par
constraint 'x = 'x x_sig

and 'x none_expr =
  (c_None * colon * 'x type_expr) par
constraint 'x = 'x x_sig

and 'x fun_call = ('x fun_name * 'x arguments) reg
constraint 'x = 'x x_sig

and 'x arguments = 'x tuple
constraint 'x = 'x x_sig

and 'x constr_app = ('x constr * 'x arguments) reg
constraint 'x = 'x x_sig

and 'x map_lookup = {
  map_name : 'x variable;
  selector : dot;
  index    : 'x expr brackets
}
constraint 'x = 'x x_sig

(* Patterns *)

and 'x pattern = ('x core_pattern, cons) nsepseq reg
constraint 'x = 'x x_sig

and 'x core_pattern =
  PVar    of Lexer.lexeme reg
| PWild   of wild
| PInt    of (Lexer.lexeme * Z.t) reg
| PBytes  of (Lexer.lexeme * MBytes.t) reg
| PString of Lexer.lexeme reg
| PUnit   of c_Unit
| PFalse  of c_False
| PTrue   of c_True
| PNone   of c_None
| PSome   of (c_Some * 'x core_pattern par) reg
| PList   of 'x list_pattern
| PTuple  of ('x core_pattern, comma) nsepseq par
constraint 'x = 'x x_sig

and 'x list_pattern =
  Sugar of ('x core_pattern, comma) sepseq brackets
| Raw   of ('x core_pattern * cons * 'x pattern) par
constraint 'x = 'x x_sig

(* Variations on the AST *)

type parse_phase = <
  annot:               unit;
  type_expr_typecheck: tfalse;
>

type typecheck_phase = <
  annot:               typecheck_phase type_expr;
  type_expr_typecheck: ttrue;
>

type t = parse_phase ast

(* Projecting regions *)

open! Region

let type_expr_to_region : parse_phase type_expr -> region = function
  Prod    (Present node) -> node.region
| Sum     (Present node) -> node.region
| Record  (Present node) -> node.region
| TypeApp (Present node) -> node.region
| ParType (Present node) -> node.region
| TAlias  (Present node) -> node.region
| _ -> .

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

type 'x visitor = {
  ass_instr       : 'x ass_instr -> unit;
  bind_to         : (region * 'x variable) option -> unit;
  block           : 'x block reg -> unit;
  bytes           : (string * MBytes.t) reg -> unit;
  cartesian       : 'x cartesian -> unit;
  case            : 'x case -> unit;
  cases           : 'x cases -> unit;
  conditional     : 'x conditional -> unit;
  const_decl      : 'x const_decl reg -> unit;
  constr          : 'x constr -> unit;
  constr_app      : 'x constr_app -> unit;
  core_pattern    : 'x core_pattern -> unit;
  down            : region option -> unit;
  empty_list      : 'x empty_list -> unit;
  empty_set       : 'x empty_set -> unit;
  expr            : 'x expr -> unit;
  fail            : (kwd_fail * 'x expr) -> unit;
  field_decl      : 'x field_decl -> unit;
  field_decls     : 'x field_decls -> unit;
  for_collect     : 'x for_collect reg -> unit;
  for_int         : 'x for_int reg -> unit;
  for_loop        : 'x for_loop -> unit;
  fun_call        : 'x fun_call -> unit;
  fun_decl        : 'x fun_decl reg -> unit;
  instruction     : 'x instruction -> unit;
  instructions    : 'x instructions -> unit;
  int             : (string * Z.t) reg -> unit;
  lambda_decl     : 'x lambda_decl -> unit;
  list            : ('x expr, region) nsepseq brackets -> unit;
  list_pattern    : 'x list_pattern -> unit;
  loop            : 'x loop -> unit;
  map_lookup      : 'x map_lookup reg -> unit;
  match_instr     : 'x match_instr -> unit;
  none_expr       : 'x none_expr -> unit;
  nsepseq         : 'a.string -> ('a -> unit) -> ('a, region) nsepseq -> unit;
  operations_decl : 'x operations_decl reg -> unit;
  par_expr        : 'x expr par -> unit;
  par_type        : 'x type_expr par -> unit;
  param_decl      : 'x param_decl -> unit;
  parameter_decl  : 'x parameter_decl reg -> unit;
  parameters      : 'x parameters -> unit;
  param_const     : 'x param_const -> unit;
  param_var       : 'x param_var -> unit;
  pattern         : 'x pattern -> unit;
  patterns        : 'x core_pattern par -> unit;
  proc_decl       : 'x proc_decl reg -> unit;
  psome           : (region * 'x core_pattern par) reg -> unit;
  ptuple          : ('x core_pattern, region) nsepseq par -> unit;
  raw             : ('x core_pattern * region * 'x pattern) par -> unit;
  record_type     : 'x record_type -> unit;
  sepseq          : 'a.string -> ('a -> unit) -> ('a, region) sepseq -> unit;
  set             : ('x expr, region) nsepseq braces -> unit;
  single_instr    : 'x single_instr -> unit;
  some_app        : (region * 'x arguments) reg -> unit;
  step            : (region * 'x expr) option -> unit;
  storage_decl    : 'x storage_decl reg -> unit;
  string          : string reg -> unit;
  sugar           : ('x core_pattern, region) sepseq brackets -> unit;
  sum_type        : ('x variant, region) nsepseq reg -> unit;
  terminator      : semi option -> unit;
  token           : region -> string -> unit;
  tuple           : 'x arguments -> unit;
  type_app        : ('x type_name * 'x type_tuple) reg -> unit;
  type_decl       : 'x type_decl reg -> unit;
  type_expr       : 'x type_expr -> unit;
  type_tuple      : 'x type_tuple -> unit;
  local_decl      : 'x local_decl -> unit;
  local_decls     : 'x local_decl list -> unit;
  var             : 'x variable -> unit;
  var_decl        : 'x var_decl reg -> unit;
  variant         : 'x variant -> unit;
  while_loop      : 'x while_loop -> unit
}
