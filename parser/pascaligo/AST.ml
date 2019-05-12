(* Abstract Syntax Tree (AST) for LIGO *)

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

(* Keywords of LIGO *)

type keyword        = Region.t
type kwd_and        = Region.t
type kwd_begin      = Region.t
type kwd_block      = Region.t
type kwd_case       = Region.t
type kwd_const      = Region.t
type kwd_contains   = Region.t
type kwd_down       = Region.t
type kwd_else       = Region.t
type kwd_end        = Region.t
type kwd_entrypoint = Region.t
type kwd_fail       = Region.t
type kwd_for        = Region.t
type kwd_from       = Region.t
type kwd_function   = Region.t
type kwd_if         = Region.t
type kwd_in         = Region.t
type kwd_is         = Region.t
type kwd_list       = Region.t
type kwd_map        = Region.t
type kwd_mod        = Region.t
type kwd_nil        = Region.t
type kwd_not        = Region.t
type kwd_of         = Region.t
type kwd_or         = Region.t
type kwd_patch      = Region.t
type kwd_procedure  = Region.t
type kwd_record     = Region.t
type kwd_remove     = Region.t
type kwd_set        = Region.t
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
type assign   = Region.t
type equal    = Region.t
type colon    = Region.t
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
type set_name   = string reg
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
  TypeDecl   of type_decl reg
| ConstDecl  of const_decl reg
| LambdaDecl of lambda_decl

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
  TProd   of cartesian
| TSum    of (variant reg, vbar) nsepseq reg
| TRecord of record_type
| TApp    of (type_name * type_tuple) reg
| TFun    of (type_expr * arrow  * type_expr) reg
| TPar    of type_expr par reg
| TAlias  of variable

and cartesian = (type_expr, times) nsepseq reg

and variant = {
  constr  : constr;
  kwd_of  : kwd_of;
  product : cartesian
}

and record_type = field_decl reg injection reg

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
  opening    : block_opening;
  statements : statements;
  terminator : semi option;
  closing    : block_closing
}

and block_opening =
  Block of kwd_block * lbrace
| Begin of kwd_begin

and block_closing =
  Block of rbrace
| End   of kwd_end

and statements = (statement, semi) nsepseq

and statement =
  Instr of instruction
| Data  of data_decl

and local_decl =
  LocalLam   of lambda_decl
| LocalData  of data_decl

and data_decl =
  LocalConst of const_decl reg
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

and instruction =
  Single of single_instr
| Block  of block reg

and single_instr =
  Cond        of conditional reg
| CaseInstr   of instruction case reg
| Assign      of assignment reg
| Loop        of loop
| ProcCall    of fun_call
| Fail        of fail_instr reg
| Skip        of kwd_skip
| RecordPatch of record_patch reg
| MapPatch    of map_patch reg
| SetPatch    of set_patch reg
| MapRemove   of map_remove reg
| SetRemove   of set_remove reg

and set_remove = {
  kwd_remove : kwd_remove;
  element    : expr;
  kwd_from   : kwd_from;
  kwd_set    : kwd_set;
  set        : path
}

and map_remove = {
  kwd_remove : kwd_remove;
  key        : expr;
  kwd_from   : kwd_from;
  kwd_map    : kwd_map;
  map        : path
}

and set_patch  = {
  kwd_patch : kwd_patch;
  path      : path;
  kwd_with  : kwd_with;
  set_inj   : expr injection reg
}

and map_patch  = {
  kwd_patch : kwd_patch;
  path      : path;
  kwd_with  : kwd_with;
  map_inj   : binding reg injection reg
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
  record_inj : record_expr
}

and fail_instr = {
  kwd_fail  : kwd_fail;
  fail_expr : expr
}

and conditional = {
  kwd_if     : kwd_if;
  test       : expr;
  kwd_then   : kwd_then;
  ifso       : if_clause;
  terminator : semi option;
  kwd_else   : kwd_else;
  ifnot      : if_clause
}

and if_clause =
  ClauseInstr of instruction
| ClauseBlock of (statements * semi option) braces reg

and set_membership = {
  set          : expr;
  kwd_contains : kwd_contains;
  element      : expr
}

and 'a case = {
  kwd_case  : kwd_case;
  expr      : expr;
  opening   : opening;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg;
  closing   : closing
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and assignment = {
  lhs    : lhs;
  assign : assign;
  rhs    : rhs
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
| ECase   of expr case reg
| EAnnot  of annot_expr reg
| ELogic  of logic_expr
| EArith  of arith_expr
| EString of string_expr
| EList   of list_expr
| ESet    of set_expr
| EConstr of constr_expr
| ERecord of record_expr
| EProj   of projection reg
| EMap    of map_expr
| EVar    of Lexer.lexeme reg
| ECall   of fun_call
| EBytes  of (Lexer.lexeme * Hex.t) reg
| EUnit   of c_Unit
| ETuple  of tuple_expr
| EPar    of expr par reg

and annot_expr = (expr * type_expr)

and set_expr =
  SetInj of expr injection reg
| SetMem of set_membership reg

and 'a injection = {
  opening    : opening;
  elements   : ('a, semi) sepseq;
  terminator : semi option;
  closing    : closing
}

and opening =
  Kwd        of keyword
| KwdBracket of keyword * lbracket

and closing =
  End       of kwd_end
| RBracket  of rbracket

and map_expr =
  MapLookUp of map_lookup reg
| MapInj    of binding reg injection reg

and map_lookup = {
  path  : path;
  index : expr brackets reg
}

and path =
  Name of variable
| Path of projection reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of kwd_or  bin_op reg
| And   of kwd_and bin_op reg
| Not   of kwd_not un_op reg
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
| Nat  of (Lexer.lexeme * Z.t) reg
| Mtz  of (Lexer.lexeme * Z.t) reg

and string_expr =
  Cat    of cat bin_op reg
| String of Lexer.lexeme reg

and list_expr =
  Cons of cons bin_op reg
| List of expr injection reg
| Nil  of nil

and nil = kwd_nil

and constr_expr =
  SomeApp   of (c_Some * arguments) reg
| NoneExpr  of none_expr
| ConstrApp of (constr * arguments) reg

and record_expr = field_assign reg injection reg

and field_assign = {
  field_name : field_name;
  equal      : equal;
  field_expr : expr
}

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and selection =
  FieldName of field_name
| Component of (Lexer.lexeme * Z.t) reg

and tuple_expr =
  TupleInj of tuple_injection

and tuple_injection = (expr, comma) nsepseq par reg

and none_expr = c_None

and fun_call = (fun_name * arguments) reg

and arguments = tuple_injection

(* Patterns *)

and pattern =
  PCons   of (pattern, cons) nsepseq reg
| PConstr of (constr * pattern reg) reg
| PVar    of Lexer.lexeme reg
| PWild   of wild
| PInt    of (Lexer.lexeme * Z.t) reg
| PBytes  of (Lexer.lexeme * Hex.t) reg
| PString of Lexer.lexeme reg
| PUnit   of c_Unit
| PFalse  of c_False
| PTrue   of c_True
| PNone   of c_None
| PSome   of (c_Some * pattern par reg) reg
| PList   of list_pattern
| PTuple  of (pattern, comma) nsepseq par reg

and list_pattern =
  Sugar of pattern injection reg
| PNil  of kwd_nil
| Raw   of (pattern * cons * pattern) par reg

(* Projecting regions *)

open! Region

let type_expr_to_region = function
  TProd   {region; _}
| TSum    {region; _}
| TRecord {region; _}
| TApp    {region; _}
| TFun    {region; _}
| TPar    {region; _}
| TAlias  {region; _} -> region

let rec expr_to_region = function
| ELogic  e -> logic_expr_to_region e
| EArith  e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EAnnot  e -> annot_expr_to_region e
| EList   e -> list_expr_to_region e
| ESet    e -> set_expr_to_region e
| EConstr e -> constr_expr_to_region e
| ERecord e -> record_expr_to_region e
| EMap    e -> map_expr_to_region e
| ETuple  e -> tuple_expr_to_region e
| EProj  {region; _}
| EVar   {region; _}
| ECall  {region; _}
| EBytes {region; _}
| EUnit   region
| ECase  {region;_}
| EPar   {region; _} -> region

and tuple_expr_to_region = function
  TupleInj {region; _} -> region

and map_expr_to_region = function
  MapLookUp {region; _}
| MapInj    {region; _} -> region

and set_expr_to_region = function
  SetInj {region; _}
| SetMem {region; _} -> region

and logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

and bool_expr_to_region = function
  Or    {region; _}
| And   {region; _}
| Not   {region; _}
| False region
| True  region -> region

and comp_expr_to_region = function
  Lt    {region; _}
| Leq   {region; _}
| Gt    {region; _}
| Geq   {region; _}
| Equal {region; _}
| Neq   {region; _} -> region

and arith_expr_to_region = function
| Add  {region; _}
| Sub  {region; _}
| Mult {region; _}
| Div  {region; _}
| Mod  {region; _}
| Neg  {region; _}
| Int  {region; _}
| Nat  {region; _}
| Mtz  {region; _} -> region

and string_expr_to_region = function
  Cat    {region; _}
| String {region; _} -> region

and annot_expr_to_region ({region; _}) = region

and list_expr_to_region = function
  Cons {region; _}
| List {region; _}
| Nil  region -> region

and constr_expr_to_region = function
  NoneExpr  region
| ConstrApp {region; _}
| SomeApp   {region; _} -> region

and record_expr_to_region {region; _} = region

let path_to_region = function
  Name var -> var.region
| Path {region; _} -> region

let instr_to_region = function
  Single Cond                {region; _}
| Single CaseInstr           {region; _}
| Single Assign              {region; _}
| Single Loop While          {region; _}
| Single Loop For ForInt     {region; _}
| Single Loop For ForCollect {region; _}
| Single ProcCall            {region; _}
| Single Skip                region
| Single Fail                {region; _}
| Single RecordPatch         {region; _}
| Single MapPatch            {region; _}
| Single SetPatch            {region; _}
| Single MapRemove           {region; _}
| Single SetRemove           {region; _}
| Block                      {region; _} -> region

let if_clause_to_region = function
  ClauseInstr instr       -> instr_to_region instr
| ClauseBlock {region; _} -> region

let pattern_to_region = function
  PCons       {region; _}
| PVar        {region; _}
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
| PList PNil  region
| PList Raw   {region; _}
| PConstr     {region; _}
| PTuple      {region; _} -> region

let local_decl_to_region = function
  LocalLam FunDecl     {region; _}
| LocalLam ProcDecl    {region; _}
| LocalLam EntryDecl   {region; _}
| LocalData LocalConst {region; _}
| LocalData LocalVar   {region; _} -> region

let lhs_to_region : lhs -> Region.t = function
  Path path -> path_to_region path
| MapPath {region; _} -> region

let rhs_to_region = function
      Expr e -> expr_to_region e
| NoneExpr r -> r

let selection_to_region = function
  FieldName {region; _}
| Component {region; _} -> region
