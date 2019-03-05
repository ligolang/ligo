[@@@warning "-30"]

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

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd,tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd,tl) =
  let reg (_,item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let sepseq_to_region to_region = function
      None -> Region.ghost
| Some seq -> nsepseq_to_region to_region seq

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

type t = {
  types      : type_decl list;
  parameter  : parameter_decl;
  storage    : storage_decl;
  operations : operations_decl;
  lambdas    : lambda_decl list;
  block      : block reg;
  eof        : eof
}

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

and fun_decl = {
  kwd_function : kwd_function;
  var          : variable;
  param        : parameters;
  colon        : colon;
  ret_type     : type_expr;
  kwd_is       : kwd_is;
  body         : block reg;
  kwd_with     : kwd_with;
  return       : expr
}

and proc_decl = {
  kwd_procedure : kwd_procedure;
  var           : variable;
  param         : parameters;
  kwd_is        : kwd_is;
  body          : block reg
}

and parameters = (param_decl, semi) nsepseq par

and param_decl = (var_kind * variable * colon * type_expr) reg

and var_kind =
  Mutable of kwd_var
| Const   of kwd_const

and block = {
  decls   : value_decls;
  opening : kwd_begin;
  instr   : instructions;
  close   : kwd_end
}

and value_decls = (var_decl reg, semi) sepseq reg

and var_decl = {
  kind   : var_kind;
  var    : variable;
  colon  : colon;
  vtype  : type_expr;
  setter : Region.t;  (* "=" or ":=" *)
  init   : expr
}

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

and conditional = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : instruction;
  kwd_else : kwd_else;
  ifnot    : instruction
}

and match_instr = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  cases     : cases;
  kwd_end   : kwd_end
}

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

and for_int = {
  kwd_for : kwd_for;
  asgnmnt : asgnmnt_instr;
  down    : kwd_down option;
  kwd_to  : kwd_to;
  bound   : expr;
  step    : (kwd_step * expr) option;
  block   : block reg
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

and map_lookup = {
  map_name : variable;
  selector : dot;
  index    : expr brackets
}

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

open Region

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

let var_kind_to_region = function
  Mutable region
| Const   region -> region

let instr_to_region = function
  Single Cond                {region;_}
| Single Match               {region; _}
| Single Asgnmnt             {region; _}
| Single Loop While          {region; _}
| Single Loop For ForInt     {region; _}
| Single Loop For ForCollect {region; _}
| Single ProcCall            {region; _}
| Single Null                 region
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

(* Printing the tokens with their source regions *)

type xyz = {
  asgnmnt_instr : asgnmnt_instr -> unit;
  bind_to : (region * variable) option -> unit;
  block : block reg -> unit;
  bytes : (string * MBytes.t) reg -> unit;
  cartesian : cartesian -> unit;
  case : case -> unit;
  cases : cases -> unit;
  conditional : conditional -> unit;
  constr : constr -> unit;
  constr_app : constr_app -> unit;
  core_pattern : core_pattern -> unit;
  down : region option -> unit;
  empty_list : empty_list -> unit;
  empty_set : empty_set -> unit;
  expr : expr -> unit;
  field_decl : field_decl -> unit;
  field_decls : field_decls -> unit;
  for_collect : for_collect reg -> unit;
  for_int : for_int reg -> unit;
  for_loop : for_loop -> unit;
  fun_call : fun_call -> unit;
  fun_decl : fun_decl reg -> unit;
  instruction : instruction -> unit;
  instructions : instructions -> unit;
  int : (string * Z.t) reg -> unit;
  lambda_decl : lambda_decl -> unit;
  list : (expr, region) nsepseq brackets -> unit;
  list_pattern : list_pattern -> unit;
  loop : loop -> unit;
  map_lookup : map_lookup reg -> unit;
  match_instr : match_instr -> unit;
  none_expr : none_expr -> unit;
  nsepseq : 'a. string -> ('a -> unit) -> 'a * (region * 'a) list -> unit;
  operations_decl : (region * type_expr) reg -> unit;
  par_expr : expr par -> unit;
  par_type : type_expr par -> unit;
  param_decl : param_decl -> unit;
  parameter_decl : (region * variable * region * type_expr) reg -> unit;
  parameters : parameters -> unit;
  pattern : pattern -> unit;
  patterns : core_pattern par -> unit;
  proc_decl : proc_decl reg -> unit;
  psome : (region * core_pattern par) reg -> unit;
  ptuple : (core_pattern, region) nsepseq par -> unit;
  raw : (core_pattern * region * pattern) par -> unit;
  record_type : record_type -> unit;
  sepseq : 'a.
           string ->
           ('a -> unit) -> ('a * (region * 'a) list) option -> unit;
  set : (expr, region) nsepseq braces -> unit;
  single_instr : single_instr -> unit;
  some_app : (region * arguments) reg -> unit;
  step : (region * expr) option -> unit;
  storage_decl : (region * type_expr) reg -> unit;
  string : string reg -> unit;
  sugar : (core_pattern, region) sepseq brackets -> unit;
  sum_type : (variant, region) nsepseq reg -> unit;
  token : region -> string -> unit;
  tuple : arguments -> unit;
  type_app : (type_name * type_tuple) reg -> unit;
  type_decl : (region * variable * region * type_expr) reg -> unit;
  type_expr : type_expr -> unit;
  type_tuple : type_tuple -> unit;
  value_decls : value_decls -> unit;
  var : variable -> unit;
  var_decl : var_decl reg -> unit;
  var_kind : var_kind -> unit;
  variant : variant -> unit;
  while_loop : while_loop -> unit
}

let printf = Printf.printf

let compact (region: Region.t) =
  region#compact ~offsets:EvalOpt.offsets EvalOpt.mode

let rec print_nsepseq : 'a . string -> ('a -> unit) -> ('a * (Region.t * 'a) list) -> unit = fun sep visit (head,tail) ->
  let print_aux (sep_reg, item) =
    printf "%s: %s\n" (compact sep_reg) sep;
    visit item
  in visit head; List.iter print_aux tail

and print_sepseq : 'a . string -> ('a -> unit) -> ('a * (Region.t * 'a) list) option -> unit = fun sep visit -> function
      None -> ()
| Some seq -> print_nsepseq sep visit seq

and print_token (_visitor : xyz) region lexeme =
  printf "%s: %s\n"(compact region) lexeme

and print_var (_visitor : xyz) {region; value=lexeme} =
  printf "%s: Ident \"%s\"\n" (compact region) lexeme

and print_constr (_visitor : xyz) {region; value=lexeme} =
  printf "%s: Constr \"%s\"\n"
         (compact region) lexeme

and print_string (_visitor : xyz) {region; value=lexeme} =
  printf "%s: String \"%s\"\n"
         (compact region) lexeme

and print_bytes (_visitor : xyz) {region; value = lexeme, abstract} =
  printf "%s: Bytes (\"%s\", \"0x%s\")\n"
         (compact region) lexeme
         (MBytes.to_hex abstract |> Hex.to_string)

and print_int (_visitor : xyz) {region; value = lexeme, abstract} =
  printf "%s: Int (\"%s\", %s)\n"
         (compact region) lexeme
         (Z.to_string abstract)

(* main print function *)
and print_tokens (visitor : xyz) ast =
  List.iter visitor.type_decl   ast.types;
  visitor.parameter_decl        ast.parameter;
  visitor.storage_decl          ast.storage;
  visitor.operations_decl       ast.operations;
  List.iter visitor.lambda_decl ast.lambdas;
  visitor.block                 ast.block;
  visitor.token                 ast.eof "EOF"

and print_parameter_decl (visitor : xyz) {value=node; _} =
  let kwd_parameter, variable, colon, type_expr = node in
  visitor.token     kwd_parameter "parameter";
  visitor.var       variable;
  visitor.token     colon ":";
  visitor.type_expr type_expr

and print_storage_decl (visitor : xyz) {value=node; _} =
  let kwd_storage, type_expr = node in
  visitor.token     kwd_storage "storage";
  visitor.type_expr type_expr

and print_operations_decl (visitor : xyz) {value=node; _} =
  let kwd_operations, type_expr = node in
  visitor.token     kwd_operations "operations";
  visitor.type_expr type_expr

and print_type_decl (visitor : xyz) {value=node; _} =
  let kwd_type, type_name, kwd_is, type_expr = node in
  visitor.token     kwd_type "type";
  visitor.var       type_name;
  visitor.token     kwd_is "is";
  visitor.type_expr type_expr

and print_type_expr (visitor : xyz) = function
  Prod    cartesian   -> visitor.cartesian   cartesian
| Sum     sum_type    -> visitor.sum_type    sum_type
| Record  record_type -> visitor.record_type record_type
| TypeApp type_app    -> visitor.type_app    type_app
| ParType par_type    -> visitor.par_type    par_type
| TAlias  type_alias  -> visitor.var         type_alias

and print_cartesian (visitor : xyz) {value=sequence; _} =
  visitor.nsepseq "*" visitor.type_expr sequence

and print_variant (visitor : xyz) {value=node; _} =
  let constr, kwd_of, cartesian = node in
  visitor.constr    constr;
  visitor.token     kwd_of "of";
  visitor.cartesian cartesian

and print_sum_type (visitor : xyz) {value=sequence; _} =
  visitor.nsepseq "|" visitor.variant sequence

and print_record_type (visitor : xyz) {value=node; _} =
  let kwd_record, field_decls, kwd_end = node in
  visitor.token       kwd_record "record";
  visitor.field_decls field_decls;
  visitor.token       kwd_end "end"

and print_type_app (visitor : xyz) {value=node; _} =
  let type_name, type_tuple = node in
  visitor.var        type_name;
  visitor.type_tuple type_tuple

and print_par_type (visitor : xyz) {value=node; _} =
  let lpar, type_expr, rpar = node in
  visitor.token     lpar "(";
  visitor.type_expr type_expr;
  visitor.token     rpar ")"

and print_field_decls (visitor : xyz) sequence =
  visitor.nsepseq ";" visitor.field_decl sequence

and print_field_decl (visitor : xyz) {value=node; _} =
  let var, colon, type_expr = node in
  visitor.var       var;
  visitor.token     colon ":";
  visitor.type_expr type_expr

and print_type_tuple (visitor : xyz) {value=node; _} =
  let lpar, sequence, rpar = node in
  visitor.token lpar "(";
  visitor.nsepseq "," visitor.var sequence;
  visitor.token rpar ")"

and print_lambda_decl (visitor : xyz) = function
  FunDecl   fun_decl -> visitor.fun_decl fun_decl
| ProcDecl proc_decl -> visitor.proc_decl proc_decl

and print_fun_decl (visitor : xyz) {value=node; _} =
  visitor.token      node.kwd_function "function";
  visitor.var        node.var;
  visitor.parameters node.param;
  visitor.token      node.colon ":";
  visitor.type_expr  node.ret_type;
  visitor.token      node.kwd_is "is";
  visitor.block      node.body;
  visitor.token      node.kwd_with "with";
  visitor.expr       node.return

and print_proc_decl (visitor : xyz) {value=node; _} =
  visitor.token      node.kwd_procedure "procedure";
  visitor.var        node.var;
  visitor.parameters node.param;
  visitor.token      node.kwd_is "is";
  visitor.block      node.body

and print_parameters (visitor : xyz) {value=node; _} =
  let lpar, sequence, rpar = node in
  visitor.token lpar "(";
  visitor.nsepseq ";" visitor.param_decl sequence;
  visitor.token rpar ")"

and print_param_decl (visitor : xyz) {value=node; _} =
  let var_kind, variable, colon, type_expr = node in
  visitor.var_kind  var_kind;
  visitor.var       variable;
  visitor.token     colon ":";
  visitor.type_expr type_expr

and print_var_kind (visitor : xyz) = function
  Mutable kwd_var -> visitor.token kwd_var "var"
| Const kwd_const -> visitor.token kwd_const "const"

and print_block (visitor : xyz) {value=node; _} =
  visitor.value_decls  node.decls;
  visitor.token        node.opening "begin";
  visitor.instructions node.instr;
  visitor.token        node.close "end"

and print_value_decls (visitor : xyz) {value=sequence; _} =
  visitor.sepseq ";" visitor.var_decl sequence

and print_var_decl (visitor : xyz) {value=node; _} =
  let setter =
    match node.kind with
      Mutable _ -> ":="
    |   Const _ -> "=" in
  visitor.var_kind  node.kind;
  visitor.var       node.var;
  visitor.token     node.colon ":";
  visitor.type_expr node.vtype;
  visitor.token     node.setter setter;
  visitor.expr      node.init

and print_instructions (visitor : xyz) {value=sequence; _} =
  visitor.nsepseq ";" visitor.instruction sequence

and print_instruction (visitor : xyz) = function
  Single instr -> visitor.single_instr instr
|  Block block -> visitor.block block

and print_single_instr (visitor : xyz) = function
  Cond     {value; _} -> visitor.conditional value
| Match    {value; _} -> visitor.match_instr value
| Asgnmnt  instr      -> visitor.asgnmnt_instr instr
| Loop     loop       -> visitor.loop loop
| ProcCall fun_call   -> visitor.fun_call fun_call
| Null     kwd_null   -> visitor.token kwd_null "null"

and print_conditional (visitor : xyz) node =
  visitor.token       node.kwd_if "if";
  visitor.expr        node.test;
  visitor.token       node.kwd_then "then";
  visitor.instruction node.ifso;
  visitor.token       node.kwd_else "else";
  visitor.instruction node.ifnot

and print_match_instr (visitor : xyz) node =
  visitor.token node.kwd_match "match";
  visitor.expr  node.expr;
  visitor.token node.kwd_with "with";
  visitor.cases node.cases;
  visitor.token node.kwd_end "end"

and print_cases (visitor : xyz) {value=sequence; _} =
  visitor.nsepseq "|" visitor.case sequence

and print_case (visitor : xyz) {value=node; _} =
  let pattern, arrow, instruction = node in
  visitor.pattern pattern;
  visitor.token arrow "->";
  visitor.instruction instruction

and print_asgnmnt_instr (visitor : xyz) {value=node; _} =
  let variable, asgnmnt, expr = node in
  visitor.var variable;
  visitor.token asgnmnt ":=";
  visitor.expr expr

and print_loop (visitor : xyz) = function
  While while_loop -> visitor.while_loop while_loop
| For     for_loop -> visitor.for_loop for_loop

and print_while_loop (visitor : xyz) {value=node; _} =
  let kwd_while, expr, block = node in
  visitor.token kwd_while "while";
  visitor.expr expr;
  visitor.block block

and print_for_loop (visitor : xyz) = function
  ForInt     for_int     -> visitor.for_int for_int
| ForCollect for_collect -> visitor.for_collect for_collect

and print_for_int (visitor : xyz) ({value=node; _} : for_int reg) =
  visitor.token         node.kwd_for "for";
  visitor.asgnmnt_instr node.asgnmnt;
  visitor.down          node.down;
  visitor.token         node.kwd_to "to";
  visitor.expr          node.bound;
  visitor.step          node.step;
  visitor.block         node.block

and print_down (visitor : xyz) = function
  Some kwd_down -> visitor.token kwd_down "down"
| None -> ()

and print_step (visitor : xyz) = function
  Some (kwd_step, expr) ->
    visitor.token kwd_step "step";
    visitor.expr expr
| None -> ()

and print_for_collect (visitor : xyz) ({value=node; _} : for_collect reg) =
  visitor.token   node.kwd_for "for";
  visitor.var     node.var;
  visitor.bind_to node.bind_to;
  visitor.token   node.kwd_in "in";
  visitor.expr    node.expr;
  visitor.block   node.block

and print_bind_to (visitor : xyz) = function
  Some (arrow, variable) ->
    visitor.token arrow "->";
    visitor.var   variable
| None -> ()

and print_expr (visitor : xyz) = function
  Or {value = expr1, bool_or, expr2; _} ->
    visitor.expr expr1; visitor.token bool_or "||"; visitor.expr expr2
| And {value = expr1, bool_and, expr2; _} ->
    visitor.expr expr1; visitor.token bool_and "&&"; visitor.expr expr2
| Lt {value = expr1, lt, expr2; _} ->
    visitor.expr expr1; visitor.token lt "<"; visitor.expr expr2
| Leq {value = expr1, leq, expr2; _} ->
    visitor.expr expr1; visitor.token leq "<="; visitor.expr expr2
| Gt {value = expr1, gt, expr2; _} ->
    visitor.expr expr1; visitor.token gt ">"; visitor.expr expr2
| Geq {value = expr1, geq, expr2; _} ->
    visitor.expr expr1; visitor.token geq ">="; visitor.expr expr2
| Equal {value = expr1, equal, expr2; _} ->
    visitor.expr expr1; visitor.token equal "="; visitor.expr expr2
| Neq {value = expr1, neq, expr2; _} ->
    visitor.expr expr1; visitor.token neq "=/="; visitor.expr expr2
| Cat {value = expr1, cat, expr2; _} ->
    visitor.expr expr1; visitor.token cat "^"; visitor.expr expr2
| Cons {value = expr1, cons, expr2; _} ->
    visitor.expr expr1; visitor.token cons "<:"; visitor.expr expr2
| Add {value = expr1, add, expr2; _} ->
    visitor.expr expr1; visitor.token add "+"; visitor.expr expr2
| Sub {value = expr1, sub, expr2; _} ->
    visitor.expr expr1; visitor.token sub "-"; visitor.expr expr2
| Mult {value = expr1, mult, expr2; _} ->
    visitor.expr expr1; visitor.token mult "*"; visitor.expr expr2
| Div {value = expr1, div, expr2; _} ->
    visitor.expr expr1; visitor.token div "/"; visitor.expr expr2
| Mod {value = expr1, kwd_mod, expr2; _} ->
    visitor.expr expr1; visitor.token kwd_mod "mod"; visitor.expr expr2
| Neg {value = minus, expr; _} ->
    visitor.token minus "-"; visitor.expr expr
| Not {value = kwd_not, expr; _} ->
    visitor.token kwd_not "not"; visitor.expr expr
| Int i            -> visitor.int i
| Var v            -> visitor.var v
| String s         -> visitor.string s
| Bytes b          -> visitor.bytes b
| False region     -> visitor.token region "False"
| True region      -> visitor.token region "True"
| Unit region      -> visitor.token region "Unit"
| Tuple tuple      -> visitor.tuple tuple
| List list        -> visitor.list list
| EmptyList elist  -> visitor.empty_list elist
| Set set          -> visitor.set set
| EmptySet eset    -> visitor.empty_set eset
| NoneExpr nexpr   -> visitor.none_expr nexpr
| FunCall fun_call -> visitor.fun_call fun_call
| ConstrApp capp   -> visitor.constr_app capp
| SomeApp sapp     -> visitor.some_app sapp
| MapLookUp lookup -> visitor.map_lookup lookup
| ParExpr pexpr    -> visitor.par_expr pexpr

and print_tuple (visitor : xyz) {value=node; _} =
  let lpar, sequence, rpar = node in
  visitor.token lpar "(";
  visitor.nsepseq "," visitor.expr sequence;
  visitor.token rpar ")"

and print_list (visitor : xyz) {value=node; _} =
  let lbra, sequence, rbra = node in
  visitor.token lbra "[";
  visitor.nsepseq "," visitor.expr sequence;
  visitor.token rbra "]"

and print_empty_list (visitor : xyz) {value=node; _} =
  let lpar, (lbracket, rbracket, colon, type_expr), rpar = node in
  visitor.token     lpar "(";
  visitor.token     lbracket "[";
  visitor.token     rbracket "]";
  visitor.token     colon ":";
  visitor.type_expr type_expr;
  visitor.token     rpar ")"

and print_set (visitor : xyz) {value=node; _} =
  let lbrace, sequence, rbrace = node in
  visitor.token lbrace "{";
  visitor.nsepseq "," visitor.expr sequence;
  visitor.token rbrace "}"

and print_empty_set (visitor : xyz) {value=node; _} =
  let lpar, (lbrace, rbrace, colon, type_expr), rpar = node in
  visitor.token     lpar "(";
  visitor.token     lbrace "{";
  visitor.token     rbrace "}";
  visitor.token     colon ":";
  visitor.type_expr type_expr;
  visitor.token     rpar ")"

and print_none_expr (visitor : xyz) {value=node; _} =
  let lpar, (c_None, colon, type_expr), rpar = node in
  visitor.token     lpar "(";
  visitor.token     c_None "None";
  visitor.token     colon ":";
  visitor.type_expr type_expr;
  visitor.token     rpar ")"

and print_fun_call (visitor : xyz) {value=node; _} =
  let fun_name, arguments = node in
  visitor.var   fun_name;
  visitor.tuple arguments

and print_constr_app (visitor : xyz) {value=node; _} =
  let constr, arguments = node in
  visitor.constr constr;
  visitor.tuple  arguments

and print_some_app (visitor : xyz) {value=node; _} =
  let c_Some, arguments = node in
  visitor.token c_Some "Some";
  visitor.tuple arguments

and print_map_lookup (visitor : xyz) {value=node; _} =
  let {value = lbracket, expr, rbracket; _} = node.index in
  visitor.var   node.map_name;
  visitor.token node.selector ".";
  visitor.token lbracket "[";
  visitor.expr  expr;
  visitor.token rbracket "]"

and print_par_expr (visitor : xyz) {value=node; _} =
  let lpar, expr, rpar = node in
  visitor.token lpar "(";
  visitor.expr  expr;
  visitor.token rpar ")"

and print_pattern (visitor : xyz) {value=sequence; _} =
  visitor.nsepseq "<:" visitor.core_pattern sequence

and print_core_pattern (visitor : xyz) = function
  PVar var      -> visitor.var var
| PWild wild    -> visitor.token wild "_"
| PInt i        -> visitor.int i
| PBytes b      -> visitor.bytes b
| PString s     -> visitor.string s
| PUnit region  -> visitor.token region "Unit"
| PFalse region -> visitor.token region "False"
| PTrue region  -> visitor.token region "True"
| PNone region  -> visitor.token region "None"
| PSome psome   -> visitor.psome psome
| PList pattern -> visitor.list_pattern pattern
| PTuple ptuple -> visitor.ptuple ptuple

and print_psome (visitor : xyz) {value=node; _} =
  let c_Some, patterns = node in
  visitor.token    c_Some "Some";
  visitor.patterns patterns

and print_patterns (visitor : xyz) {value=node; _} =
  let lpar, core_pattern, rpar = node in
  visitor.token lpar "(";
  visitor.core_pattern core_pattern;
  visitor.token rpar ")"

and print_list_pattern (visitor : xyz) = function
  Sugar sugar -> visitor.sugar sugar
| Raw     raw -> visitor.raw raw

and print_sugar (visitor : xyz) {value=node; _} =
  let lbracket, sequence, rbracket = node in
  visitor.token lbracket "[";
  visitor.sepseq "," visitor.core_pattern sequence;
  visitor.token rbracket "]"

and print_raw (visitor : xyz) {value=node; _} =
  let lpar, (core_pattern, cons, pattern), rpar = node in
  visitor.token        lpar "(";
  visitor.core_pattern core_pattern;
  visitor.token        cons "<:";
  visitor.pattern      pattern;
  visitor.token        rpar ")"

and print_ptuple (visitor : xyz) {value=node; _} =
  let lpar, sequence, rpar = node in
  visitor.token lpar "(";
  visitor.nsepseq "," visitor.core_pattern sequence;
  visitor.token rpar ")"

let rec visitor () : xyz = {
    nsepseq         = print_nsepseq; (* : 'a . string -> ('a -> unit) -> ('a * (Region.t * 'a) list)        -> unit *)
    sepseq          = print_sepseq; (*  : 'a . string -> ('a -> unit) -> ('a * (Region.t * 'a) list) option -> unit *)
    token           = print_token           (visitor ());
    var             = print_var             (visitor ());
    constr          = print_constr          (visitor ());
    string          = print_string          (visitor ());
    bytes           = print_bytes           (visitor ());
    int             = print_int             (visitor ());

    parameter_decl  = print_parameter_decl  (visitor ());
    storage_decl    = print_storage_decl    (visitor ());
    operations_decl = print_operations_decl (visitor ());
    type_decl       = print_type_decl       (visitor ());
    type_expr       = print_type_expr       (visitor ());
    cartesian       = print_cartesian       (visitor ());
    variant         = print_variant         (visitor ());
    sum_type        = print_sum_type        (visitor ());
    record_type     = print_record_type     (visitor ());
    type_app        = print_type_app        (visitor ());
    par_type        = print_par_type        (visitor ());
    field_decls     = print_field_decls     (visitor ());
    field_decl      = print_field_decl      (visitor ());
    type_tuple      = print_type_tuple      (visitor ());
    lambda_decl     = print_lambda_decl     (visitor ());
    fun_decl        = print_fun_decl        (visitor ());
    proc_decl       = print_proc_decl       (visitor ());
    parameters      = print_parameters      (visitor ());
    param_decl      = print_param_decl      (visitor ());
    var_kind        = print_var_kind        (visitor ());
    block           = print_block           (visitor ());
    value_decls     = print_value_decls     (visitor ());
    var_decl        = print_var_decl        (visitor ());
    instructions    = print_instructions    (visitor ());
    instruction     = print_instruction     (visitor ());
    single_instr    = print_single_instr    (visitor ());
    conditional     = print_conditional     (visitor ());
    match_instr     = print_match_instr     (visitor ());
    cases           = print_cases           (visitor ());
    case            = print_case            (visitor ());
    asgnmnt_instr   = print_asgnmnt_instr   (visitor ());
    loop            = print_loop            (visitor ());
    while_loop      = print_while_loop      (visitor ());
    for_loop        = print_for_loop        (visitor ());
    for_int         = print_for_int         (visitor ());
    down            = print_down            (visitor ());
    step            = print_step            (visitor ());
    for_collect     = print_for_collect     (visitor ());
    bind_to         = print_bind_to         (visitor ());
    expr            = print_expr            (visitor ());
    tuple           = print_tuple           (visitor ());
    list            = print_list            (visitor ());
    empty_list      = print_empty_list      (visitor ());
    set             = print_set             (visitor ());
    empty_set       = print_empty_set       (visitor ());
    none_expr       = print_none_expr       (visitor ());
    fun_call        = print_fun_call        (visitor ());
    constr_app      = print_constr_app      (visitor ());
    some_app        = print_some_app        (visitor ());
    map_lookup      = print_map_lookup      (visitor ());
    par_expr        = print_par_expr        (visitor ());
    pattern         = print_pattern         (visitor ());
    core_pattern    = print_core_pattern    (visitor ());
    psome           = print_psome           (visitor ());
    patterns        = print_patterns        (visitor ());
    list_pattern    = print_list_pattern    (visitor ());
    sugar           = print_sugar           (visitor ());
    raw             = print_raw             (visitor ());
    ptuple          = print_ptuple          (visitor ())
  }

let print_tokens = print_tokens (visitor ())
