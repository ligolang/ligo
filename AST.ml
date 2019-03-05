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
  types      : type_decl reg list;
  constants  : const_decl reg list;
  parameter  : parameter_decl reg;
  storage    : storage_decl reg;
  operations : operations_decl reg;
  lambdas    : lambda_decl list;
  block      : block reg;
  eof        : eof
}

and ast = t

and parameter_decl = {
  kwd_parameter : kwd_parameter;
  name          : variable;
  colon         : colon;
  param_type    : type_expr;
  terminator    : semi option
}

and storage_decl = {
  kwd_storage : kwd_storage;
  store_type  : type_expr;
  terminator  : semi option
}

and operations_decl = {
  kwd_operations : kwd_operations;
  op_type        : type_expr;
  terminator     : semi option
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

and parameters = (param_decl, semi) nsepseq par

and param_decl =
  ParamConst of param_const
| ParamVar   of param_var

and param_const = (kwd_const * variable * colon * type_expr) reg

and param_var = (kwd_var * variable * colon * type_expr) reg

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

and const_decl = {
  kwd_const  : kwd_const;
  name       : variable;
  colon      : colon;
  vtype      : type_expr;
  equal      : equal;
  init       : expr;
  terminator : semi option
}

and var_decl = {
  kwd_var    : kwd_var;
  name       : variable;
  colon      : colon;
  vtype      : type_expr;
  ass        : ass;
  init       : expr;
  terminator : semi option
}

and instructions = (instruction, semi) nsepseq reg

and instruction =
  Single of single_instr
| Block  of block reg

and single_instr =
  Cond     of conditional reg
| Match    of match_instr reg
| Ass      of ass_instr
| Loop     of loop
| ProcCall of fun_call
| Null     of kwd_null
| Fail     of (kwd_fail * expr) reg

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
  lead_vbar : vbar option;
  cases     : cases;
  kwd_end   : kwd_end
}

and cases = (case, vbar) nsepseq reg

and case = (pattern * arrow * instruction) reg

and ass_instr = (variable * ass * expr) reg

and loop =
  While of while_loop
| For   of for_loop

and while_loop = (kwd_while * expr * block reg) reg

and for_loop =
  ForInt     of for_int reg
| ForCollect of for_collect reg

and for_int = {
  kwd_for : kwd_for;
  ass     : ass_instr;
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

type visitor = {
  ass_instr       : ass_instr -> unit;
  bind_to         : (region * variable) option -> unit;
  block           : block reg -> unit;
  bytes           : (string * MBytes.t) reg -> unit;
  cartesian       : cartesian -> unit;
  case            : case -> unit;
  cases           : cases -> unit;
  conditional     : conditional -> unit;
  const_decl      : const_decl reg -> unit;
  constr          : constr -> unit;
  constr_app      : constr_app -> unit;
  core_pattern    : core_pattern -> unit;
  down            : region option -> unit;
  empty_list      : empty_list -> unit;
  empty_set       : empty_set -> unit;
  expr            : expr -> unit;
  fail            : (kwd_fail * expr) -> unit;
  field_decl      : field_decl -> unit;
  field_decls     : field_decls -> unit;
  for_collect     : for_collect reg -> unit;
  for_int         : for_int reg -> unit;
  for_loop        : for_loop -> unit;
  fun_call        : fun_call -> unit;
  fun_decl        : fun_decl reg -> unit;
  instruction     : instruction -> unit;
  instructions    : instructions -> unit;
  int             : (string * Z.t) reg -> unit;
  lambda_decl     : lambda_decl -> unit;
  list            : (expr, region) nsepseq brackets -> unit;
  list_pattern    : list_pattern -> unit;
  loop            : loop -> unit;
  map_lookup      : map_lookup reg -> unit;
  match_instr     : match_instr -> unit;
  none_expr       : none_expr -> unit;
  nsepseq         : 'a.string -> ('a -> unit) -> ('a, region) nsepseq -> unit;
  operations_decl : operations_decl reg -> unit;
  par_expr        : expr par -> unit;
  par_type        : type_expr par -> unit;
  param_decl      : param_decl -> unit;
  parameter_decl  : parameter_decl reg -> unit;
  parameters      : parameters -> unit;
  param_const     : param_const -> unit;
  param_var       : param_var -> unit;
  pattern         : pattern -> unit;
  patterns        : core_pattern par -> unit;
  proc_decl       : proc_decl reg -> unit;
  psome           : (region * core_pattern par) reg -> unit;
  ptuple          : (core_pattern, region) nsepseq par -> unit;
  raw             : (core_pattern * region * pattern) par -> unit;
  record_type     : record_type -> unit;
  sepseq          : 'a.string -> ('a -> unit) -> ('a, region) sepseq -> unit;
  set             : (expr, region) nsepseq braces -> unit;
  single_instr    : single_instr -> unit;
  some_app        : (region * arguments) reg -> unit;
  step            : (region * expr) option -> unit;
  storage_decl    : storage_decl reg -> unit;
  string          : string reg -> unit;
  sugar           : (core_pattern, region) sepseq brackets -> unit;
  sum_type        : (variant, region) nsepseq reg -> unit;
  terminator      : semi option -> unit;
  token           : region -> string -> unit;
  tuple           : arguments -> unit;
  type_app        : (type_name * type_tuple) reg -> unit;
  type_decl       : type_decl reg -> unit;
  type_expr       : type_expr -> unit;
  type_tuple      : type_tuple -> unit;
  local_decl      : local_decl -> unit;
  local_decls     : local_decl list -> unit;
  var             : variable -> unit;
  var_decl        : var_decl reg -> unit;
  variant         : variant -> unit;
  while_loop      : while_loop -> unit
}

let printf = Printf.printf

let compact (region: Region.t) =
  region#compact ~offsets:EvalOpt.offsets EvalOpt.mode

let print_nsepseq :
  string -> ('a -> unit) -> ('a, Region.t) nsepseq -> unit =
  fun sep visit (head, tail) ->
    let print_aux (sep_reg, item) =
      printf "%s: %s\n" (compact sep_reg) sep;
      visit item
    in visit head; List.iter print_aux tail

let print_sepseq :
  string -> ('a -> unit) -> ('a, Region.t) sepseq -> unit =
  fun sep visit -> function
        None -> ()
  | Some seq -> print_nsepseq sep visit seq

and print_token _visitor region lexeme =
  printf "%s: %s\n"(compact region) lexeme

and print_var _visitor {region; value=lexeme} =
  printf "%s: Ident \"%s\"\n" (compact region) lexeme

and print_constr _visitor {region; value=lexeme} =
  printf "%s: Constr \"%s\"\n"
         (compact region) lexeme

and print_string _visitor {region; value=lexeme} =
  printf "%s: String \"%s\"\n"
         (compact region) lexeme

and print_bytes _visitor {region; value = lexeme, abstract} =
  printf "%s: Bytes (\"%s\", \"0x%s\")\n"
         (compact region) lexeme
         (MBytes.to_hex abstract |> Hex.to_string)

and print_int _visitor {region; value = lexeme, abstract} =
  printf "%s: Int (\"%s\", %s)\n"
         (compact region) lexeme
         (Z.to_string abstract)

(* Main printing function *)

and print_tokens (v: visitor) ast =
  List.iter v.type_decl   ast.types;
  v.parameter_decl        ast.parameter;
  v.storage_decl          ast.storage;
  v.operations_decl       ast.operations;
  List.iter v.lambda_decl ast.lambdas;
  v.block                 ast.block;
  v.token                 ast.eof "EOF"

and print_parameter_decl (v: visitor) {value=node; _} =
  v.token      node.kwd_parameter "parameter";
  v.var        node.name;
  v.token      node.colon ":";
  v.type_expr  node.param_type;
  v.terminator node.terminator

and print_storage_decl (v: visitor) {value=node; _} =
  v.token      node.kwd_storage "storage";
  v.type_expr  node.store_type;
  v.terminator node.terminator

and print_operations_decl (v: visitor) {value=node; _} =
  v.token      node.kwd_operations "operations";
  v.type_expr  node.op_type;
  v.terminator node.terminator

and print_type_decl (v: visitor) {value=node; _} =
  v.token      node.kwd_type "type";
  v.var        node.name;
  v.token      node.kwd_is "is";
  v.type_expr  node.type_expr;
  v.terminator node.terminator

and print_type_expr (v: visitor) = function
  Prod    cartesian   -> v.cartesian   cartesian
| Sum     sum_type    -> v.sum_type    sum_type
| Record  record_type -> v.record_type record_type
| TypeApp type_app    -> v.type_app    type_app
| ParType par_type    -> v.par_type    par_type
| TAlias  type_alias  -> v.var         type_alias

and print_cartesian (v: visitor) {value=sequence; _} =
  v.nsepseq "*" v.type_expr sequence

and print_variant (v: visitor) {value=node; _} =
  let constr, kwd_of, cartesian = node in
  v.constr    constr;
  v.token     kwd_of "of";
  v.cartesian cartesian

and print_sum_type (v: visitor) {value=sequence; _} =
  v.nsepseq "|" v.variant sequence

and print_record_type (v: visitor) {value=node; _} =
  let kwd_record, field_decls, kwd_end = node in
  v.token       kwd_record "record";
  v.field_decls field_decls;
  v.token       kwd_end "end"

and print_type_app (v: visitor) {value=node; _} =
  let type_name, type_tuple = node in
  v.var        type_name;
  v.type_tuple type_tuple

and print_par_type (v: visitor) {value=node; _} =
  let lpar, type_expr, rpar = node in
  v.token     lpar "(";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_field_decls (v: visitor) sequence =
  v.nsepseq ";" v.field_decl sequence

and print_field_decl (v: visitor) {value=node; _} =
  let var, colon, type_expr = node in
  v.var       var;
  v.token     colon ":";
  v.type_expr type_expr

and print_type_tuple (v: visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq "," v.var sequence;
  v.token rpar ")"

and print_lambda_decl (v: visitor) = function
  FunDecl   fun_decl -> v.fun_decl fun_decl
| ProcDecl proc_decl -> v.proc_decl proc_decl

and print_fun_decl (v: visitor) {value=node; _} =
  v.token       node.kwd_function "function";
  v.var         node.name;
  v.parameters  node.param;
  v.token       node.colon ":";
  v.type_expr   node.ret_type;
  v.token       node.kwd_is "is";
  v.local_decls node.local_decls;
  v.block       node.block;
  v.token       node.kwd_with "with";
  v.expr        node.return;
  v.terminator  node.terminator

and print_proc_decl (v: visitor) {value=node; _} =
  v.token       node.kwd_procedure "procedure";
  v.var         node.name;
  v.parameters  node.param;
  v.token       node.kwd_is "is";
  v.local_decls node.local_decls;
  v.block       node.block;
  v.terminator  node.terminator

and print_parameters (v: visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq ";" v.param_decl sequence;
  v.token rpar ")"

and print_param_decl (v: visitor) = function
  ParamConst param_const -> v.param_const param_const
| ParamVar   param_var   -> v.param_var   param_var

and print_param_const (v: visitor) {value=node; _} =
  let kwd_const, variable, colon, type_expr = node in
  v.token     kwd_const "const";
  v.var       variable;
  v.token     colon ":";
  v.type_expr type_expr

and print_param_var (v: visitor) {value=node; _} =
  let kwd_var, variable, colon, type_expr = node in
  v.token     kwd_var "var";
  v.var       variable;
  v.token     colon ":";
  v.type_expr type_expr

and print_block (v: visitor) {value=node; _} =
  v.token        node.opening "begin";
  v.instructions node.instr;
  v.terminator   node.terminator;
  v.token        node.close "end"

and print_local_decls (v: visitor) sequence =
  List.iter v.local_decl sequence

and print_local_decl (v: visitor) = function
  LocalLam   decl -> v.lambda_decl decl
| LocalConst decl -> v.const_decl  decl
| LocalVar   decl -> v.var_decl    decl

and print_const_decl (v: visitor) {value=node; _} =
  v.token      node.kwd_const "const";
  v.var        node.name;
  v.token      node.colon ":";
  v.type_expr  node.vtype;
  v.token      node.equal "=";
  v.expr       node.init;
  v.terminator node.terminator

and print_var_decl (v: visitor) {value=node; _} =
  v.token      node.kwd_var "var";
  v.var        node.name;
  v.token      node.colon ":";
  v.type_expr  node.vtype;
  v.token      node.ass ":=";
  v.expr       node.init;
  v.terminator node.terminator

and print_instructions (v: visitor) {value=sequence; _} =
  v.nsepseq ";" v.instruction sequence

and print_instruction (v: visitor) = function
  Single instr -> v.single_instr instr
|  Block block -> v.block block

and print_single_instr (v: visitor) = function
  Cond     {value; _} -> v.conditional value
| Match    {value; _} -> v.match_instr value
| Ass      instr      -> v.ass_instr instr
| Loop     loop       -> v.loop loop
| ProcCall fun_call   -> v.fun_call fun_call
| Null     kwd_null   -> v.token kwd_null "null"
| Fail     {value; _} -> v.fail value

and print_fail (v: visitor) (kwd_fail, expr) =
  v.token kwd_fail "fail";
  v.expr expr

and print_conditional (v: visitor) node =
  v.token       node.kwd_if "if";
  v.expr        node.test;
  v.token       node.kwd_then "then";
  v.instruction node.ifso;
  v.token       node.kwd_else "else";
  v.instruction node.ifnot

and print_match_instr (v: visitor) node =
  v.token node.kwd_match "match";
  v.expr  node.expr;
  v.token node.kwd_with "with";
  v.cases node.cases;
  v.token node.kwd_end "end"

and print_cases (v: visitor) {value=sequence; _} =
  v.nsepseq "|" v.case sequence

and print_case (v: visitor) {value=node; _} =
  let pattern, arrow, instruction = node in
  v.pattern pattern;
  v.token arrow "->";
  v.instruction instruction

and print_ass_instr (v: visitor) {value=node; _} =
  let variable, ass, expr = node in
  v.var variable;
  v.token ass ":=";
  v.expr expr

and print_loop (v: visitor) = function
  While while_loop -> v.while_loop while_loop
| For     for_loop -> v.for_loop for_loop

and print_while_loop (v: visitor) {value=node; _} =
  let kwd_while, expr, block = node in
  v.token kwd_while "while";
  v.expr expr;
  v.block block

and print_for_loop (v: visitor) = function
  ForInt     for_int     -> v.for_int for_int
| ForCollect for_collect -> v.for_collect for_collect

and print_for_int (v: visitor) ({value=node; _} : for_int reg) =
  v.token     node.kwd_for "for";
  v.ass_instr node.ass;
  v.down      node.down;
  v.token     node.kwd_to "to";
  v.expr      node.bound;
  v.step      node.step;
  v.block     node.block

and print_down (v: visitor) = function
  Some kwd_down -> v.token kwd_down "down"
| None -> ()

and print_step (v: visitor) = function
  Some (kwd_step, expr) ->
    v.token kwd_step "step";
    v.expr expr
| None -> ()

and print_for_collect (v: visitor) ({value=node; _} : for_collect reg) =
  v.token   node.kwd_for "for";
  v.var     node.var;
  v.bind_to node.bind_to;
  v.token   node.kwd_in "in";
  v.expr    node.expr;
  v.block   node.block

and print_bind_to (v: visitor) = function
  Some (arrow, variable) ->
    v.token arrow "->";
    v.var   variable
| None -> ()

and print_expr (v: visitor) = function
  Or {value = expr1, bool_or, expr2; _} ->
    v.expr expr1; v.token bool_or "||"; v.expr expr2
| And {value = expr1, bool_and, expr2; _} ->
    v.expr expr1; v.token bool_and "&&"; v.expr expr2
| Lt {value = expr1, lt, expr2; _} ->
    v.expr expr1; v.token lt "<"; v.expr expr2
| Leq {value = expr1, leq, expr2; _} ->
    v.expr expr1; v.token leq "<="; v.expr expr2
| Gt {value = expr1, gt, expr2; _} ->
    v.expr expr1; v.token gt ">"; v.expr expr2
| Geq {value = expr1, geq, expr2; _} ->
    v.expr expr1; v.token geq ">="; v.expr expr2
| Equal {value = expr1, equal, expr2; _} ->
    v.expr expr1; v.token equal "="; v.expr expr2
| Neq {value = expr1, neq, expr2; _} ->
    v.expr expr1; v.token neq "=/="; v.expr expr2
| Cat {value = expr1, cat, expr2; _} ->
    v.expr expr1; v.token cat "^"; v.expr expr2
| Cons {value = expr1, cons, expr2; _} ->
    v.expr expr1; v.token cons "<:"; v.expr expr2
| Add {value = expr1, add, expr2; _} ->
    v.expr expr1; v.token add "+"; v.expr expr2
| Sub {value = expr1, sub, expr2; _} ->
    v.expr expr1; v.token sub "-"; v.expr expr2
| Mult {value = expr1, mult, expr2; _} ->
    v.expr expr1; v.token mult "*"; v.expr expr2
| Div {value = expr1, div, expr2; _} ->
    v.expr expr1; v.token div "/"; v.expr expr2
| Mod {value = expr1, kwd_mod, expr2; _} ->
    v.expr expr1; v.token kwd_mod "mod"; v.expr expr2
| Neg {value = minus, expr; _} ->
    v.token minus "-"; v.expr expr
| Not {value = kwd_not, expr; _} ->
    v.token kwd_not "not"; v.expr expr
| Int i            -> v.int i
| Var var          -> v.var var
| String s         -> v.string s
| Bytes b          -> v.bytes b
| False region     -> v.token region "False"
| True region      -> v.token region "True"
| Unit region      -> v.token region "Unit"
| Tuple tuple      -> v.tuple tuple
| List list        -> v.list list
| EmptyList elist  -> v.empty_list elist
| Set set          -> v.set set
| EmptySet eset    -> v.empty_set eset
| NoneExpr nexpr   -> v.none_expr nexpr
| FunCall fun_call -> v.fun_call fun_call
| ConstrApp capp   -> v.constr_app capp
| SomeApp sapp     -> v.some_app sapp
| MapLookUp lookup -> v.map_lookup lookup
| ParExpr pexpr    -> v.par_expr pexpr

and print_tuple (v: visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq "," v.expr sequence;
  v.token rpar ")"

and print_list (v: visitor) {value=node; _} =
  let lbra, sequence, rbra = node in
  v.token lbra "[";
  v.nsepseq "," v.expr sequence;
  v.token rbra "]"

and print_empty_list (v: visitor) {value=node; _} =
  let lpar, (lbracket, rbracket, colon, type_expr), rpar = node in
  v.token     lpar "(";
  v.token     lbracket "[";
  v.token     rbracket "]";
  v.token     colon ":";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_set (v: visitor) {value=node; _} =
  let lbrace, sequence, rbrace = node in
  v.token lbrace "{";
  v.nsepseq "," v.expr sequence;
  v.token rbrace "}"

and print_empty_set (v: visitor) {value=node; _} =
  let lpar, (lbrace, rbrace, colon, type_expr), rpar = node in
  v.token     lpar "(";
  v.token     lbrace "{";
  v.token     rbrace "}";
  v.token     colon ":";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_none_expr (v: visitor) {value=node; _} =
  let lpar, (c_None, colon, type_expr), rpar = node in
  v.token     lpar "(";
  v.token     c_None "None";
  v.token     colon ":";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_fun_call (v: visitor) {value=node; _} =
  let fun_name, arguments = node in
  v.var   fun_name;
  v.tuple arguments

and print_constr_app (v: visitor) {value=node; _} =
  let constr, arguments = node in
  v.constr constr;
  v.tuple  arguments

and print_some_app (v: visitor) {value=node; _} =
  let c_Some, arguments = node in
  v.token c_Some "Some";
  v.tuple arguments

and print_map_lookup (v: visitor) {value=node; _} =
  let {value = lbracket, expr, rbracket; _} = node.index in
  v.var   node.map_name;
  v.token node.selector ".";
  v.token lbracket "[";
  v.expr  expr;
  v.token rbracket "]"

and print_par_expr (v: visitor) {value=node; _} =
  let lpar, expr, rpar = node in
  v.token lpar "(";
  v.expr  expr;
  v.token rpar ")"

and print_pattern (v: visitor) {value=sequence; _} =
  v.nsepseq "<:" v.core_pattern sequence

and print_core_pattern (v: visitor) = function
  PVar var      -> v.var var
| PWild wild    -> v.token wild "_"
| PInt i        -> v.int i
| PBytes b      -> v.bytes b
| PString s     -> v.string s
| PUnit region  -> v.token region "Unit"
| PFalse region -> v.token region "False"
| PTrue region  -> v.token region "True"
| PNone region  -> v.token region "None"
| PSome psome   -> v.psome psome
| PList pattern -> v.list_pattern pattern
| PTuple ptuple -> v.ptuple ptuple

and print_psome (v: visitor) {value=node; _} =
  let c_Some, patterns = node in
  v.token    c_Some "Some";
  v.patterns patterns

and print_patterns (v: visitor) {value=node; _} =
  let lpar, core_pattern, rpar = node in
  v.token lpar "(";
  v.core_pattern core_pattern;
  v.token rpar ")"

and print_list_pattern (v: visitor) = function
  Sugar sugar -> v.sugar sugar
| Raw     raw -> v.raw raw

and print_sugar (v: visitor) {value=node; _} =
  let lbracket, sequence, rbracket = node in
  v.token lbracket "[";
  v.sepseq "," v.core_pattern sequence;
  v.token rbracket "]"

and print_raw (v: visitor) {value=node; _} =
  let lpar, (core_pattern, cons, pattern), rpar = node in
  v.token        lpar "(";
  v.core_pattern core_pattern;
  v.token        cons "<:";
  v.pattern      pattern;
  v.token        rpar ")"

and print_ptuple (v: visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq "," v.core_pattern sequence;
  v.token rpar ")"

and print_terminator (v: visitor) = function
  Some semi -> v.token semi ";"
| None -> ()

let rec visitor () : visitor = {
  nsepseq         = print_nsepseq;
  sepseq          = print_sepseq;
  token           = print_token           (visitor ());
  var             = print_var             (visitor ());
  constr          = print_constr          (visitor ());
  string          = print_string          (visitor ());
  bytes           = print_bytes           (visitor ());
  int             = print_int             (visitor ());

  local_decl      = print_local_decl      (visitor ());
  fail            = print_fail            (visitor ());
  param_var       = print_param_var       (visitor ());
  param_const     = print_param_const     (visitor ());
  const_decl      = print_const_decl      (visitor ());
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
  block           = print_block           (visitor ());
  local_decls     = print_local_decls     (visitor ());
  var_decl        = print_var_decl        (visitor ());
  instructions    = print_instructions    (visitor ());
  instruction     = print_instruction     (visitor ());
  single_instr    = print_single_instr    (visitor ());
  conditional     = print_conditional     (visitor ());
  match_instr     = print_match_instr     (visitor ());
  cases           = print_cases           (visitor ());
  case            = print_case            (visitor ());
  ass_instr       = print_ass_instr       (visitor ());
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
  ptuple          = print_ptuple          (visitor ());
  terminator      = print_terminator      (visitor ())
}

let print_tokens = print_tokens (visitor ())
