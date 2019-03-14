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
type kwd_copy       = Region.t
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
type kwd_match      = Region.t
type kwd_mod        = Region.t
type kwd_not        = Region.t
type kwd_null       = Region.t
type kwd_of         = Region.t
type kwd_procedure  = Region.t
type kwd_record     = Region.t
type kwd_step       = Region.t
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

and type_tuple = (type_name, comma) nsepseq par reg

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
  param          : parameters;
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
  ass        : ass;
  init       : expr;
  terminator : semi option
}

and instructions = (instruction, semi) nsepseq

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
| Fail     of fail_instr reg

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

and match_instr = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
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

and ass_instr =
  VarAss of var_ass reg
| MapAss of map_ass reg

and var_ass = {
  var  : variable;
  ass  : ass;
  expr : expr
}

and map_ass = {
  lookup : map_lookup reg;
  ass    : ass;
  expr   : expr
}

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
  var_ass : var_ass reg;
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
  LogicExpr  of logic_expr
| ArithExpr  of arith_expr
| StringExpr of string_expr
| ListExpr   of list_expr
| SetExpr    of set_expr
| ConstrExpr of constr_expr
| RecordExpr of record_expr
| Var        of Lexer.lexeme reg
| FunCall    of fun_call
| Bytes      of (Lexer.lexeme * MBytes.t) reg
| Unit       of c_Unit
| Tuple      of tuple
| MapLookUp  of map_lookup reg
| ParExpr    of expr par reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of bool_or bin_op reg
| And   of bool_and bin_op reg
| Not   of kwd_not un_op reg
| False of c_False
| True  of c_True

and 'a bin_op = {
  op1 : expr;
  op  : 'a;
  op2 : expr
}

and 'a un_op = {
  op  : 'a;
  op1 : expr
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
| Neg  of minus   un_op reg
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
| RecordCopy of record_copy reg

and record_injection = {
  opening    : kwd_record;
  fields     : (field_ass reg, semi) nsepseq;
  terminator : semi option;
  close      : kwd_end
}

and field_ass = {
  field_name : field_name;
  equal      : equal;
  field_expr : expr
}

and record_projection = {
  record_name : variable;
  selector    : dot;
  field_name  : field_name
}

and record_copy = {
  kwd_copy    : kwd_copy;
  record_name : variable;
  kwd_with    : kwd_with;
  delta       : record_injection reg
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

and map_lookup = {
  map_name : variable;
  selector : dot;
  index    : expr brackets reg
}

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

open! Region

let type_expr_to_region = function
  Prod    {region; _}
| Sum     {region; _}
| Record  {region; _}
| TypeApp {region; _}
| ParType {region; _}
| TAlias  {region; _} -> region

let rec expr_to_region = function
  LogicExpr  e -> logic_expr_to_region e
| ArithExpr  e -> arith_expr_to_region e
| StringExpr e -> string_expr_to_region e
| ListExpr   e -> list_expr_to_region e
| SetExpr    e -> set_expr_to_region e
| ConstrExpr e -> constr_expr_to_region e
| RecordExpr e -> record_expr_to_region e
| Var       {region; _}
| FunCall   {region; _}
| Bytes     {region; _}
| Unit      region
| Tuple     {region; _}
| MapLookUp {region; _}
| ParExpr   {region; _} -> region

and logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

and bool_expr_to_region = function
  Or        {region; _}
| And       {region; _}
| Not       {region; _}
| False     region
| True      region -> region

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
| Int  {region; _} -> region

and string_expr_to_region = function
  Cat    {region; _}
| String {region; _} -> region

and list_expr_to_region = function
  Cons      {region; _}
| List      {region; _}
| EmptyList {region; _} -> region

and set_expr_to_region = function
  Set      {region; _}
| EmptySet {region; _} -> region

and constr_expr_to_region = function
  NoneExpr  {region; _}
| ConstrApp {region; _}
| SomeApp   {region; _} -> region

and record_expr_to_region = function
  RecordInj  {region; _}
| RecordProj {region; _}
| RecordCopy {region; _} -> region

let instr_to_region = function
  Single Cond                {region; _}
| Single Match               {region; _}
| Single Ass VarAss          {region; _}
| Single Ass MapAss          {region; _}
| Single Loop While          {region; _}
| Single Loop For ForInt     {region; _}
| Single Loop For ForCollect {region; _}
| Single ProcCall            {region; _}
| Single Null                 region
| Single Fail                {region; _}
| Block                      {region; _} -> region

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
| PList Raw   {region; _}
| PTuple      {region; _} -> region

let local_decl_to_region = function
  LocalLam FunDecl   {region; _}
| LocalLam ProcDecl  {region; _}
| LocalLam EntryDecl {region; _}
| LocalConst         {region; _}
| LocalVar           {region; _} -> region

(* Printing the tokens with their source regions *)

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

let print_token region lexeme =
  printf "%s: %s\n"(compact region) lexeme

let print_var {region; value=lexeme} =
  printf "%s: Ident \"%s\"\n" (compact region) lexeme

let print_constr {region; value=lexeme} =
  printf "%s: Constr \"%s\"\n"
         (compact region) lexeme

let print_string {region; value=lexeme} =
  printf "%s: String %s\n"
         (compact region) lexeme

let print_bytes {region; value = lexeme, abstract} =
  printf "%s: Bytes (\"%s\", \"0x%s\")\n"
         (compact region) lexeme
         (MBytes.to_hex abstract |> Hex.to_string)

let print_int {region; value = lexeme, abstract} =
  printf "%s: Int (\"%s\", %s)\n"
         (compact region) lexeme
         (Z.to_string abstract)

(* Main printing function *)

let rec print_tokens ast =
  let {decl; eof} = ast in
  Utils.nseq_iter print_decl decl;
  print_token eof "EOF"

and print_decl = function
  TypeDecl    decl -> print_type_decl       decl
| ConstDecl   decl -> print_const_decl      decl
| LambdaDecl  decl -> print_lambda_decl     decl

and print_const_decl {value; _} =
  let {kwd_const; name; colon; const_type;
       equal; init; terminator} = value in
  print_token      kwd_const "const";
  print_var        name;
  print_token      colon ":";
  print_type_expr  const_type;
  print_token      equal "=";
  print_expr       init;
  print_terminator terminator

and print_type_decl {value; _} =
  let {kwd_type; name; kwd_is;
       type_expr; terminator} = value in
  print_token      kwd_type "type";
  print_var        name;
  print_token      kwd_is "is";
  print_type_expr  type_expr;
  print_terminator terminator

and print_type_expr = function
  Prod    cartesian   -> print_cartesian   cartesian
| Sum     sum_type    -> print_sum_type    sum_type
| Record  record_type -> print_record_type record_type
| TypeApp type_app    -> print_type_app    type_app
| ParType par_type    -> print_par_type    par_type
| TAlias  type_alias  -> print_var         type_alias

and print_cartesian {value; _} =
  print_nsepseq "*" print_type_expr value

and print_variant {value; _} =
  let {constr; kwd_of; product} = value in
  print_constr    constr;
  print_token     kwd_of "of";
  print_cartesian product

and print_sum_type {value; _} =
  print_nsepseq "|" print_variant value

and print_record_type {value; _} =
  let {kwd_record; fields; kwd_end} = value in
  print_token       kwd_record "record";
  print_field_decls fields;
  print_token       kwd_end "end"

and print_type_app {value; _} =
  let type_name, type_tuple = value in
  print_var        type_name;
  print_type_tuple type_tuple

and print_par_type {value; _} =
  let {lpar; inside; rpar} = value in
  print_token     lpar "(";
  print_type_expr inside;
  print_token     rpar ")"

and print_field_decls sequence =
  print_nsepseq ";" print_field_decl sequence

and print_field_decl {value; _} =
  let {field_name; colon; field_type} = value in
  print_var       field_name;
  print_token     colon ":";
  print_type_expr field_type

and print_type_tuple {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq "," print_var inside;
  print_token rpar ")"

and print_lambda_decl = function
  FunDecl     fun_decl -> print_fun_decl   fun_decl
| ProcDecl   proc_decl -> print_proc_decl  proc_decl
| EntryDecl entry_decl -> print_entry_decl entry_decl

and print_fun_decl {value; _} =
  let {kwd_function; name; param; colon;
       ret_type; kwd_is; local_decls;
       block; kwd_with; return; terminator} = value in
  print_token       kwd_function "function";
  print_var         name;
  print_parameters  param;
  print_token       colon ":";
  print_type_expr   ret_type;
  print_token       kwd_is "is";
  print_local_decls local_decls;
  print_block       block;
  print_token       kwd_with "with";
  print_expr        return;
  print_terminator  terminator

and print_proc_decl {value; _} =
  let {kwd_procedure; name; param; kwd_is;
       local_decls; block; terminator} = value in
  print_token       kwd_procedure "procedure";
  print_var         name;
  print_parameters  param;
  print_token       kwd_is "is";
  print_local_decls local_decls;
  print_block       block;
  print_terminator  terminator

and print_entry_decl {value; _} =
  let {kwd_entrypoint; name; param; colon;
       ret_type; kwd_is; local_decls;
       block; kwd_with; return; terminator} = value in
  print_token       kwd_entrypoint "entrypoint";
  print_var         name;
  print_parameters  param;
  print_token       colon ":";
  print_type_expr   ret_type;
  print_token       kwd_is "is";
  print_local_decls local_decls;
  print_block       block;
  print_token       kwd_with "with";
  print_expr        return;
  print_terminator  terminator

and print_parameters {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq ";" print_param_decl inside;
  print_token rpar ")"

and print_param_decl = function
  ParamConst param_const -> print_param_const param_const
| ParamVar   param_var   -> print_param_var   param_var

and print_param_const {value; _} =
  let {kwd_const; var; colon; param_type} = value in
  print_token     kwd_const "const";
  print_var       var;
  print_token     colon ":";
  print_type_expr param_type

and print_param_var {value; _} =
  let {kwd_var; var; colon; param_type} = value in
  print_token     kwd_var "var";
  print_var       var;
  print_token     colon ":";
  print_type_expr param_type

and print_block {value; _} =
  let {opening; instr; terminator; close} = value in
  print_token        opening "begin";
  print_instructions instr;
  print_terminator   terminator;
  print_token        close "end"

and print_local_decls sequence =
  List.iter print_local_decl sequence

and print_local_decl = function
  LocalLam   decl -> print_lambda_decl decl
| LocalConst decl -> print_const_decl  decl
| LocalVar   decl -> print_var_decl    decl

and print_var_decl {value; _} =
  let {kwd_var; name; colon; var_type;
       ass; init; terminator} = value in
  print_token      kwd_var "var";
  print_var        name;
  print_token      colon ":";
  print_type_expr  var_type;
  print_token      ass ":=";
  print_expr       init;
  print_terminator terminator

and print_instructions sequence =
  print_nsepseq ";" print_instruction sequence

and print_instruction = function
  Single instr -> print_single_instr instr
|  Block block -> print_block block

and print_single_instr = function
  Cond     {value; _} -> print_conditional value
| Match    {value; _} -> print_match_instr value
| Ass      instr      -> print_ass_instr instr
| Loop     loop       -> print_loop loop
| ProcCall fun_call   -> print_fun_call fun_call
| Null     kwd_null   -> print_token kwd_null "null"
| Fail     {value; _} -> print_fail value

and print_fail {kwd_fail; fail_expr} =
  print_token kwd_fail "fail";
  print_expr fail_expr

and print_conditional node =
  let {kwd_if; test; kwd_then; ifso;
       kwd_else; ifnot} = node in
  print_token       kwd_if "if";
  print_expr        test;
  print_token       kwd_then "then";
  print_instruction ifso;
  print_token       kwd_else "else";
  print_instruction ifnot

and print_match_instr node =
  let {kwd_match; expr; kwd_with;
       lead_vbar; cases; kwd_end} = node in
  print_token kwd_match "match";
  print_expr  expr;
  print_token kwd_with "with";
  print_token_opt lead_vbar "|";
  print_cases cases;
  print_token kwd_end "end"

and print_token_opt = function
         None -> fun _ -> ()
| Some region -> print_token region

and print_cases {value; _} =
  print_nsepseq "|" print_case value

and print_case {value; _} =
  let {pattern; arrow; instr} = value in
  print_pattern pattern;
  print_token arrow "->";
  print_instruction instr

and print_ass_instr = function
  VarAss a -> print_var_ass a
| MapAss a -> print_map_ass a

and print_var_ass {value; _} =
  let {var; ass; expr} = value in
  print_var var;
  print_token ass ":=";
  print_expr expr

and print_map_ass {value; _} =
  let {lookup; ass; expr} = value in
  print_map_lookup lookup;
  print_token ass ":=";
  print_expr expr

and print_loop = function
  While {value; _} -> print_while_loop value
| For     for_loop -> print_for_loop for_loop

and print_while_loop value =
  let {kwd_while; cond; block} = value in
  print_token kwd_while "while";
  print_expr cond;
  print_block block

and print_for_loop = function
  ForInt     for_int     -> print_for_int for_int
| ForCollect for_collect -> print_for_collect for_collect

and print_for_int ({value; _} : for_int reg) =
  let {kwd_for; var_ass; down; kwd_to;
       bound; step; block} = value in
  print_token   kwd_for "for";
  print_var_ass var_ass;
  print_down    down;
  print_token   kwd_to "to";
  print_expr    bound;
  print_step    step;
  print_block   block

and print_down = function
  Some kwd_down -> print_token kwd_down "down"
| None -> ()

and print_step = function
  Some (kwd_step, expr) ->
    print_token kwd_step "step";
    print_expr expr
| None -> ()

and print_for_collect ({value; _} : for_collect reg) =
  let {kwd_for; var; bind_to; kwd_in; expr; block} = value in
  print_token   kwd_for "for";
  print_var     var;
  print_bind_to bind_to;
  print_token   kwd_in "in";
  print_expr    expr;
  print_block   block

and print_bind_to = function
  Some (arrow, variable) ->
    print_token arrow "->";
    print_var   variable
| None -> ()

and print_expr = function
  LogicExpr e  -> print_logic_expr e
| ArithExpr e  -> print_arith_expr e
| StringExpr e -> print_string_expr e
| ListExpr e   -> print_list_expr e
| SetExpr e    -> print_set_expr e
| ConstrExpr e -> print_constr_expr e
| RecordExpr e -> print_record_expr e
| Var var      -> print_var var
| FunCall e    -> print_fun_call e
| Bytes b      -> print_bytes b
| Unit region  -> print_token region "Unit"
| Tuple e      -> print_tuple e
| MapLookUp e  -> print_map_lookup e
| ParExpr e    -> print_par_expr e

and print_logic_expr = function
  BoolExpr e -> print_bool_expr e
| CompExpr e -> print_comp_expr e

and print_bool_expr = function
  Or {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "||"; print_expr op2
| And {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "&&"; print_expr op2
| Not {value = {op; op1}; _} ->
    print_token op "not"; print_expr op1
| False region -> print_token region "False"
| True region  -> print_token region "True"

and print_comp_expr = function
  Lt {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "<"; print_expr op2
| Leq {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "<="; print_expr op2
| Gt {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op ">"; print_expr op2
| Geq {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op ">="; print_expr op2
| Equal {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "="; print_expr op2
| Neq {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "=/="; print_expr op2

and print_arith_expr = function
  Add {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "+"; print_expr op2
| Sub {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "-"; print_expr op2
| Mult {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "*"; print_expr op2
| Div {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "/"; print_expr op2
| Mod {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "mod"; print_expr op2
| Neg {value = {op; op1}; _} ->
    print_token op "-"; print_expr op1
| Int i -> print_int i

and print_string_expr = function
  Cat {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "^"; print_expr op2
| String s -> print_string s

and print_list_expr = function
  Cons {value = {op1; op; op2}; _} ->
    print_expr op1; print_token op "#"; print_expr op2
| List e       -> print_list e
| EmptyList e  -> print_empty_list e

and print_set_expr = function
  Set e      -> print_set e
| EmptySet e -> print_empty_set e

and print_constr_expr = function
  SomeApp e   -> print_some_app e
| NoneExpr e  -> print_none_expr e
| ConstrApp e -> print_constr_app e

and print_record_expr = function
  RecordInj  e -> print_record_injection e
| RecordProj e -> print_record_projection e
| RecordCopy e -> print_record_copy e

and print_record_injection {value; _} =
  let {opening; fields; terminator; close} = value in
  print_token opening "record";
  print_nsepseq ";" print_field_ass fields;
  print_terminator terminator;
  print_token close "end"

and print_field_ass {value; _} =
  let {field_name; equal; field_expr} = value in
  print_var field_name;
  print_token equal "=";
  print_expr field_expr

and print_record_projection {value; _} =
  let {record_name; selector; field_name} = value in
  print_var record_name;
  print_token selector ".";
  print_var field_name

and print_record_copy {value; _} =
  let {kwd_copy; record_name; kwd_with; delta} = value in
  print_token kwd_copy "copy";
  print_var record_name;
  print_token kwd_with "with";
  print_record_injection delta

and print_tuple {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq "," print_expr inside;
  print_token rpar ")"

and print_list {value; _} =
  let {lbracket; inside; rbracket} = value in
  print_token lbracket "[";
  print_nsepseq "," print_expr inside;
  print_token rbracket "]"

and print_empty_list {value; _} =
  let {lpar; inside; rpar} = value in
  let {lbracket; rbracket; colon; list_type} = inside in
  print_token     lpar "(";
  print_token     lbracket "[";
  print_token     rbracket "]";
  print_token     colon ":";
  print_type_expr list_type;
  print_token     rpar ")"

and print_set {value; _} =
  let {lbrace; inside; rbrace} = value in
  print_token lbrace "{";
  print_nsepseq "," print_expr inside;
  print_token rbrace "}"

and print_empty_set {value; _} =
  let {lpar; inside; rpar} = value in
  let {lbrace; rbrace; colon; set_type} = inside in
  print_token     lpar "(";
  print_token     lbrace "{";
  print_token     rbrace "}";
  print_token     colon ":";
  print_type_expr set_type;
  print_token     rpar ")"

and print_none_expr {value; _} =
  let {lpar; inside; rpar} = value in
  let {c_None; colon; opt_type} = inside in
  print_token     lpar "(";
  print_token     c_None "None";
  print_token     colon ":";
  print_type_expr opt_type;
  print_token     rpar ")"

and print_fun_call {value; _} =
  let fun_name, arguments = value in
  print_var   fun_name;
  print_tuple arguments

and print_constr_app {value; _} =
  let constr, arguments = value in
  print_constr constr;
  print_tuple  arguments

and print_some_app {value; _} =
  let c_Some, arguments = value in
  print_token c_Some "Some";
  print_tuple arguments

and print_map_lookup {value; _} =
  let {map_name; selector; index} = value in
  let {lbracket; inside; rbracket} = index.value in
  print_var   map_name;
  print_token selector ".";
  print_token lbracket "[";
  print_expr  inside;
  print_token rbracket "]"

and print_par_expr {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_expr  inside;
  print_token rpar ")"

and print_pattern = function
  PCons {value; _} -> print_nsepseq "#" print_pattern value
| PVar var         -> print_var var
| PWild wild       -> print_token wild "_"
| PInt i           -> print_int i
| PBytes b         -> print_bytes b
| PString s        -> print_string s
| PUnit region     -> print_token region "Unit"
| PFalse region    -> print_token region "False"
| PTrue region     -> print_token region "True"
| PNone region     -> print_token region "None"
| PSome psome      -> print_psome psome
| PList pattern    -> print_list_pattern pattern
| PTuple ptuple    -> print_ptuple ptuple

and print_psome {value; _} =
  let c_Some, patterns = value in
  print_token    c_Some "Some";
  print_patterns patterns

and print_patterns {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_pattern inside;
  print_token rpar ")"

and print_list_pattern = function
  Sugar sugar -> print_sugar sugar
| Raw     raw -> print_raw raw

and print_sugar {value; _} =
  let {lbracket; inside; rbracket} = value in
  print_token lbracket "[";
  print_sepseq "," print_pattern inside;
  print_token rbracket "]"

and print_raw {value; _} =
  let {lpar; inside; rpar} = value in
  let head, cons, tail = inside in
  print_token   lpar "(";
  print_pattern head;
  print_token   cons "#";
  print_pattern tail;
  print_token   rpar ")"

and print_ptuple {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq "," print_pattern inside;
  print_token rpar ")"

and print_terminator = function
  Some semi -> print_token semi ";"
| None -> ()
