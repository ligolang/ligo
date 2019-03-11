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
type kwd_operations = Region.t
type kwd_procedure  = Region.t
type kwd_record     = Region.t
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
  decl : declaration nseq;
  eof  : eof
}

and ast = t

and declaration =
  TypeDecl    of type_decl reg
| ConstDecl   of const_decl reg
| StorageDecl of storage_decl reg
| OpDecl      of operations_decl reg
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

and storage_decl = {
  kwd_storage : kwd_storage;
  name        : variable;
  colon       : colon;
  store_type  : type_expr;
  terminator  : semi option
}

and operations_decl = {
  kwd_operations : kwd_operations;
  name           : variable;
  colon          : colon;
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
  kwd_is         : kwd_is;
  local_decls    : local_decl list;
  block          : block reg;
  terminator     : semi option
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

and var_decl = {
  kwd_var    : kwd_var;
  name       : variable;
  colon      : colon;
  var_type   : type_expr;
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
  Prod    {region; _}
| Sum     {region; _}
| Record  {region; _}
| TypeApp {region; _}
| ParType {region; _}
| TAlias  {region; _} -> region

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
| StorageDecl decl -> print_storage_decl    decl
| OpDecl      decl -> print_operations_decl decl
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

and print_storage_decl {value; _} =
  let {kwd_storage; name; colon;
       store_type; terminator} = value in
  print_token      kwd_storage "storage";
  print_var        name;
  print_token      colon ":";
  print_type_expr  store_type;
  print_terminator terminator

and print_operations_decl {value; _} =
  let {kwd_operations; name; colon;
       op_type; terminator} = value in
  print_token      kwd_operations "operations";
  print_var        name;
  print_token      colon ":";
  print_type_expr  op_type;
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
  let constr, kwd_of, cartesian = value in
  print_constr    constr;
  print_token     kwd_of "of";
  print_cartesian cartesian

and print_sum_type {value; _} =
  print_nsepseq "|" print_variant value

and print_record_type {value; _} =
  let kwd_record, field_decls, kwd_end = value in
  print_token       kwd_record "record";
  print_field_decls field_decls;
  print_token       kwd_end "end"

and print_type_app {value; _} =
  let type_name, type_tuple = value in
  print_var        type_name;
  print_type_tuple type_tuple

and print_par_type {value; _} =
  let lpar, type_expr, rpar = value in
  print_token     lpar "(";
  print_type_expr type_expr;
  print_token     rpar ")"

and print_field_decls sequence =
  print_nsepseq ";" print_field_decl sequence

and print_field_decl {value; _} =
  let var, colon, type_expr = value in
  print_var       var;
  print_token     colon ":";
  print_type_expr type_expr

and print_type_tuple {value; _} =
  let lpar, sequence, rpar = value in
  print_token lpar "(";
  print_nsepseq "," print_var sequence;
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
  let {kwd_entrypoint; name; param; kwd_is;
       local_decls; block; terminator} = value in
  print_token       kwd_entrypoint "entrypoint";
  print_var         name;
  print_parameters  param;
  print_token       kwd_is "is";
  print_local_decls local_decls;
  print_block       block;
  print_terminator  terminator

and print_parameters {value; _} =
  let lpar, sequence, rpar = value in
  print_token lpar "(";
  print_nsepseq ";" print_param_decl sequence;
  print_token rpar ")"

and print_param_decl = function
  ParamConst param_const -> print_param_const param_const
| ParamVar   param_var   -> print_param_var   param_var

and print_param_const {value; _} =
  let kwd_const, variable, colon, type_expr = value in
  print_token     kwd_const "const";
  print_var       variable;
  print_token     colon ":";
  print_type_expr type_expr

and print_param_var {value; _} =
  let kwd_var, variable, colon, type_expr = value in
  print_token     kwd_var "var";
  print_var       variable;
  print_token     colon ":";
  print_type_expr type_expr

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

and print_instructions {value; _} =
  print_nsepseq ";" print_instruction value

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

and print_fail (kwd_fail, expr) =
  print_token kwd_fail "fail";
  print_expr expr

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
  let pattern, arrow, instruction = value in
  print_pattern pattern;
  print_token arrow "->";
  print_instruction instruction

and print_ass_instr {value; _} =
  let variable, ass, expr = value in
  print_var variable;
  print_token ass ":=";
  print_expr expr

and print_loop = function
  While while_loop -> print_while_loop while_loop
| For     for_loop -> print_for_loop for_loop

and print_while_loop {value; _} =
  let kwd_while, expr, block = value in
  print_token kwd_while "while";
  print_expr expr;
  print_block block

and print_for_loop = function
  ForInt     for_int     -> print_for_int for_int
| ForCollect for_collect -> print_for_collect for_collect

and print_for_int ({value; _} : for_int reg) =
  let {kwd_for; ass; down; kwd_to;
       bound; step; block} = value in
  print_token     kwd_for "for";
  print_ass_instr ass;
  print_down      down;
  print_token     kwd_to "to";
  print_expr      bound;
  print_step      step;
  print_block     block

and print_down = function
  Some kwd_down -> print_token kwd_down "down"
| None -> ()

and print_step = function
  Some (kwd_step, expr) ->
    print_token kwd_step "step";
    print_expr expr
| None -> ()

and print_for_collect ({value; _} : for_collect reg) =
  let {kwd_for; var; bind_to;
       kwd_in; expr; block} = value in
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
  Or {value = expr1, bool_or, expr2; _} ->
    print_expr expr1; print_token bool_or "||"; print_expr expr2
| And {value = expr1, bool_and, expr2; _} ->
    print_expr expr1; print_token bool_and "&&"; print_expr expr2
| Lt {value = expr1, lt, expr2; _} ->
    print_expr expr1; print_token lt "<"; print_expr expr2
| Leq {value = expr1, leq, expr2; _} ->
    print_expr expr1; print_token leq "<="; print_expr expr2
| Gt {value = expr1, gt, expr2; _} ->
    print_expr expr1; print_token gt ">"; print_expr expr2
| Geq {value = expr1, geq, expr2; _} ->
    print_expr expr1; print_token geq ">="; print_expr expr2
| Equal {value = expr1, equal, expr2; _} ->
    print_expr expr1; print_token equal "="; print_expr expr2
| Neq {value = expr1, neq, expr2; _} ->
    print_expr expr1; print_token neq "=/="; print_expr expr2
| Cat {value = expr1, cat, expr2; _} ->
    print_expr expr1; print_token cat "^"; print_expr expr2
| Cons {value = expr1, cons, expr2; _} ->
    print_expr expr1; print_token cons "#"; print_expr expr2
| Add {value = expr1, add, expr2; _} ->
    print_expr expr1; print_token add "+"; print_expr expr2
| Sub {value = expr1, sub, expr2; _} ->
    print_expr expr1; print_token sub "-"; print_expr expr2
| Mult {value = expr1, mult, expr2; _} ->
    print_expr expr1; print_token mult "*"; print_expr expr2
| Div {value = expr1, div, expr2; _} ->
    print_expr expr1; print_token div "/"; print_expr expr2
| Mod {value = expr1, kwd_mod, expr2; _} ->
    print_expr expr1; print_token kwd_mod "mod"; print_expr expr2
| Neg {value = minus, expr; _} ->
    print_token minus "-"; print_expr expr
| Not {value = kwd_not, expr; _} ->
    print_token kwd_not "not"; print_expr expr
| Int i            -> print_int i
| Var var          -> print_var var
| String s         -> print_string s
| Bytes b          -> print_bytes b
| False region     -> print_token region "False"
| True region      -> print_token region "True"
| Unit region      -> print_token region "Unit"
| Tuple tuple      -> print_tuple tuple
| List list        -> print_list list
| EmptyList elist  -> print_empty_list elist
| Set set          -> print_set set
| EmptySet eset    -> print_empty_set eset
| NoneExpr nexpr   -> print_none_expr nexpr
| FunCall fun_call -> print_fun_call fun_call
| ConstrApp capp   -> print_constr_app capp
| SomeApp sapp     -> print_some_app sapp
| MapLookUp lookup -> print_map_lookup lookup
| ParExpr pexpr    -> print_par_expr pexpr

and print_tuple {value; _} =
  let lpar, sequence, rpar = value in
  print_token lpar "(";
  print_nsepseq "," print_expr sequence;
  print_token rpar ")"

and print_list {value; _} =
  let lbra, sequence, rbra = value in
  print_token lbra "[";
  print_nsepseq "," print_expr sequence;
  print_token rbra "]"

and print_empty_list {value; _} =
  let lpar, (lbracket, rbracket, colon, type_expr),
      rpar = value in
  print_token     lpar "(";
  print_token     lbracket "[";
  print_token     rbracket "]";
  print_token     colon ":";
  print_type_expr type_expr;
  print_token     rpar ")"

and print_set {value; _} =
  let lbrace, sequence, rbrace = value in
  print_token lbrace "{";
  print_nsepseq "," print_expr sequence;
  print_token rbrace "}"

and print_empty_set {value; _} =
  let lpar, (lbrace, rbrace, colon, type_expr),
      rpar = value in
  print_token     lpar "(";
  print_token     lbrace "{";
  print_token     rbrace "}";
  print_token     colon ":";
  print_type_expr type_expr;
  print_token     rpar ")"

and print_none_expr {value; _} =
  let lpar, (c_None, colon, type_expr), rpar = value in
  print_token     lpar "(";
  print_token     c_None "None";
  print_token     colon ":";
  print_type_expr type_expr;
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
  let {value = lbracket, expr, rbracket; _} = index in
  print_var   map_name;
  print_token selector ".";
  print_token lbracket "[";
  print_expr  expr;
  print_token rbracket "]"

and print_par_expr {value; _} =
  let lpar, expr, rpar = value in
  print_token lpar "(";
  print_expr  expr;
  print_token rpar ")"

and print_pattern {value; _} =
  print_nsepseq "#" print_core_pattern value

and print_core_pattern = function
  PVar var      -> print_var var
| PWild wild    -> print_token wild "_"
| PInt i        -> print_int i
| PBytes b      -> print_bytes b
| PString s     -> print_string s
| PUnit region  -> print_token region "Unit"
| PFalse region -> print_token region "False"
| PTrue region  -> print_token region "True"
| PNone region  -> print_token region "None"
| PSome psome   -> print_psome psome
| PList pattern -> print_list_pattern pattern
| PTuple ptuple -> print_ptuple ptuple

and print_psome {value; _} =
  let c_Some, patterns = value in
  print_token    c_Some "Some";
  print_patterns patterns

and print_patterns {value; _} =
  let lpar, core_pattern, rpar = value in
  print_token lpar "(";
  print_core_pattern core_pattern;
  print_token rpar ")"

and print_list_pattern = function
  Sugar sugar -> print_sugar sugar
| Raw     raw -> print_raw raw

and print_sugar {value; _} =
  let lbracket, sequence, rbracket = value in
  print_token lbracket "[";
  print_sepseq "," print_core_pattern sequence;
  print_token rbracket "]"

and print_raw {value; _} =
  let lpar, (core_pattern, cons, pattern), rpar = value in
  print_token        lpar "(";
  print_core_pattern core_pattern;
  print_token        cons "#";
  print_pattern      pattern;
  print_token        rpar ")"

and print_ptuple {value; _} =
  let lpar, sequence, rpar = value in
  print_token lpar "(";
  print_nsepseq "," print_core_pattern sequence;
  print_token rpar ")"

and print_terminator = function
  Some semi -> print_token semi ";"
| None -> ()
