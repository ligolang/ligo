%{
(* START HEADER *)

[@@@warning "-42"]

open Region
open AST

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start program
%type <AST.t> program

%%

(* RULES *)

(* Compound constructs *)

par(X):
  LPAR X RPAR {
    let region = cover $1 $3
    in {region; value = $1,$2,$3}
  }

braces(X):
  LBRACE X RBRACE {
    let region = cover $1 $3
    in {region; value = $1,$2,$3}
  }

brackets(X):
  LBRACKET X RBRACKET {
    let region = cover $1 $3
    in {region; value = $1,$2,$3}
  }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those rules.
 *)

(* Possibly empty sequence of items *)

seq(X):
  (**)     {     [] }
| X seq(X) { $1::$2 }

(* Non-empty sequence of items *)

nseq(X):
  X seq(X) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empy separated sequence of items *)

sepseq(X,Sep):
  (**)           {    None }
| nsepseq(X,Sep) { Some $1 }

(* Inlines *)

%inline var        : Ident { $1 }
%inline type_name  : Ident { $1 }
%inline fun_name   : Ident { $1 }
%inline field_name : Ident { $1 }
%inline map_name   : Ident { $1 }

(* Main *)

program:
  seq(type_decl)
  seq(const_decl)
  parameter_decl
  storage_decl
  operations_decl
  seq(lambda_decl)
  block
  EOF {
    {
     types      = $1;
     constants  = $2;
     parameter  = $3;
     storage    = $4;
     operations = $5;
     lambdas    = $6;
     block      = $7;
     eof        = $8;
    }
  }

parameter_decl:
  Parameter var COLON type_expr option(SEMI) {
    let stop =
      match $5 with
               None -> type_expr_to_region $4
      | Some region -> region in
    let region = cover $1 stop in
    let value = {
      kwd_parameter = $1;
      name          = $2;
      colon         = $3;
      param_type    = $4;
      terminator    = $5}
    in {region; value}
  }

storage_decl:
  Storage type_expr option(SEMI) {
    let stop =
      match $3 with
               None -> type_expr_to_region $2
      | Some region -> region in
    let region = cover $1 stop in
    let value = {
      kwd_storage = $1;
      store_type  = $2;
      terminator  = $3}
    in {region; value}
  }

operations_decl:
  Operations type_expr option(SEMI) {
    let stop =
      match $3 with
              None -> type_expr_to_region $2
     | Some region -> region in
    let region = cover $1 stop in
    let value = {
      kwd_operations = $1;
      op_type        = $2;
      terminator     = $3}
    in {region; value}
  }

(* Type declarations *)

type_decl:
  Type type_name Is type_expr option(SEMI) {
    let stop =
      match $5 with
        None -> type_expr_to_region $4
      | Some region -> region in
    let region = cover $1 stop in
    let value = {
      kwd_type   = $1;
      name       = $2;
      kwd_is     = $3;
      type_expr  = $4;
      terminator = $5}
    in {region; value}}

type_expr:
  cartesian   { Prod   (Present $1) }
| sum_type    { Sum    (Present $1) }
| record_type { Record (Present $1) }

cartesian:
  nsepseq(core_type,TIMES) {
    let region = nsepseq_to_region type_expr_to_region $1
    in {region; value=$1}
  }

core_type:
  type_name {
    TAlias (Present $1)
  }
| type_name type_tuple {
    let region = cover $1.region $2.region
    in TypeApp (Present {region; value = $1,$2})
  }
| par(type_expr) {
   ParType (Present $1)
  }

type_tuple:
  par(nsepseq(type_name,COMMA)) { $1 }

sum_type:
  nsepseq(variant,VBAR) {
    let region = nsepseq_to_region (fun x -> x.region) $1
    in {region; value=$1}
  }

variant:
  Constr Of cartesian {
    let region = cover $1.region $3.region
    in {region; value = $1,$2,$3}
  }

record_type:
  Record
    nsepseq(field_decl,SEMI)
  End
  {
   let region = cover $1 $3
   in {region; value = $1,$2,$3}
  }

field_decl:
  field_name COLON type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    in {region; value = $1,$2,$3}
  }

(* Function and procedure declarations *)

lambda_decl:
  fun_decl  { FunDecl  $1 }
| proc_decl { ProcDecl $1 }

fun_decl:
  Function fun_name parameters COLON type_expr Is
    seq(local_decl)
    block
  With expr option(SEMI) {
    let stop =
      match $11 with
               None -> expr_to_region $10
      | Some region -> region in
    let region = cover $1 stop in
    let value = {
      kwd_function = $1;
      name         = $2;
      param        = $3;
      colon        = $4;
      ret_type     = $5;
      kwd_is       = $6;
      local_decls  = $7;
      block        = $8;
      kwd_with     = $9;
      return       = $10;
      terminator   = $11}
    in {region; value}
  }

proc_decl:
  Procedure fun_name parameters Is
    seq(local_decl)
    block option(SEMI)
    {
     let stop =
       match $7 with
         None -> $6.region
       | Some region -> region in
     let region = cover $1 stop in
     let value = {
       kwd_procedure = $1;
       name          = $2;
       param         = $3;
       kwd_is        = $4;
       local_decls   = $5;
       block         = $6;
       terminator    = $7}
     in {region; value}
  }

parameters:
  par(nsepseq(param_decl,SEMI)) { $1 }

param_decl:
  Var var COLON type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop
    in ParamVar {region; value = $1,$2,$3,$4}
  }
| Const var COLON type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop
    in ParamConst {region; value = $1,$2,$3,$4}
  }

block:
  Begin
    instruction after_instr
  {
   let instrs, terminator, close = $3 in
   let region = cover $1 close in
   let value = {
     opening     = $1;
     instr       = (let value = $2, instrs in
                    let region = nsepseq_to_region instr_to_region value
                    in {value; region});
     terminator;
     close}
   in {region; value}
  }

after_instr:
  SEMI instr_or_end {
    match $2 with
      `Some (instr, instrs, term, close) ->
        ($1, instr)::instrs, term, close
    | `End close ->
        [], Some $1, close
  }
| End {
   [], None, $1
  }

instr_or_end:
  End {
    `End $1 }
| instruction after_instr {
    let instrs, term, close = $2 in
    `Some ($1, instrs, term, close)
  }

local_decl:
  lambda_decl { LocalLam   $1 }
| const_decl  { LocalConst $1 }
| var_decl    { LocalVar   $1 }

const_decl:
  Const var COLON type_expr EQUAL expr option(SEMI) {
    let stop =
      match $7 with
        None -> expr_to_region $6
      | Some region -> region in
    let region = cover $1 stop in
    let value  = {
      kwd_const  = $1;
      name       = $2;
      colon      = $3;
      vtype      = $4;
      equal      = $5;
      init       = $6;
      terminator = $7}
    in {region; value}
  }

var_decl:
  Var var COLON type_expr ASS expr option(SEMI) {
    let stop =
      match $7 with
               None -> expr_to_region $6
      | Some region -> region in
    let region = cover $1 stop in
    let value = {
      kwd_var    = $1;
      name       = $2;
      colon      = $3;
      vtype      = $4;
      ass        = $5;
      init       = $6;
      terminator = $7}
    in {region; value}
  }

instruction:
  single_instr { Single $1 }
| block        { Block  $1 }

single_instr:
  conditional {     Cond $1 }
| match_instr {    Match $1 }
| ass         {      Ass $1 }
| loop        {     Loop $1 }
| proc_call   { ProcCall $1 }
| Null        {     Null $1 }
| Fail expr   { let region = cover $1 (expr_to_region $2)
                in Fail {region; value = $1,$2} }

proc_call:
  fun_call { $1 }

conditional:
  If expr Then instruction Else instruction {
    let region = cover $1 (instr_to_region $6) in
    let value = {
      kwd_if   = $1;
      test     = $2;
      kwd_then = $3;
      ifso     = $4;
      kwd_else = $5;
      ifnot    = $6}
    in {region; value}
  }

match_instr:
  Match expr With option(VBAR) cases End {
    let region = cover $1 $6 in
    let value = {
      kwd_match = $1;
      expr      = $2;
      kwd_with  = $3;
      lead_vbar = $4;
      cases     = $5;
      kwd_end   = $6}
    in {region; value}
  }

cases:
  nsepseq(case,VBAR) {
    let region = nsepseq_to_region (fun x -> x.region) $1
    in {region; value=$1}
  }

case:
  pattern ARROW instruction {
    let region = cover $1.region (instr_to_region $3)
    in {region; value = $1,$2,$3}
  }

ass:
  var ASS expr {
    let region = cover $1.region (expr_to_region $3)
    in {region; value = $1,$2,$3}
  }

loop:
  while_loop { $1 }
| for_loop   { $1 }

while_loop:
  While expr block {
    let region = cover $1 $3.region
    in While {region; value=$1,$2,$3}
  }

for_loop:
  For ass Down? To expr option(step_clause) block {
    let region = cover $1 $7.region in
    let value =
      {
        kwd_for  = $1;
        ass      = $2;
        down     = $3;
        kwd_to   = $4;
        bound    = $5;
        step     = $6;
        block    = $7;
      }
    in For (ForInt {region; value})
  }

| For var option(arrow_clause) In expr block {
    let region = cover $1 $6.region in
    let value =
      {
        kwd_for = $1;
        var     = $2;
        bind_to = $3;
        kwd_in  = $4;
        expr    = $5;
        block   = $6;
      }
    in For (ForCollect {region; value})
  }

step_clause:
  Step expr { $1,$2 }

arrow_clause:
  ARROW var { $1,$2 }

(* Expressions *)

expr:
  expr OR conj_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Or {region; value = $1,$2,$3}
  }
| conj_expr { $1 }

conj_expr:
  conj_expr AND comp_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    And {region; value = $1,$2,$3}
  }
| comp_expr { $1 }

comp_expr:
  comp_expr LT cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Lt {region; value = $1,$2,$3}
  }
| comp_expr LEQ cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Leq {region; value = $1,$2,$3}
  }
| comp_expr GT cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Gt {region; value = $1,$2,$3}
  }
| comp_expr GEQ cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Geq {region; value = $1,$2,$3}
  }
| comp_expr EQUAL cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Equal {region; value = $1,$2,$3}
  }
| comp_expr NEQ cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Neq {region; value = $1,$2,$3}
  }
| cat_expr { $1 }

cat_expr:
  cons_expr CAT cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Cat {region; value = $1,$2,$3}
  }
| cons_expr { $1 }

cons_expr:
  add_expr CONS cons_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Cons {region; value = $1,$2,$3}
  }
| add_expr { $1 }

add_expr:
  add_expr PLUS mult_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Add {region; value = $1,$2,$3}
  }
| add_expr MINUS mult_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Sub {region; value = $1,$2,$3}
  }
| mult_expr { $1 }

mult_expr:
  mult_expr TIMES unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Mult {region; value = $1,$2,$3}
  }
| mult_expr SLASH unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Div {region; value = $1,$2,$3}
  }
| mult_expr Mod unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    Mod {region; value = $1,$2,$3}
  }
| unary_expr { $1 }

unary_expr:
  MINUS core_expr {
    let stop   = expr_to_region $2 in
    let region = cover $1 stop in
    Neg {region; value = $1,$2}
  }
| Not core_expr {
    let stop   = expr_to_region $2 in
    let region = cover $1 stop in
    Not {region; value = $1,$2}
  }
| core_expr { $1 }

core_expr:
  Int        {       Int $1 }
| var        {       Var $1 }
| String     {    String $1 }
| Bytes      {     Bytes $1 }
| C_False    {     False $1 }
| C_True     {      True $1 }
| C_Unit     {      Unit $1 }
| tuple      {     Tuple $1 }
| list_expr  {      List $1 }
| empty_list { EmptyList $1 }
| set_expr   {       Set $1 }
| empty_set  {  EmptySet $1 }
| none_expr  {  NoneExpr $1 }
| fun_call   {   FunCall $1 }
| Constr arguments {
    let region = cover $1.region $2.region in
    ConstrApp {region; value = $1,$2}
  }
| C_Some arguments {
    let region = cover $1 $2.region in
    SomeApp {region; value = $1,$2}
  }
| map_name DOT brackets(expr) {
    let region = cover $1.region $3.region in
    let value =
      {
        map_name = $1;
        selector = $2;
        index    = $3;
      }
    in MapLookUp {region; value}
  }

fun_call:
  fun_name arguments {
    let region = cover $1.region $2.region
    in {region; value = $1,$2}
  }

tuple:
  par(nsepseq(expr,COMMA)) { $1 }

arguments:
  tuple { $1 }

list_expr:
  brackets(nsepseq(expr,COMMA)) { $1 }

empty_list:
  par(LBRACKET RBRACKET COLON type_expr { $1,$2,$3,$4 }) { $1 }

set_expr:
  braces(nsepseq(expr,COMMA)) { $1 }

empty_set:
  par(LBRACE RBRACE COLON type_expr { $1,$2,$3,$4 }) { $1 }

none_expr:
  par(C_None COLON type_expr { $1,$2,$3 }) { $1 }

(* Patterns *)

pattern:
  nsepseq(core_pattern,CONS) {
    let region = nsepseq_to_region core_pattern_to_region $1
    in {region; value=$1}
  }

core_pattern:
  var        {    PVar $1 }
| WILD       {   PWild $1 }
| Int        {    PInt $1 }
| String     { PString $1 }
| C_Unit     {   PUnit $1 }
| C_False    {  PFalse $1 }
| C_True     {   PTrue $1 }
| C_None     {   PNone $1 }
| list_patt  {   PList $1 }
| tuple_patt {  PTuple $1 }
| C_Some par(core_pattern) {
    let region = cover $1 $2.region
    in PSome {region; value = $1,$2}
  }

list_patt:
  brackets(sepseq(core_pattern,COMMA)) { Sugar $1 }
| par(cons_pattern)                    {   Raw $1 }

cons_pattern:
  core_pattern CONS pattern { $1,$2,$3 }

tuple_patt:
  par(nsepseq(core_pattern,COMMA)) { $1 }
