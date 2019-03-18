%{
(* START HEADER *)

[@@@warning "-42"]

open Region
open AST

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start program interactive_expr
%type <AST.t> program
%type <AST.expr> interactive_expr

%%

(* RULES *)

(* Compound constructs *)

par(X):
  LPAR X RPAR {
    let region = cover $1 $3
    and value  = {
      lpar   = $1;
      inside = $2;
      rpar   = $3}
    in {region; value}
  }

braces(X):
  LBRACE X RBRACE {
    let region = cover $1 $3
    and value = {
      lbrace = $1;
      inside = $2;
      rbrace = $3}
    in {region; value}
  }

brackets(X):
  LBRACKET X RBRACKET {
    let region = cover $1 $3
    and value = {
      lbracket = $1;
      inside   = $2;
      rbracket = $3}
    in {region; value}
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

%inline var         : Ident { $1 }
%inline type_name   : Ident { $1 }
%inline fun_name    : Ident { $1 }
%inline field_name  : Ident { $1 }
%inline record_name : Ident { $1 }
%inline map_name    : Ident { $1 }

(* Main *)

program:
  nseq(declaration) EOF {
    {decl = $1; eof = $2}
  }

declaration:
  type_decl       {    TypeDecl $1 }
| const_decl      {   ConstDecl $1 }
| lambda_decl     {  LambdaDecl $1 }

(* Type declarations *)

type_decl:
  Type type_name Is type_expr option(SEMI) {
    let stop =
      match $5 with
        Some region -> region
      |        None -> type_expr_to_region $4 in
    let region = cover $1 stop in
    let value = {
      kwd_type   = $1;
      name       = $2;
      kwd_is     = $3;
      type_expr  = $4;
      terminator = $5}
    in {region; value}}

type_expr:
  cartesian   { Prod   $1 }
| sum_type    { Sum    $1 }
| record_type { Record $1 }

cartesian:
  nsepseq(core_type,TIMES) {
    let region = nsepseq_to_region type_expr_to_region $1
    in {region; value=$1}
  }

core_type:
  type_name {
    TAlias $1
  }
| type_name type_tuple {
    let region = cover $1.region $2.region
    in TypeApp {region; value = $1,$2}
  }
| par(type_expr) {
    ParType $1
  }

type_tuple:
  par(nsepseq(type_expr,COMMA)) { $1 }

sum_type:
  nsepseq(variant,VBAR) {
    let region = nsepseq_to_region (fun x -> x.region) $1
    in {region; value = $1}
  }

variant:
  Constr Of cartesian {
    let region = cover $1.region $3.region
    and value = {constr = $1; kwd_of = $2; product = $3}
    in {region; value}
  }

record_type:
  Record
    nsepseq(field_decl,SEMI)
  End
  {
   let region = cover $1 $3
   and value  = {kwd_record = $1; fields = $2; kwd_end = $3}
   in {region; value}
  }

field_decl:
  field_name COLON type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {field_name = $1; colon = $2; field_type = $3}
    in {region; value}
  }

(* Function and procedure declarations *)

lambda_decl:
  fun_decl   { FunDecl   $1 }
| proc_decl  { ProcDecl  $1 }
| entry_decl { EntryDecl $1 }

fun_decl:
  Function fun_name parameters COLON type_expr Is
    seq(local_decl)
    block
  With expr option(SEMI) {
    let stop =
      match $11 with
        Some region -> region
      |        None -> expr_to_region $10 in
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

entry_decl:
  Entrypoint fun_name entry_params COLON type_expr Is
    seq(local_decl)
    block
  With expr option(SEMI) {
    let stop =
      match $11 with
        Some region -> region
      |        None -> expr_to_region $10 in
    let region = cover $1 stop in
    let value = {
      kwd_entrypoint = $1;
      name           = $2;
      param          = $3;
      colon          = $4;
      ret_type       = $5;
      kwd_is         = $6;
      local_decls    = $7;
      block          = $8;
      kwd_with       = $9;
      return         = $10;
      terminator     = $11}
    in {region; value}
  }

entry_params:
  par(nsepseq(entry_param_decl,SEMI)) { $1 }

proc_decl:
  Procedure fun_name parameters Is
    seq(local_decl)
    block option(SEMI)
    {
     let stop =
       match $7 with
         Some region -> region
       |        None -> $6.region in
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
    and value  = {
      kwd_var    = $1;
      var        = $2;
      colon      = $3;
      param_type = $4}
    in ParamVar {region; value}
  }
| Const var COLON type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop
    and value  = {
      kwd_const  = $1;
      var        = $2;
      colon      = $3;
      param_type = $4}
    in ParamConst {region; value}
  }

entry_param_decl:
  param_decl {
    match $1 with
      ParamConst const -> EntryConst const
    | ParamVar     var -> EntryVar   var
  }
| Storage var COLON type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop
    and value  = {
      kwd_storage  = $1;
      var          = $2;
      colon        = $3;
      storage_type = $4}
    in EntryStore {region; value}
  }

block:
  Begin
    instruction after_instr
  {
   let instrs, terminator, close = $3 in
   let region = cover $1 close
   and value = {
     opening = $1;
     instr   = $2, instrs;
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
   `End $1
  }
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
        Some region -> region
      |        None -> expr_to_region $6 in
    let region = cover $1 stop in
    let value  = {
      kwd_const  = $1;
      name       = $2;
      colon      = $3;
      const_type = $4;
      equal      = $5;
      init       = $6;
      terminator = $7}
    in {region; value}
  }

var_decl:
  Var var COLON type_expr ASS expr option(SEMI) {
    let stop =
      match $7 with
        Some region -> region
      |        None -> expr_to_region $6 in
    let region = cover $1 stop in
    let value = {
      kwd_var    = $1;
      name       = $2;
      colon      = $3;
      var_type   = $4;
      assign     = $5;
      init       = $6;
      terminator = $7}
    in {region; value}
  }
(*
| Var var COLON type_name type_tuple
  ASS extended_expr option(SEMI) {
    let stop =
      match $7 with
        Some region -> region
      |        None -> expr_to_region $6 in
    let region = cover $1 stop in
    let value = {
      kwd_var    = $1;
      name       = $2;
      colon      = $3;
      var_type   = $4;
      ass        = $5;
      init       = $6;
      terminator = $7}
    in {region; value}
  }

extended_expr:
  expr              { }
| LBRACKET RBRACKET { }
| C_None            { }
 *)
instruction:
  single_instr { Single $1 }
| block        { Block  $1 }

single_instr:
  conditional {      Cond $1 }
| match_instr {     Match $1 }
| assignment  {    Assign $1 }
| loop        {      Loop $1 }
| proc_call   {  ProcCall $1 }
| fail_instr  {      Fail $1 }
| Do Nothing  { let region = cover $1 $2 in DoNothing region }

fail_instr:
  Fail expr {
    let region = cover $1 (expr_to_region $2)
    and value  = {kwd_fail = $1; fail_expr = $2}
    in {region; value}}

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
    in {region; value = $1}
  }

case:
  pattern ARROW instruction {
    let region = cover (pattern_to_region $1) (instr_to_region $3)
    and value  = {pattern = $1; arrow = $2; instr = $3}
    in {region; value}
  }

assignment:
  var ASS expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {var = $1; assign = $2; expr = $3}
    in {region; value}
  }

loop:
  while_loop { $1 }
| for_loop   { $1 }

while_loop:
  While expr block {
    let region = cover $1 $3.region
    and value  = {
      kwd_while = $1;
      cond      = $2;
      block     = $3}
    in While {region; value}
  }

for_loop:
  For assignment Down? To expr option(step_clause) block {
    let region = cover $1 $7.region in
    let value =
      {
        kwd_for  = $1;
        assign   = $2;
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
    let value = {
      kwd_for = $1;
      var     = $2;
      bind_to = $3;
      kwd_in  = $4;
      expr    = $5;
      block   = $6}
    in For (ForCollect {region; value})
  }

step_clause:
  Step expr { $1,$2 }

arrow_clause:
  ARROW var { $1,$2 }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  expr OR conj_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3} in
    LogicExpr (BoolExpr (Or {region; value}))
  }
| conj_expr { $1 }

conj_expr:
  conj_expr AND comp_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3} in
    LogicExpr (BoolExpr (And {region; value}))
  }
| comp_expr { $1 }

comp_expr:
  comp_expr LT cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3} in
    LogicExpr (CompExpr (Lt {region; value}))
  }
| comp_expr LEQ cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in LogicExpr (CompExpr (Leq {region; value}))
  }
| comp_expr GT cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in LogicExpr (CompExpr (Gt {region; value}))
  }
| comp_expr GEQ cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in LogicExpr (CompExpr (Geq {region; value}))
  }
| comp_expr EQUAL cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in LogicExpr (CompExpr (Equal {region; value}))
  }
| comp_expr NEQ cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in LogicExpr (CompExpr (Neq {region; value}))
  }
| cat_expr { $1 }

cat_expr:
  cons_expr CAT cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in StringExpr (Cat {region; value})
  }
| cons_expr { $1 }

cons_expr:
  add_expr CONS cons_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ListExpr (Cons {region; value})
  }
| add_expr { $1 }

add_expr:
  add_expr PLUS mult_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ArithExpr (Add {region; value})
  }
| add_expr MINUS mult_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ArithExpr (Sub {region; value})
  }
| mult_expr { $1 }

mult_expr:
  mult_expr TIMES unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ArithExpr (Mult {region; value})
  }
| mult_expr SLASH unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ArithExpr (Div {region; value})
  }
| mult_expr Mod unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ArithExpr (Mod {region; value})
  }
| unary_expr { $1 }

unary_expr:
  MINUS core_expr {
    let stop   = expr_to_region $2 in
    let region = cover $1 stop
    and value  = {op = $1; arg = $2}
    in ArithExpr (Neg {region; value})
  }
| Not core_expr {
    let stop   = expr_to_region $2 in
    let region = cover $1 stop
    and value  = {op = $1; arg = $2} in
    LogicExpr (BoolExpr (Not {region; value}))
  }
| core_expr { $1 }

core_expr:
  Int              { ArithExpr (Int $1) }
| var              { Var $1 }
| String           { StringExpr (String $1) }
| Bytes            { Bytes $1 }
| C_False          { LogicExpr (BoolExpr (False $1)) }
| C_True           { LogicExpr (BoolExpr (True $1)) }
| C_Unit           { Unit $1 }
| tuple            { Tuple $1 }
| list_expr        { ListExpr (List $1) }
| empty_list       { ListExpr (EmptyList $1) }
| set_expr         { SetExpr (Set $1) }
| empty_set        { SetExpr (EmptySet $1) }
| none_expr        { ConstrExpr (NoneExpr $1) }
| fun_call         { FunCall $1 }
| map_selection    { MapLookUp $1 }
| record_expr      { RecordExpr $1 }
| Constr arguments {
    let region = cover $1.region $2.region in
    ConstrExpr (ConstrApp {region; value = $1,$2})
  }
| C_Some arguments {
    let region = cover $1 $2.region in
    ConstrExpr (SomeApp {region; value = $1,$2})
  }

map_selection:
  map_name brackets(expr) {
    let region = cover $1.region $2.region in
    let value  = {
      map_path = Map $1;
      index    = $2}
    in {region; value}
  }
| record_projection brackets(expr) {
    let region = cover $1.region $2.region in
    let value  = {
      map_path = MapPath $1;
      index    = $2}
    in {region; value}
 }

record_expr:
  record_injection  { RecordInj  $1 }
| record_projection { RecordProj $1 }
| record_copy       { RecordCopy $1 }

record_injection:
  Record
    field_assignment after_field
  {
   let fields, terminator, close = $3 in
   let region = cover $1 close
   and value = {
     opening = $1;
     fields  = $2, fields;
     terminator;
     close}
   in {region; value}
  }

after_field:
  SEMI field_or_end {
    match $2 with
      `Some (field, fields, term, close) ->
        ($1, field)::fields, term, close
    | `End close ->
        [], Some $1, close
  }
| End {
   [], None, $1
  }

field_or_end:
  End {
   `End $1
  }
| field_assignment after_field {
    let fields, term, close = $2 in
   `Some ($1, fields, term, close)
  }

field_assignment:
  field_name EQUAL expr {
    let region = cover $1.region (expr_to_region $3)
    and value = {
      field_name = $1;
      equal      = $2;
      field_expr = $3}
    in {region; value}
  }

record_projection:
  record_name DOT nsepseq(field_name,DOT) {
    let stop   = nsepseq_to_region (fun x -> x.region) $3 in
    let region = cover $1.region stop
    and value  = {
      record_name = $1;
      selector    = $2;
      field_path  = $3}
    in {region; value}
  }

record_copy:
  Copy record_name With record_injection {
    let region = cover $1 $4.region in
    let value  = {
      kwd_copy    = $1;
      record_name = $2;
      kwd_with    = $3;
      delta       = $4}
    in {region; value}
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
  par(typed_empty_list) { $1 }

typed_empty_list:
  LBRACKET RBRACKET COLON type_expr {
    {lbracket  = $1;
     rbracket  = $2;
     colon     = $3;
     list_type = $4}
  }

set_expr:
  braces(nsepseq(expr,COMMA)) { $1 }

empty_set:
  par(typed_empty_set) { $1 }

typed_empty_set:
  LBRACE RBRACE COLON type_expr {
    {lbrace   = $1;
     rbrace   = $2;
     colon    = $3;
     set_type = $4}
  }

none_expr:
  par(typed_none_expr) { $1 }

typed_none_expr:
  C_None COLON type_expr {
    {c_None   = $1;
     colon    = $2;
     opt_type = $3}
  }

(* Patterns *)

pattern:
  nsepseq(core_pattern,CONS) {
    let region = nsepseq_to_region pattern_to_region $1
    in PCons {region; value=$1}
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
