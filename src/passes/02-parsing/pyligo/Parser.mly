(* Menhir specification of the parser of PyLIGO *)

%{
(* START HEADER *)

[@@@warning "-42"]

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module CST = Cst_pyligo.CST
open! CST
module Wrap = Lexing_shared.Wrap

(* UTILITIES *)

let (<@) f g x = f (g x)

(* Computing stop region of optional node *)

let mk_stop to_region prev_region = function
  None      -> prev_region
| Some node -> to_region node

let mk_reg region value = Region.{region; value}

(* END HEADER *)
%}

(* Entry points *)

%type <CST.t> contract
%type <CST.expr> interactive_expr

%start contract interactive_expr

(* Reductions on error *)

%on_error_reduce member_name
%on_error_reduce class_type
%on_error_reduce core_type
%on_error_reduce cartesian_level
%on_error_reduce fun_type_level
%on_error_reduce nsepseq(variant,VBAR)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce return_stmt
%on_error_reduce nsepseq(simple_stmt,SEMI)
%on_error_reduce qualified_member
%on_error_reduce core_expr
%on_error_reduce shift_expr_level
%on_error_reduce power_expr_level
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce add_expr_level
%on_error_reduce bin_op(comp_expr_level,NE,add_expr_level)
%on_error_reduce conj_expr_level
%on_error_reduce disj_expr_level
%on_error_reduce expr
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce comp_expr_level
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce bin_op(comp_expr_level,LT,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,LE,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,GT,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,GE,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,EQ2,add_expr_level)
%on_error_reduce nseq(brackets(expr))
%on_error_reduce assert_stmt
%on_error_reduce left_expr
%on_error_reduce nsepseq(left_expr,COMMA)
%on_error_reduce nsepseq(__anonymous_4,COMMA)

%%

(* See [ParToken.mly] for the definition of the tokens. *)

(* Menhir attributes used for error recovery *)

(* RULES *)

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The non-empty
   sequence is parsed by [nseq], which returns a pair made of the
   first parsed item (the parameter [X]) and the rest of the sequence
   (possibly empty). This way, the OCaml typechecker can keep track of
   this information along the static control-flow graph. The rule
   [nsepseq] is for non-empty such sequences. See module [Utils] for
   the types corresponding to the semantic actions of those rules. *)

(* Possibly empty sequence of items *)

seq(item):
  ioption(item seq(item) { $1::$2 }) { Option.value ~default:[] $1 }

(* Non-empty sequence of items *)

nseq(X):
  X         { $1, [] }
| X nseq(X) { let hd,tl = $2 in $1, hd::tl }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. The follwing rules were inspired by the
   following blog post by Pottier:

   http://gallium.inria.fr/blog/lr-lists/
*)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1#region $3#region
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1#region $3#region
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
    in {region; value} }

braces(X):
  "{" X "}" {
    let region = cover $1#region $3#region
    and value  = {lbrace=$1; inside=$2; rbrace=$3}
    in {region; value} }

tuple(item):
  par(item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }) { $1 }

(* Aliasing and inlining some tokens *)

%inline
variable    : "<ident>"  { $1 }
fun_name    : "<ident>"  { $1 }
member_name : "<ident>"  { $1 }
module_name : "<ident>"  { $1 }
type_name   : "<ident>"  { $1 }

class_name  : "<uident>" { $1 }

(* Unary operators *)

unary_op(op,arg):
  op arg {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in {region; value} }

(* Binary operators *)

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

(* ENTRY POINTS *)

interactive_expr: expr EOF { $1 }

contract:
  nseq(top_declaration) EOF { {decl=$1; eof=$2} }

(* DECLARATIONS (top-level) *)

top_declaration:
  "<directive>"         { D_Directive $1 }
| TOP_LEVEL declaration { $2             }

declaration:
  attr_decl  { D_Attr  $1 }
| type_decl  { D_Type  $1 }
| const_decl { D_Const $1 }
| fun_decl   { D_Fun   $1 }
| class_decl { D_Class $1 }

(* Attributed declaration *)

attr_decl:
  "[@attr]" TOP_LEVEL? declaration { $1,$3 }

(* Type declarations *)

type_decl:
  class_name ioption(class_type_superclasses) "=" decl_rhs(type_expr) {
    let stop   = type_expr_to_region $4 in
    let region = cover $1#region stop
    and value  = {class_name=$1; superclasses=$2; equal=$3; class_type=$4}
    in ClassEq {region; value}
  }
| "type" type_name "=" decl_rhs(type_expr) {
    let region = cover $1#region (type_expr_to_region $4)
    and value  = {kwd_type=$1; alias=$2; equal=$3; type_expr=$4}
    in TypeAlias {region; value} }

%inline
class_type_superclasses:
  par(nsepseq(class_type_superclass,",")) { $1 }

class_type_superclass:
  superclass { $1 }

decl_rhs(kind):
  BEGIN INDENT kind END { $3 }
| kind                  { $1 }

(* Constant declarations *)

const_decl:
  "const" ext_var type_annotation? "=" decl_rhs(expr) {
    let region = cover $1#region (expr_to_region $5)
    and value  = {kwd_const=$1; variable=$2; const_type=$3;
                  equal=$4; init=$5}
    in {region; value} }

ext_var:
  variable | "_" { $1 }

%inline
type_annotation:
  ":" type_expr { $1,$2 }

(* Function declarations *)

fun_decl:
  "def" fun_name ioption(fun_class_params) fun_params
   ioption(return_type) ":" block {
     let region = cover $1#region $7.region
     and value  = {kwd_def=$1; fun_name=$2; class_params=$3;
                   fun_params=$4; return_type=$5; colon=$6; block=$7}
     in {region; value} }

fun_class_params:
  brackets(nsepseq(fun_class_name,",")) { $1 }

fun_class_name:
  class_name { $1 }

return_type:
  "->" type_expr { $1,$2 }

fun_params:
  par(ioption(nsepseq(param_decl,","))) { $1 }

param_decl:
  var_param_decl   { $1 }
| const_param_decl { $1 }

var_param_decl:
  ext_var ioption(type_annotation) {
    let stop   = mk_stop (type_expr_to_region <@ snd) $1#region $2 in
    let region = cover $1#region stop in
    let value  = {kwd_const=None; parameter=$1; param_type=$2}
    in {region; value} }

const_param_decl:
  "const" var_param_decl {
    let parameter, param_type = $2.value.parameter, $2.value.param_type in
    let region = cover $1#region $2.region
    and value  = {kwd_const = Some $1; parameter; param_type}
    in {region; value} }

block:
  INDENT simple_stmts  { $2 }
| BEGIN statements END {
    let region = cover $1#region $3#region
    and value  = Statements {virt_begin=$1#region; statements=$2;
                             virt_end=$3#region}
    in {region; value} }

statements:
  nseq(statement) {
    let region = nseq_to_region (local_stmt_to_region <@ snd) $1
    in mk_reg region $1 }

statement:
  INDENT attributes local_stmt { $2, $3 }

%inline
attributes:
  seq("[@attr]" newline { $1 }) { $1 }

%inline
newline:
  TOP_LEVEL | INDENT { () }

local_stmt:
  simple_stmt   { SimpleStmt   $1 }
| compound_stmt { CompoundStmt $1 }

(* Simple statements *)

simple_stmts:
  nsepseq(simple_stmt,";") {
    let region = nsepseq_to_region simple_stmt_to_region $1
    and value  = SimpleStmts $1
    in {region; value} }

simple_stmt:
  assignment   { S_Assign      $1 }
| return_stmt  { S_Return      $1 }
| module_alias { S_ModuleAlias $1 }
| assert_stmt  { S_Assert      $1 }
| proc_call    { S_ProcCall    $1 }
| "pass"       { S_Pass        $1 }

(* Assignment statement *)

assignment:
  nsepseq(left_expr,",") assign_sym assign_rhs {
    let start  = nsepseq_to_region expr_to_region $1
    and stop   = nsepseq_to_region expr_to_region $3 in
    let region = cover start stop
    and value  = {lhs_assign=$1; assign_sym=$2; rhs_assign=$3}
    in {region; value} }

left_expr:
  map_lookup       { E_MapLookup $1 }
| qualified_member { $1             }

assign_rhs:
  nsepseq(INDENT? expr { $2 },",")           { $1 }
| BEGIN nsepseq(INDENT? expr { $2 },",") END { $2 }

assign_sym:
  "="   { Equal     $1 }
| "+="  { PlusEq    $1 }
| "-="  { MinusEq   $1 }
| "*="  { MultEq    $1 }
| "%="  { ModEq     $1 }
| "//=" { IntDivEq  $1 }
| "&="  { AndEq     $1 }
| "|="  { OrEq      $1 }
| "^="  { XorEq     $1 }
| "<<=" { LShiftEq  $1 }
| ">>=" { RShiftEq  $1 }
| "**=" { ExpEq     $1 }

(* Map lookups *)

map_lookup:
  qualified_expr nseq(brackets(expr)) {
    let start  = expr_to_region $1
    and stop   = nseq_to_region (fun x -> x.region) $2 in
    let region = cover start stop
    and value  = {map=$1; keys=$2}
    in {region; value} }

(* Return statements *)

return_stmt:
  "return" ioption(expr) {
    let stop   = mk_stop expr_to_region $1#region $2 in
    let region = cover $1#region stop
    and value  = {kwd_return=$1; returned=$2}
    in {region; value} }

(* Module aliases *)

module_alias:
  "module" module_name "=" decl_rhs(module_name) {
    let region = cover $1#region $4#region
    and value  = {kwd_module=$1; name=$2; equal=$3; module_expr=$4}
    in {region; value} }

(* Assert statements *)

assert_stmt:
  "assert" expr ioption("," expr { $1,$2 }) {
    let stop   = mk_stop (expr_to_region <@ snd) (expr_to_region $2) $3 in
    let region = cover $1#region stop
    and value  = {kwd_assert=$1; condition=$2; fatal_err=$3}
    in {region; value} }

(* Procedure call *)

proc_call:
  call_expr { $1 } (* Return type is [unit] *)

(* Compound statements *)

compound_stmt:
  type_decl  { C_Type  $1 }
| const_decl { C_Const $1 }
| fun_decl   { C_Fun   $1 }
| var_decl   { C_Var   $1 } (* Not at the top-level *)
| class_decl { C_Class $1 }
| if_stmt    { C_If    $1 }
| for_stmt   { C_For   $1 }
| while_stmt { C_While $1 }
| match_stmt { C_Match $1 }

(* Mutable variable declaration *)

var_decl:
  "var" ext_var type_annotation? "=" decl_rhs(expr) {
    let stop   = expr_to_region $5 in
    let region = cover $1#region stop
    and value  = {kwd_var = Some $1; variable=$2;
                  var_type=$3; equal=$4; init=$5}
    in {region; value}
  }
| ext_var type_annotation "=" decl_rhs(expr) {
    let stop   = expr_to_region $4 in
    let region = cover $1#region stop
    and value  = {kwd_var=None; variable=$1; var_type = Some $2;
                  equal=$3; init=$4}
    in {region; value} }

(* Class declarations *)

class_decl:
  "class" class_name ioption(superclasses) ":" class_body {
    let stop   = CST.nseq_to_region (member_decl_to_region <@ snd) $5 in
    let region = cover $1#region stop
    and value  = {kwd_class=$1; name=$2; superclasses=$3;
                  colon=$4; class_body=$5}
    in {region; value} }

superclasses:
  par(nsepseq(superclass,",")) { $1 }

superclass: (* e.g. "Generic[X,Y]" *)
  class_name ioption(class_params) {
    let stop   = mk_stop (fun x -> x.region) $1#region $2 in
    let region = cover $1#region stop
    and value  = {class_name=$1; class_params=$2}
    in {region; value} }

class_params:
  brackets(nsepseq(class_name,",")) { $1 }

class_body:
  BEGIN member_decls END { $2 }

member_decls:
  nseq(INDENT attributes member_decl { $2,$3 }) { $1 }

member_decl: (* Must be ascribed a type w.r.t. top-level equivalent *)
  member_fun_decl  { MemberFun  $1 }
| member_vars_decl { MemberVars $1 }

member_fun_decl:
  "def" fun_name member_fun_params return_type? ":" block {
    let region = cover $1#region $6.region in
    let value  = {kwd_def=$1; fun_name=$2; member_fun_params=$3;
                  return_type=$4; colon=$5; block=$6}
    in {region; value} }

member_fun_params:
  par(ioption(nsepseq(member_vars_decl,","))) { $1 }

member_vars_decl:
  "const"? nsepseq(variable,",") type_annotation {
    let start =
      match $1 with
        None -> nsepseq_to_region (fun x -> x#region) $2
      | Some kwd_const -> kwd_const#region in
    let stop   = type_expr_to_region (snd $3) in
    let region = cover start stop
    and value  = {kind=$1; vars=$2; vars_type=$3}
    in {region; value} }

(* Conditional statements *)

if_stmt:
  "if" expr ":" block elif_stmt {
    let region = cover $1#region $5.region
    and value  = {kwd_if=$1; test=$2; colon=$3; if_so=$4;
                  if_not = Some (Elif $5)}
    in {region; value}
  }
| "if" expr ":" block ioption(else_block) {
    let if_not, stop =
      match $5 with
        None -> None, $4.region
      | Some e_block -> Some (Else e_block), e_block.region in
    let region = cover $1#region stop
    and value  = {kwd_if=$1; test=$2; colon=$3; if_so=$4; if_not}
    in {region; value} }

elif_stmt: (* INDENT *)
  "elif" expr ":" block elif_stmt {
    let region = cover $1#region $5.region in
    let value  = {kwd_elif=$1; test=$2; colon=$3; if_so=$4;
                  if_not = Some (Elif $5)}
    in {region; value}
  }
| "elif" expr ":" block ioption(else_block) {
    let if_not, stop =
      match $5 with
        None -> None, $4.region
      | Some e_block -> Some (Else e_block), e_block.region in
    let region = cover $1#region stop
    and value  = {kwd_elif=$1; test=$2; colon=$3; if_so=$4; if_not}
    in {region; value} }

else_block:
  "else" ":" block {
    let region = cover $1#region $3.region
    and value  = {kwd_else=$1; colon=$2; block=$3}
    in {region; value} }

(* Bounded Iterations (a.k.a. "for" loops) *)

for_stmt:
  "for" indices "in" expr ":" block {
    let region = cover $1#region $6.region in
    let value  = {kwd_for=$1; indices=$2; kwd_in=$3;
                  expr=$4; colon=$5; block=$6}
    in {region; value} }

indices:
  nsepseq(index,",") { $1 }

index:
  variable { $1 }

(* Unbounded Iterations (a.k.a. "while" loops *)

while_stmt:
  "while" expr ":" block {
    let region = cover $1#region $4.region
    and value  = {kwd_while=$1; cond=$2; colon=$3; block=$4}
    in {region; value} }

(* Pattern matching statements *)

match_stmt:
  "match" exprs ":" BEGIN nseq(case_stmt) END {
    let region = cover $1#region $6#region in
    let value : match_stmt = {kwd_match=$1; subject=$2; colon=$3; cases=$5}
    in {region; value} }

exprs:
  nsepseq(expr,",") { $1 }

case_stmt:
  INDENT "case" pattern ":" block {
    {kwd_case=$2; pattern=$3; colon=$4; block=$5} }

(* PATTERNS *)

pattern:
  attr_pattern       { P_Attr    $1 }
| tuple(pattern)     { P_Tuple   $1 }
| ctor_call(pattern) { P_Ctor    $1 }
| variant_pattern    { P_Variant $1 }
| par(pattern)       { P_Par     $1 }
| par(typed_pattern) { P_Par     $1 }
| literal_pattern    {           $1 }

(* Variant pattern *)

variant_pattern:
  "`" ZWSP ctor_call(pattern) { $3 }

(* Literal pattern *)

literal_pattern:
  "<int>"      { P_Int      $1 }
| "<nat>"      { P_Nat      $1 }
| "<bytes>"    { P_Bytes    $1 }
| "<string>"   { P_String   $1 }
| "<verbatim>" { P_Verbatim $1 }
| "<mutez>"    { P_Mutez    $1 }
| class_name   (* None, False, True or custom *)
| "_"
| variable     { P_Var      $1 }

(* Typed patterns *)

typed_pattern:
  pattern type_annotation {
    let start  = pattern_to_region $1
    and stop   = type_expr_to_region (snd $2) in
    let region = cover start stop
    and value  = {pattern=$1; type_annot=$2}
    in P_Typed {region; value} }

(* Attributed patterns *)

attr_pattern:
  "[@attr]" pattern { $1,$2 }

(* Class constructor call pattern *)

ctor_call(category):
  class_type par(nsepseq(member(category),",")?) {
    let CST.{class_path; class_args} = $1.Region.value in
    let region = cover $1.Region.region $2.region
    and value  = {class_path; class_args; class_body=$2}
    in {region; value} }

member(rhs):
  member_name "=" rhs {
    Named {member_lhs=$1; equal=$2; member_rhs=$3}
  }
| rhs { Value $1 } (* No punning *)

(*%inline*)
qualified_class:
  class_name {
    ClassName $1
  }
| variable "." class_name { (* variable is module_name *)
    let region = cover $1#region $3#region in
    let value  = {module_path=$1; selector=$2; field=$3}
    in ClassPath {region; value} }

(* EXPRESSIONS *)

(* Paths *)

qualified_expr:
  par(expr) "." nsepseq(selection,".") {
    let object_or_tuple = E_Par $1 in
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {object_or_tuple; selector=$2; path=$3}
    in E_Proj {region; value}
  }
| qualified_member { $1 }

qualified_member:
  variable "." nsepseq(selection,".") {
    let object_or_tuple = E_Var $1 in
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1#region stop
    and value  = {object_or_tuple; selector=$2; path=$3}
    in E_Proj {region; value}
  }
| member_name { E_Var $1 }

%inline
selection:
  member_name { MemberName $1 } (* Can be a component, e.g. "@1" *)
| "<int>"     { Component  $1 }

(* Expressions *)

expr:
  match_expr      { E_Match  $1 }
| if_else_expr    { E_IfElse $1 }
| fun_expr        { E_Fun    $1 }
| disj_expr_level {          $1 }

(* Pattern matching expressions *)

match_expr:
  "match" exprs ":" BEGIN nseq(case_expr) END {
    let region = cover $1#region $6#region in
    let value : match_expr = {kwd_match=$1; subject=$2; colon=$3; cases=$5}
    in {region; value} }

case_expr:
  INDENT "case" pattern ":" decl_rhs(expr) {
    {kwd_case=$2; pattern=$3; colon=$4; expr=$5} }

(* Conditional expression *)

if_else_expr:
  disj_expr_level "if" disj_expr_level "else" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $5 in
    let region = cover start stop
    and value  = {if_so=$1; kwd_if=$2; subject=$3; kwd_else=$4; if_not=$5}
    in {region; value} }

(* Lambdas (functional expressions) *)

fun_expr:
  "lambda" lambda_params? ":" expr {
    let stop   = expr_to_region $4 in
    let region = cover $1#region stop
    and value  = {kwd_lambda=$1; lambda_params=$2; colon=$3; lambda_body=$4}
    in {region; value} }

lambda_params:
  nsepseq(lambda_param,",") { $1 }

lambda_param:
  par(param_decl) { LambdaTypedParam $1 }
| variable        { LambdaParam      $1 }

(* Stratification *)

(* Disjunctive and conjunctive logical expressions *)

disj_expr_level:
  bin_op(conj_expr_level,"or",disj_expr_level) { E_Or $1 }
| conj_expr_level                              {      $1 }

conj_expr_level:
  bin_op(comp_expr_level,"and",conj_expr_level) { E_And $1 }
| comp_expr_level                               {       $1 }

(* Comparisons *)

comp_expr_level:
  bin_op(comp_expr_level, "<",  add_expr_level) { E_Lt    $1 }
| bin_op(comp_expr_level, "<=", add_expr_level) { E_Leq   $1 }
| bin_op(comp_expr_level, ">",  add_expr_level) { E_Gt    $1 }
| bin_op(comp_expr_level, ">=", add_expr_level) { E_Geq   $1 }
| bin_op(comp_expr_level, "==", add_expr_level) { E_Equal $1 }
| bin_op(comp_expr_level, "!=", add_expr_level) { E_Neq   $1 }
| add_expr_level                                {         $1 }

(* Arithmetic expressions *)

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level) { E_Add $1 }
| bin_op(add_expr_level, "-", mult_expr_level) { E_Sub $1 }
| mult_expr_level                              {       $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*",  power_expr_level) { E_Mult   $1 }
| bin_op(mult_expr_level, "//", power_expr_level) { E_IntDiv $1 }
| bin_op(mult_expr_level, "%",  power_expr_level) { E_Mod    $1 }
| bin_op(mult_expr_level, "|",  power_expr_level) { E_BitOr  $1 }
| bin_op(mult_expr_level, "^",  power_expr_level) { E_BitXor $1 }
| bin_op(mult_expr_level, "&",  power_expr_level) { E_BitAnd $1 }
| power_expr_level                                {          $1 }

power_expr_level:
  bin_op(shift_expr_level, "**", power_expr_level) { E_Exp $1 }
| shift_expr_level                                 {       $1 }

shift_expr_level:
  bin_op(unary_expr_level, "<<", shift_expr_level) { E_BitLShift $1 }
| bin_op(unary_expr_level, ">>", shift_expr_level) { E_BitRShift $1 }
| unary_expr_level                                 {             $1 }

unary_expr_level:
  unary_op("-",   core_expr) { E_Neg    $1 }
| unary_op("not", core_expr) { E_Not    $1 }
| unary_op("~",   core_expr) { E_BitNot $1 }
| core_expr                  {          $1 }

core_expr:
  ctor_call(expr)    { E_Ctor       $1 }
| variant_expr       { E_Variant    $1 }
| code_inj           { E_CodeInj    $1 }
| call_expr          { E_Call       $1 }
| list_expr          { E_List       $1 }
| list_comprehension { E_ListCompr  $1 }
| set_expr           { E_Set        $1 }
| set_comprehension  { E_SetCompr   $1 }
| map_expr           { E_Map        $1 }
| map_comprehension  { E_MapCompr   $1 }
| map_lookup         { E_MapLookup  $1 }
| attr_expr          { E_Attr       $1 }
| tuple(expr)        { E_Tuple      $1 }
| par(expr)          { E_Par        $1 }
| typed_expr         { E_Typed      $1 }
| qualified_expr
| literal_expr       { $1 }

(* Lists defined by comprehension *)

list_comprehension:
  brackets(comprehension_expr) { $1 }

%inline
comprehension_expr:
  expr "for" indices "in" disj_expr_level ioption(predicate) {
    let stop   = mk_stop (fun x -> x.region) (expr_to_region $5) $6
    and start  = expr_to_region $1 in
    let region = cover start stop
    and value  = {subject=$1; kwd_for=$2; indices=$3; kwd_in=$4;
                  iterated=$5; predicate=$6}
    in {region; value} }

predicate:
  "if" expr {
    let region = cover $1#region (expr_to_region $2) in
    let value  = {kwd_if=$1; condition=$2}
    in {region; value} }

(* Sets defined by comprehension *)

set_comprehension:
  braces(comprehension_expr) { $1 }

(* Maps defined by comprehension *)

map_comprehension:
  braces(comprehension_map_expr) { $1 }

comprehension_map_expr:
  binding "for" "(" variable "," variable ")"
  "in" disj_expr_level ioption(predicate) {
    let stop   = mk_stop (fun x -> x.region) (expr_to_region $9) $10 in
    let region = cover $1.region stop
    and value  = {subject=$1; kwd_for=$2;
                  lpar=$3; key=$4; comma=$5; value=$6; rpar=$7;
                  kwd_in=$8; iterated=$9; predicate=$10}
    in {region; value} }

(* Variant expressions (from a sum type) *)

variant_expr:
  "`" ZWSP ctor_call(expr) { $3 }

(* Literal expressions *)

literal_expr:
  "<int>"      { E_Int      $1 }
| "<nat>"      { E_Nat      $1 }
| "<mutez>"    { E_Mutez    $1 }
| "<string>"   { E_String   $1 }
| "<verbatim>" { E_Verbatim $1 }
| "<bytes>"    { E_Bytes    $1 }

(* Set expressions *)

set_expr:
  braces(nsepseq(set_element,",")) { $1 }

set_element:
  expr { $1 }

(* Typed expressions *)

typed_expr:
  par(disj_expr_level type_annotation { $1,$2 }) { $1 }

(* Attributed expression *)

attr_expr:
  "[@attr]" newline core_expr { $1,$3 }

(* Function call expressions *)

call_expr:
  qualified_expr par(arguments?) {
    let region = cover (expr_to_region $1) $2.region
    in mk_reg region ($1,$2) }
| par(expr) par(arguments?) {
    let region = cover $1.region $2.region
    in mk_reg region (E_Par $1, $2) }

arguments:
  nsepseq(argument,",") { $1 }

argument:
  expr { $1 }

(* Code injection *)

code_inj:
  "[%lang" expr "]" {
    let region = cover $1.region $3#region
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

(* Map expression *)

map_expr:
  braces(nsepseq(binding,",")) { $1 }

binding:
  key ":" value {
    let region = cover (expr_to_region $1) (expr_to_region $3)
    and value  = {key=$1; colon=$2; value=$3}
    in {region; value} }

key:
  expr { $1 } (* Dictionary if string. *)

value:
  expr { $1 }

(* List expressions *)

list_expr:
  brackets(nsepseq(list_elt,",")?) { $1 }

list_elt:
  expr { $1 }

(* TYPE EXPRESSIONS *)

type_expr:
  sum_type | fun_type_level { $1 }

(* Sum types *)

sum_type:
  variant "|" nsepseq(variant,"|") {
    let stop   = nsepseq_to_region (fun x -> x.region) $3 in
    let region = cover $1.region stop
    and value  = Utils.nsepseq_cons $1 $2 $3
    in T_Sum {region; value}
  }
| "|" variant {
    let region = cover $1#region $2.region
    in T_Sum {region; value = ($2,[])} }

variant:
  "[@attr]" variant {
    let attributes, class_type = $2.value in
    {$2 with value = $1::attributes, class_type}
  }
| class_type { {region=$1.region; value = [], $1} }

(* Functional type expressions *)

fun_type_level:
  cartesian_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop
    in T_Fun {region; value = ($1, $2, $3)}
  }
| cartesian_level { $1 }

(* Cartesian products *)

cartesian_level:
  core_type "*" nsepseq(core_type,"*") {
    let start  = type_expr_to_region $1
    and stop   = nsepseq_to_region type_expr_to_region $3 in
    let region = cover start stop in
    T_Cart {region; value = $1,$2,$3}
  }
| tuple(core_type) { T_Tuple $1 }
| core_type { $1 }

(* Core types *)

core_type:
  "<string>"     { T_String $1 }
| "<int>"        { T_Int    $1 }
| "_" | variable { T_Var    $1 } (* variable is a type name *)
| class_type     { T_Class  $1 }
| par(type_expr) { T_Par    $1 }
| attr_type      { T_Attr   $1 }

(* Class types *)

class_type:
  qualified_class ioption(type_tuple) {
    let start  = qualified_class_to_region $1 in
    let stop   = mk_stop (fun x -> x.region) start $2 in
    let region = cover start stop
    and value  = {class_path=$1; class_args=$2}
    in {region; value} }

type_tuple:
  brackets(nsepseq(type_expr,",")) { $1 }

(* Attributed types *)

attr_type:
  "[@attr]" core_type { $1,$2 }
